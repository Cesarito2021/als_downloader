#' Read and validate a polygon study area
#'
#' @param x An `sf` or `sfc` polygon, a path to a vector file, or a Shiny
#'   upload object containing `name` and `datapath`.
#' @param layer Layer name for a multi-layer GeoPackage. Required when there
#'   is more than one layer.
#' @return An `sf` polygon object in EPSG:4326. Empty geometries are removed.
#' @details Zipped Shapefiles must contain one Shapefile with a declared CRS.
#'   Archives with unsafe paths or more than 200 MB uncompressed are rejected.
#'   No coordinate reference system is guessed.
#' @export
#' @examples
#' ring <- matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE)
#' aoi <- read_aoi(sf::st_sfc(sf::st_polygon(list(ring)), crs = 4326))
read_aoi <- function(x, layer = NULL) {
  if (inherits(x, "sfc")) x <- sf::st_sf(geometry = x)
  if (!inherits(x, "sf")) {
    name <- if (is.list(x) && !is.null(x$name)) x$name[[1]] else x
    path <- if (is.list(x) && !is.null(x$datapath)) x$datapath[[1]] else x
    if (!is.character(path) || length(path) != 1L || !file.exists(path))
      stop("Study-area file does not exist.", call. = FALSE)
    ext <- tolower(tools::file_ext(name))
    if (ext == "zip") {
      members <- utils::unzip(path, list = TRUE)
      nms <- gsub("\\\\", "/", members$Name)
      if (any(grepl("(^/|^[A-Za-z]:|(^|/)\\.\\.(/|$))", nms)) ||
          sum(members$Length) > 200 * 1024^2)
        stop("Unsafe or oversized study-area ZIP archive.", call. = FALSE)
      folder <- tempfile("als-aoi-")
      dir.create(folder)
      on.exit(unlink(folder, recursive = TRUE), add = TRUE)
      utils::unzip(path, exdir = folder)
      shp <- list.files(folder, "\\.shp$", recursive = TRUE,
                        full.names = TRUE, ignore.case = TRUE)
      if (length(shp) != 1L) stop("ZIP must contain exactly one Shapefile.", call. = FALSE)
      path <- shp
    } else if (!ext %in% c("gpkg", "geojson", "json", "shp", "fgb")) {
      stop("Use a zipped Shapefile, GeoPackage, GeoJSON or FlatGeobuf.", call. = FALSE)
    }
    if (ext == "gpkg") {
      layers <- sf::st_layers(path)$name
      if (is.null(layer) && length(layers) != 1L)
        stop("Select a GeoPackage layer using the layer argument.", call. = FALSE)
      if (is.null(layer)) layer <- layers[[1]]
      x <- sf::st_read(path, layer = layer, quiet = TRUE)
    } else x <- sf::st_read(path, quiet = TRUE)
  }
  if (is.na(sf::st_crs(x))) stop("Study area has no CRS. Assign its known CRS first.", call. = FALSE)
  x <- sf::st_zm(sf::st_transform(x, 4326), drop = TRUE, what = "ZM")
  x <- sf::st_make_valid(x)
  x <- x[!sf::st_is_empty(x), , drop = FALSE]
  if (!nrow(x) || !all(as.character(sf::st_geometry_type(x)) %in% c("POLYGON", "MULTIPOLYGON")))
    stop("Study area must contain nonempty polygons only.", call. = FALSE)
  bb <- sf::st_bbox(x)
  if (any(!is.finite(bb)) || bb[[1]] < -180 || bb[[3]] > 180 || bb[[2]] < -90 || bb[[4]] > 90)
    stop("Study-area coordinates are outside longitude/latitude bounds.", call. = FALSE)
  x
}

#' Calculate study-area size
#' @param aoi Polygon input accepted by [read_aoi()].
#' @return Geodesic union area in square kilometres, without double-counting
#'   overlapping polygons.
#' @export
#' @examples
#' ring <- matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE)
#' aoi_area(sf::st_sfc(sf::st_polygon(list(ring)), crs = 4326))
aoi_area <- function(aoi) {
  g <- sf::st_as_s2(read_aoi(aoi))
  as.numeric(s2::s2_area(s2::s2_union_agg(g))) / 1e6
}

aoi_geometry <- function(aoi) {
  g <- sf::st_as_sfc(s2::s2_union_agg(sf::st_as_s2(read_aoi(aoi))))
  tmp <- tempfile(fileext = ".geojson")
  on.exit(unlink(tmp), add = TRUE)
  sf::st_write(sf::st_sf(geometry = g), tmp, quiet = TRUE)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)$features[[1]]$geometry
}

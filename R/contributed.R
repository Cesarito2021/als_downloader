#' Read a contributed aerial LiDAR tile index
#' @param path Local GeoJSON index, at most 5 MiB and 10,000 polygons.
#' @return An sf tile table accepted by [download_tiles()].
#' @details Reads metadata only. Does not contact asset URLs or approve a source.
#'   Required properties are tile_id, dataset, url, acquired_start, acquired_end,
#'   platform, license_url and citation. Dates may be null when unknown.
#'   Platform must be ALS or UAV-LiDAR. Coordinates must be EPSG:4326.
#'   Assets must be direct HTTPS LAS/LAZ files or Zenodo record ZIP files.
#'   ZIP files are downloaded intact and require local extraction for preview.
#' @export
#' @examples
#' read_tile_index(system.file("extdata", "contribution-template.geojson",
#'                            package = "alsdownloader"))
read_tile_index <- function(path) {
  if (length(path) != 1L || !file.exists(path) || file.size(path) > 5 * 1024^2)
    stop("Use a local GeoJSON index up to 5 MiB.")
  j <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  if (!identical(j$type, "FeatureCollection") || !length(j$features) || length(j$features) > 10000L)
    stop("Index requires 1 to 10,000 GeoJSON features.")
  required <- c("tile_id", "dataset", "url", "acquired_start", "acquired_end", "platform", "license_url", "citation")
  scalar <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x)) && nchar(x) <= 2000
  date <- function(x) is.null(x) || (scalar(x) && grepl("^\\d{4}-\\d{2}-\\d{2}$", x) &&
    !is.na(suppressWarnings(as.Date(x, format = "%Y-%m-%d"))) && format(as.Date(x, format = "%Y-%m-%d"), "%Y-%m-%d") == x)
  for (f in j$features) {
    p <- f$properties
    if (!identical(f$type, "Feature") || !all(required %in% names(p))) stop("Missing required tile properties.")
    if (!all(vapply(p[setdiff(required, c("acquired_start", "acquired_end"))], scalar, logical(1)))) stop("Tile properties must be non-empty text.")
    if (!p$platform %in% c("ALS", "UAV-LiDAR")) stop("Only ALS and UAV-LiDAR are accepted.")
    if (!date(p$acquired_start) || !date(p$acquired_end) ||
        (!is.null(p$acquired_start) && !is.null(p$acquired_end) && p$acquired_start > p$acquired_end)) stop("Invalid acquisition interval.")
    u <- httr::parse_url(p$url)
    if (!identical(u$scheme, "https") || is.null(u$hostname) || !is.null(u$username) || !is.null(u$password) ||
        length(u$query) || !is.null(u$fragment) || !(grepl("\\.(las|laz)$", u$path, ignore.case = TRUE) ||
          (identical(u$hostname,"zenodo.org") && grepl("^records/[0-9]+/files/[^/]+\\.zip$",u$path,ignore.case=TRUE))))
      stop("Use stable HTTPS LAS/LAZ or Zenodo record ZIP URLs without credentials or query tokens.")
    if (!grepl("^https://[^[:space:]]+$", p$license_url)) stop("A public license URL is required.")
    if (!is.null(p$size_bytes) && (!is.numeric(p$size_bytes) || length(p$size_bytes) != 1L ||
        !is.finite(p$size_bytes) || p$size_bytes <= 0)) stop("size_bytes must be positive or null.")
    if (!is.character(f$geometry$type) || length(f$geometry$type) != 1L ||
        !f$geometry$type %in% c("Polygon", "MultiPolygon")) stop("Each tile needs a polygon footprint.")
  }
  g <- sf::st_read(path, quiet = TRUE)
  if (is.na(sf::st_crs(g)) || !isTRUE(sf::st_crs(g) == sf::st_crs(4326)) ||
      any(sf::st_is_empty(g)) || any(!sf::st_is_valid(g))) stop("Use valid non-empty EPSG:4326 polygons.")
  xy <- sf::st_coordinates(g)
  if (any(!is.finite(xy[,1:2])) || any(abs(xy[,1]) > 180) || any(abs(xy[,2]) > 90)) stop("Coordinates exceed longitude/latitude bounds.")
  value <- function(key, missing = NA_character_) vapply(j$features, function(f) {
    x <- f$properties[[key]]; if (is.null(x)) missing else x
  }, missing)
  rows <- sf::st_sf(tile_id = value("tile_id"), provider = "contributed", dataset = value("dataset"),
    filename = utils::URLdecode(basename(value("url"))), url = value("url"), acquired_start = value("acquired_start"),
    acquired_end = value("acquired_end"), size_bytes = value("size_bytes", NA_real_),
    license_url = value("license_url"), citation = value("citation"), geometry = sf::st_geometry(g))
  if (anyDuplicated(paste(rows$dataset, rows$tile_id)) || anyDuplicated(rows$url)) stop("Duplicate tile identifiers or URLs.")
  rows
}

search_contributed <- function(aoi, folder, max_items) {
  if (is.null(folder) || !dir.exists(folder)) stop("Configure a directory of maintainer-approved *.tiles.geojson indexes.")
  paths <- list.files(folder, "\\.tiles\\.geojson$", full.names = TRUE, ignore.case = TRUE)
  if (!length(paths)) stop("No approved *.tiles.geojson indexes found.")
  result <- empty_tiles()
  for (path in paths) {
    tiles <- read_tile_index(path)
    hits <- lengths(sf::st_intersects(tiles, aoi)) > 0
    if (any(hits)) result <- rbind(result, tiles[hits, ])
    if (nrow(result) > max_items) stop("Search exceeds max_items; use a smaller area.")
  }
  if (!nrow(result)) return(empty_tiles())
  result[!duplicated(result$url), ]
}

selection_script <- function(tiles) {
  if (inherits(tiles, "sf")) tiles <- sf::st_drop_geometry(tiles)
  tiles$url <- redact_url(tiles$url)
  c("# Original tiles intersecting the AOI; files are not clipped.",
    "# Review the recorded provider licenses and access conditions before downloading.",
    paste0("tiles <- ", paste(utils::capture.output(dput(tiles)), collapse = "\n")),
    'alsdownloader::download_tiles(tiles, output_dir = "als-data", workers = 1L)',
    '# Rerun in the same directory to reuse successfully verified files.')
}

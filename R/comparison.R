# Campaign identity includes the project and acquisition interval, never the upload year.
campaign_groups <- function(tiles) {
  if (is.null(tiles) || !nrow(tiles)) return(list())
  project <- tiles$dataset
  usgs <- tiles$provider == "usgs3dep"
  project[usgs] <- basename(dirname(dirname(redact_url(tiles$url[usgs]))))
  date <- function(x) ifelse(is.na(x) | !nzchar(x), "unknown", x)
  interval <- paste0(date(tiles$acquired_start), " to ", date(tiles$acquired_end))
  label <- paste(project, paste0("Collection end: ", date(tiles$acquired_end)), interval, sep = " | ")
  split(seq_len(nrow(tiles)), label)
}

comparison_availability <- function(tiles, aoi, a, b, opted_in = FALSE, side_m = 100) {
  unavailable <- function(message) list(ready = FALSE, message = message)
  groups <- campaign_groups(tiles)
  if (is.null(aoi) || length(groups) < 2L)
    return(unavailable("Comparison needs at least two point-cloud campaigns in the same study area. Search the AOI first."))
  if (!isTRUE(opted_in)) return(unavailable("Comparison is optional. Tick the checkbox to choose two clouds."))
  if (is.null(a) || is.null(b) || !a %in% names(groups) || !b %in% names(groups) || identical(a, b))
    return(unavailable("Choose two different point-cloud campaigns from this AOI."))
  tryCatch({
    region <- comparison_region(tiles[groups[[a]], ], tiles[groups[[b]], ], aoi, side_m)
    list(ready = TRUE, message = sprintf("Ready: %s m square window, clipped to shared coverage (%.4f km2). Visualization only.", side_m, aoi_area(region$overlap)))
  }, error = function(e) unavailable(conditionMessage(e)))
}

comparison_overlap <- function(a, b, aoi, max_km2 = 1) {
  if (!inherits(a, "sf") || !inherits(b, "sf")) stop("Both clouds require provider footprints.")
  footprint <- function(x) sf::st_union(sf::st_geometry(read_aoi(x)))
  overlap <- suppressWarnings(sf::st_intersection(footprint(a), footprint(b)))
  overlap <- suppressWarnings(sf::st_intersection(overlap, footprint(aoi)))
  if (!length(overlap) || all(sf::st_is_empty(overlap))) stop("The two clouds have no overlapping area inside the AOI.")
  area <- sum(as.numeric(sf::st_area(overlap))) / 1e6
  if (!is.finite(area) || area <= 0) stop("The two clouds have no overlapping area inside the AOI.")
  if (area > max_km2) stop("The overlapping area exceeds 1 km2. Draw a smaller AOI.")
  sf::st_sf(geometry = overlap)
}

# A small viewing window, not a claim of ecological representativeness.
comparison_region <- function(a, b, aoi, side_m = 100) {
  side_m <- suppressWarnings(as.numeric(side_m))
  if (length(side_m) != 1L || !is.finite(side_m) || side_m < 100 || side_m > 1000)
    stop("Choose a window side between 100 and 1000 metres.")
  overlap <- comparison_overlap(a, b, aoi, max_km2 = Inf)
  bb <- sf::st_bbox(sf::st_transform(overlap, 4326))
  local_crs <- sprintf("+proj=aeqd +lat_0=%.8f +lon_0=%.8f +R=6371010 +units=m", (bb[2]+bb[4])/2, (bb[1]+bb[3])/2)
  local <- sf::st_transform(overlap, local_crs)
  parts <- suppressWarnings(sf::st_cast(sf::st_geometry(local), "POLYGON"))
  largest <- parts[which.max(as.numeric(sf::st_area(parts)))]
  center <- sf::st_coordinates(sf::st_point_on_surface(largest))[1, 1:2]
  half <- side_m / 2
  window <- sf::st_as_sfc(sf::st_bbox(c(xmin=unname(center[1]-half), ymin=unname(center[2]-half),
    xmax=unname(center[1]+half), ymax=unname(center[2]+half)), crs=sf::st_crs(local)))
  crop <- suppressWarnings(sf::st_intersection(sf::st_geometry(local), window))
  crop <- sf::st_transform(sf::st_sf(geometry=crop), sf::st_crs(overlap))
  select <- function(x) x[lengths(sf::st_intersects(x, sf::st_transform(crop, sf::st_crs(x)))) > 0, , drop=FALSE]
  a <- select(a); b <- select(b)
  if (nrow(a) > 4L || nrow(b) > 4L) stop("Choose a smaller window: at most four tiles per campaign can be previewed.")
  list(a=a, b=b, overlap=crop)
}

read_comparison_cloud <- function(path, aoi) {
  header <- lidR::readLASheader(path)
  crs <- sf::st_crs(header)
  if (is.na(crs) || isTRUE(sf::st_is_longlat(crs)) || !isTRUE(crs$units_gdal %in% c("metre", "meter", "metres", "meters", "m")))
    stop("Comparison requires an embedded projected CRS in metres. Reproject unsupported files externally first.")
  n <- header@PHB[["Extended Number of point records"]]
  if (is.null(n) || n == 0) n <- header@PHB[["Number of point records"]]
  if (is.null(n) || !is.finite(n) || n > 50000000) stop("A comparison tile exceeds the 50-million source-point limit.")
  roi <- sf::st_transform(aoi, crs)
  bb <- sf::st_bbox(roi)
  las <- lidR::readLAS(path, select = "xyz", filter = paste("-inside", paste(format(as.numeric(bb), scientific = FALSE, trim = TRUE), collapse = " ")))
  if (is.null(las) || !nrow(las@data)) return(list(points = data.frame(X = numeric(), Y = numeric(), Z = numeric()), crs = crs))
  if (nrow(las@data) > 2000000) stop("More than two million AOI bounding-box points; use a smaller AOI.")
  points <- as.data.frame(las@data)
  points <- points[is.finite(points$X) & is.finite(points$Y) & is.finite(points$Z), c("X", "Y", "Z")]
  if (nrow(points)) {
    inside <- lengths(sf::st_intersects(sf::st_as_sf(points, coords = c("X", "Y"), crs = crs), roi)) > 0
    points <- points[inside, , drop = FALSE]
  }
  list(points = points, crs = crs)
}

compare_campaigns <- function(a, b, aoi, directory) {
  overlap <- comparison_overlap(a, b, aoi)
  overlap_km2 <- aoi_area(overlap)
  a <- sf::st_drop_geometry(a); b <- sf::st_drop_geometry(b)
  if (!nrow(a) || !nrow(b) || nrow(a) > 4L || nrow(b) > 4L)
    stop("Use an AOI intersecting at most four tiles per campaign for this preview.")
  reference <- NULL
  read_epoch <- function(rows, prefix) {
    remaining <- 600 * 1024^2
    point_budget <- 2000000L
    clouds <- lapply(seq_len(nrow(rows)), function(i) {
      writeLines(sprintf("Campaign %s: downloading/reading tile %s of %s", toupper(prefix), i, nrow(rows)), file.path(directory, "progress.txt"))
      cloud <- preview_remote_tile(rows[i, , drop = FALSE], max_bytes = min(300 * 1024^2, remaining),
        path = file.path(directory, paste0(prefix, i, ".laz")),
        reader = function(path) {
          remaining <<- remaining - file.size(path)
          read_comparison_cloud(path, overlap)
        })
      if (is.null(reference)) reference <<- cloud$crs
      else if (!isTRUE(reference == cloud$crs)) stop("Campaign CRS definitions differ. Align horizontal and vertical references externally before comparison.")
      point_budget <<- point_budget - nrow(cloud$points)
      if (point_budget < 0) stop("More than two million retained AOI points in a campaign; reduce the AOI.")
      cloud$points
    })
    points <- do.call(rbind, clouds)
    if (!nrow(points)) stop("One campaign has no laser points inside the AOI.")
    if (nrow(points) > 2000000) stop("More than two million AOI points in a campaign; reduce the AOI.")
    points
  }
  pa <- read_epoch(a, "a"); pb <- read_epoch(b, "b")
  writeLines("Preparing the overlapping point-cloud views...", file.path(directory, "progress.txt"))
  origin <- vapply(rbind(pa, pb), min, numeric(1))
  sample_cloud <- function(p) {
    p <- p[unique(round(seq(1, nrow(p), length.out = min(50000, nrow(p))))), ]
    p[] <- Map(function(x, offset) x - offset, p, origin)
    unname(as.matrix(p))
  }
  list(a = sample_cloud(pa), b = sample_cloud(pb), origin = unname(origin), overlap_km2 = overlap_km2,
    counts = c(nrow(pa), nrow(pb)), crs = reference$wkt,
    method = "Visualization only: both point clouds clipped to the intersection of provider footprints and AOI. Display sampling only; no denoising, differences, metrics or analysis exports. Footprints do not resolve within-tile data gaps.")
}

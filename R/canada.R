# This adapter consumes configured NRCan regional indexes. The official ArcGIS
# tile query was verified on 2026-09-19; the adapter does not fetch it automatically.
# See docs/REGIONAL_VERIFICATION.md for scope, access checks and sample decoding.
search_canelevation <- function(aoi, folder, max_items) {
  if (is.null(folder) || !dir.exists(folder)) stop("Configure a local CanElevation tile-index directory.", call. = FALSE)
  files <- list.files(folder, "\\.(gpkg|shp)$", full.names = TRUE, ignore.case = TRUE)
  if (!length(files)) stop("No CanElevation tile-index files (.gpkg or .shp) found.", call. = FALSE)
  results <- list()
  for (path in files) {
    obj <- tryCatch(sf::st_read(path, quiet = TRUE),
      error = function(e) stop("Could not read tile index: ", basename(path), call. = FALSE))
    if (is.na(sf::st_crs(obj))) stop("Tile index has no embedded CRS; provide a corrected index.", call. = FALSE)
    names(obj) <- tolower(names(obj))
    if (!"url" %in% names(obj)) stop("Tile index lacks a URL field.", call. = FALSE)
    obj <- sf::st_transform(sf::st_make_valid(sf::st_zm(obj, drop = TRUE, what = "ZM")), 4326)
    # Some official tile rings retain crossings after spherical repair. Repair
    # those rings in Canada's Lambert projection before the geographic query.
    invalid <- which(!sf::st_is_valid(obj))
    if(length(invalid)) sf::st_geometry(obj)[invalid] <- sf::st_geometry(
      sf::st_make_valid(sf::st_transform(sf::st_make_valid(sf::st_transform(obj[invalid,],3347)),4326)))
    obj <- obj[lengths(sf::st_intersects(obj, aoi)) > 0, , drop = FALSE]
    if (!nrow(obj)) next
    href <- as.character(obj$url)
    if (any(!startsWith(href, "https://canelevation-lidar-point-clouds.s3.ca-central-1.amazonaws.com/")))
      stop("Unexpected CanElevation asset host/path.", call. = FALSE)
    dataset <- sub("\\.(gpkg|shp)$", "", basename(path), ignore.case = TRUE)
    name <- basename(sub("\\?.*$", "", href))
    rows <- sf::st_sf(tile_id = paste(dataset, name, sep = "/"), provider = "canelevation",
      dataset = dataset, filename = name, url = href, acquired_start = NA_character_,
      acquired_end = NA_character_, size_bytes = NA_real_,
      license_url = "https://open.canada.ca/en/open-government-licence-canada",
      citation = paste("Source: Natural Resources Canada; CanElevation Series LiDAR point clouds.",
        "Contains information licensed under the Open Government Licence - Canada.",
        "https://open.canada.ca/data/en/dataset/7069387e-9986-4297-9f55-0288e9676947"),
      geometry = sf::st_geometry(obj))
    results[[length(results) + 1L]] <- rows
    if (sum(vapply(results, nrow, integer(1))) > max_items)
      stop("Search exceeds max_items; use a smaller AOI.", call. = FALSE)
  }
  if (!length(results)) return(empty_tiles())
  ans <- do.call(rbind, results)
  ans[!duplicated(ans$url), , drop = FALSE]
}

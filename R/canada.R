# This adapter uses the official NRCan spatial service when no local index is
# supplied. Configured regional indexes remain available for offline discovery.
# See docs/REGIONAL_VERIFICATION.md for scope, access checks and sample decoding.
search_canelevation <- function(aoi, folder, max_items) {
  if (is.null(folder)) return(search_canelevation_online(aoi, max_items))
  if (!dir.exists(folder)) stop("Configure a local CanElevation tile-index directory.", call. = FALSE)
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

# Official NRCan spatial tile service. Read only intersecting index records;
# no point cloud is downloaded during discovery.
search_canelevation_online <- function(aoi, max_items) {
  bounds <- sf::st_bbox(sf::st_transform(aoi,4326))
  if(bounds['xmax'] < -142 || bounds['xmin'] > -52 || bounds['ymax'] < 41 || bounds['ymin'] > 84) return(empty_tiles())
  endpoint <- "https://maps-cartes.services.geo.ca/server_serveur/rest/services/NRCan/lidar_point_cloud_canelevation_en/MapServer/1/query"
  query <- list(geometry=paste(as.numeric(bounds),collapse=","),geometryType="esriGeometryEnvelope",
    inSR=4326,spatialRel="esriSpatialRelIntersects",where="1=1",returnIdsOnly="true",f="json")
  ids <- request_json(endpoint,query=query)
  if(!is.null(ids$error) || is.null(ids$objectIds)) {
    if(!is.null(ids$error)) stop("Canada index request failed; search incomplete.")
    if(!is.null(ids$objectIdFieldName)) return(empty_tiles())
    stop("Canada index returned an invalid response; search incomplete.")
  }
  ids <- unlist(ids$objectIds,use.names=FALSE)
  if(!length(ids)) return(empty_tiles())
  if(length(ids)>max_items) stop("Search exceeds max_items; use a smaller AOI.")
  if(any(!is.finite(as.numeric(ids)))) stop("Invalid Canada tile identifiers.")
  results <- list()
  for(batch in split(ids,ceiling(seq_along(ids)/500))) {
    page <- request_json(endpoint,query=list(objectIds=paste(batch,collapse=","),outFields="project,provider,tile_name,url,OBJECTID",
      returnGeometry="true",outSR=4326,f="geojson"))
    if(!identical(page$type,"FeatureCollection") || isTRUE(page$exceededTransferLimit) ||
       isTRUE(page$properties$exceededTransferLimit) || length(page$features)!=length(batch)) stop("Canada index response is incomplete.")
    x <- sf::st_read(jsonlite::toJSON(page,auto_unbox=TRUE,null="null"),quiet=TRUE)
    if(anyDuplicated(x$OBJECTID) || !setequal(as.character(x$OBJECTID),as.character(batch))) stop("Canada index returned mismatched tiles.")
    if(anyNA(x$url) || any(!grepl("^https://canelevation-lidar-point-clouds\\.s3[.-]ca-central-1\\.amazonaws\\.com/pointclouds_nuagespoints/.*\\.(las|laz)$",x$url,ignore.case=TRUE))) stop("Unexpected CanElevation asset host/path.")
    x <- sf::st_transform(sf::st_make_valid(sf::st_transform(sf::st_zm(x),3347)),4326)
    x <- x[lengths(sf::st_intersects(x,aoi))>0,,drop=FALSE]
    if(!nrow(x)) next
    results[[length(results)+1L]] <- sf::st_sf(tile_id=paste(x$project,x$tile_name,sep="/"),provider="canelevation",
      dataset=x$project,filename=basename(x$url),url=x$url,acquired_start=NA_character_,acquired_end=NA_character_,size_bytes=NA_real_,
      license_url="https://open.canada.ca/en/open-government-licence-canada",
      citation=paste("Natural Resources Canada; CanElevation. Producer:",x$provider,
        "Contains information licensed under the Open Government Licence - Canada.",
        "https://open.canada.ca/data/en/dataset/7069387e-9986-4297-9f55-0288e9676947"),geometry=sf::st_geometry(x))
  }
  if(!length(results)) return(empty_tiles())
  do.call(rbind,results)
}

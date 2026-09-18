# Use native geometry and asset links; never construct a synthetic download grid.
native_pages <- function(url, prefix, max_items) {
  features <- list(); seen <- character()
  repeat {
    if (!startsWith(url, prefix) || url %in% seen) stop("Unexpected or repeated index pagination; search incomplete.")
    seen <- c(seen,url)
    page <- request_json(url)
    if (!identical(page$type,"FeatureCollection")) stop("Provider did not return a spatial tile index.")
    features <- c(features,page$features)
    if (length(features)>max_items) stop("Search exceeds max_items; use a smaller area.")
    next_link <- Filter(function(x) identical(x$rel,"next"),page$links)
    if (!length(next_link)) break
    if (!length(page$features) && identical(as.integer(page$numberReturned),0L)) break
    if (length(next_link)!=1L || !length(page$features)) stop("Invalid index pagination; search incomplete.")
    url <- next_link[[1]]$href
  }
  features
}

search_europe <- function(aoi, provider, max_items) {
  bbox <- paste(format(as.numeric(sf::st_bbox(aoi)),scientific=FALSE,trim=TRUE,digits=12),collapse=",")
  if (provider=="ahn6") {
    prefix <- "https://api.ellipsis-drive.com/v3/ogc/features/0820faae-5240-499b-8486-cf406433cf71/"
    url <- paste0(prefix,"collections/6aec07f5-f7eb-4f51-b6f7-aee45e5767bd/items?limit=100&bbox=",bbox)
  } else if (provider=="ignfr") {
    prefix <- "https://api.stac.teledetection.fr/collections/lidarhd/"
    url <- paste0(prefix,"items?limit=100&bbox=",bbox)
  } else {
    prefix <- "https://data.geo.admin.ch/api/stac/v1/collections/ch.swisstopo.swisssurface3d/"
    url <- paste0(prefix,"items?limit=100&bbox=",bbox)
  }
  features <- native_pages(url,prefix,max_items)
  result <- list()
  for (f in features) {
    if (is.null(f$geometry)) stop("Index item has no footprint.")
    g <- sf::st_read(jsonlite::toJSON(list(type="Feature",properties=list(),geometry=f$geometry),auto_unbox=TRUE),quiet=TRUE)
    if (!any(lengths(sf::st_intersects(g,aoi))>0)) next
    acquired <- c(start=NA_character_, end=NA_character_)
    if (provider=="ahn6") {
      href <- f$properties$Puntenwolk
      if (is.null(href) || !grepl("^https://basisdata\\.nl/hwh-ahn/AHN6/.*\\.laz$",href,ignore.case=TRUE))
        stop("AHN tile has no reviewed point-cloud asset; search incomplete.")
      assets <- list(list(href=href))
      license <- "https://creativecommons.org/licenses/by/4.0/"
      citation <- "Actueel Hoogtebestand Nederland (AHN6); original point-cloud tile. https://www.ahn.nl/dataroom"
    } else if (provider=="ignfr") {
      assets <- Filter(function(a) !is.null(a$href) && grepl("\\.copc\\.laz$",a$href,ignore.case=TRUE),f$assets)
      if (!length(assets)) stop("France LiDAR HD item has no supported point-cloud asset; search incomplete.")
      if (any(!vapply(assets,function(a) startsWith(a$href,"https://data.geopf.fr/telechargement/download/"),logical(1))))
        stop("Unexpected France LiDAR HD asset host/path.")
      license <- "https://www.data.gouv.fr/pages/legal/licences/etalab-2.0"
      citation <- paste("Source: IGN LiDAR HD, producer Institut national de l'information geographique et forestiere (IGN).",
        "Indexed through a public STAC catalogue maintained by UMR TETIS / INRAE (api.stac.teledetection.fr),",
        "not IGN's own WFS. Licence Ouverte 2.0. https://geoservices.ign.fr/lidarhd")
      acquired <- stac_acquisition_period(f$properties)
    } else {
      assets <- Filter(function(a) !is.null(a$href) && grepl("\\.(las|laz|las\\.zip)$",a$href,ignore.case=TRUE),f$assets)
      if (!length(assets)) stop("Swiss item has no supported point-cloud asset; search incomplete.")
      if (any(!vapply(assets,function(a) startsWith(a$href,"https://data.geo.admin.ch/ch.swisstopo.swisssurface3d/"),logical(1))))
        stop("Unexpected Swiss asset host/path.")
      license <- "https://www.swisstopo.admin.ch/en/terms-of-use-free-geodata-and-geoservices"
      citation <- "Source: Federal Office of Topography swisstopo; swissSURFACE3D. https://www.swisstopo.admin.ch/en/height-model-swisssurface3d"
    }
    # Catalog timestamps and filename years are not confirmed acquisition dates,
    # except for ignfr, whose STAC properties carry named acquisition fields.
    dataset <- switch(provider, ahn6="AHN6", ignfr="IGN LiDAR HD", "swissSURFACE3D")
    for (asset in assets) result[[length(result)+1L]] <- sf::st_sf(
      tile_id=as.character(f$id),provider=provider,dataset=dataset,
      filename=basename(asset$href),url=asset$href,
      acquired_start=unname(acquired['start']),acquired_end=unname(acquired['end']),
      size_bytes=if(is.null(asset[["file:size"]]))NA_real_ else as.numeric(asset[["file:size"]]),
      license_url=license,citation=citation,geometry=sf::st_geometry(g))
    if (length(result)>max_items) stop("Search exceeds max_items; use a smaller area.")
  }
  if (!length(result)) return(empty_tiles())
  x <- do.call(rbind,result);x[!duplicated(x$url),]
}

valid_tile_container <- function(path,tile) {
  if (identical(tile$provider,"swisstopo") && grepl("\\.las\\.zip$",tile$filename,ignore.case=TRUE)) {
    if (!file.exists(path)) return(FALSE)
    members <- tryCatch(suppressWarnings(utils::unzip(path,list=TRUE)),error=function(e)NULL)
    if (is.null(members) || !nrow(members)) return(FALSE)
    names <- gsub("\\\\","/",members$Name)
    return(!any(grepl("(^/|^[A-Za-z]:|(^|/)\\.\\.(/|$))",names)) &&
      any(grepl("\\.las$",names,ignore.case=TRUE) & members$Length>=227))
  }
  valid_las_header(path)
}

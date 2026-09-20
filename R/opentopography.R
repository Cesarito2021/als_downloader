# The bundled registry is generated from the complete hosted point-cloud
# catalogue and the provider's object inventory. No personal filesystem paths.
ot_snapshot_path <- function(root) {
  marker <- file.path(root,"complete.json")
  if(!file.exists(marker))stop("OpenTopography snapshot is incomplete.")
  manifest <- jsonlite::fromJSON(marker,simplifyVector=FALSE)
  required <- c("opentopography-registry.rds","opentopography-access-audit.csv",
                "opentopography-verification.json","opentopography-update-state.json")
  if(!identical(manifest$catalog,"catalog"))stop("Invalid OpenTopography snapshot layout.")
  for(name in required) {
    path <- file.path(root,"catalog",name); expected <- manifest$files[[name]]
    if(!is.character(expected) || length(expected)!=1L || !file.exists(path) ||
       !identical(digest::digest(file=path,algo="sha256"),expected))
      stop("OpenTopography snapshot checksum mismatch: ",name)
  }
  file.path(root,"catalog","opentopography-registry.rds")
}

ot_export_audit <- function(destination, directory=attr(ot_registry(),"catalog_directory")) {
  source <- file.path(directory,"opentopography-access-audit.csv")
  if(file.exists(source)) {
    if(!file.copy(source,destination,overwrite=TRUE))stop("Could not export access audit.")
  } else {
    input <- gzfile(paste0(source,".gz"),"rb");on.exit(close(input),add=TRUE)
    output <- file(destination,"wb");on.exit(close(output),add=TRUE)
    repeat {
      chunk <- readBin(input,"raw",n=65536L)
      if(!length(chunk))break
      writeBin(chunk,output)
    }
  }
  invisible(destination)
}

ot_registry <- local({
  cached <- NULL; signature <- NULL; attempted <- NULL
  function() {
  snapshot <- Sys.getenv("ALS_OT_CATALOG_SNAPSHOT","")
  if(nzchar(snapshot)) {
    # Snapshots are immutable. Change the directory (and restart the app) to
    # activate a new release; an incomplete/corrupt release cannot replace cache.
    if(!identical(snapshot,attempted)) {
      attempted <<- snapshot
      result <- tryCatch({
        candidate_path <- ot_snapshot_path(snapshot)
        candidate <- readRDS(candidate_path)
        if(!inherits(candidate,"sf") || !all(c("dataset","access_status","index_sha256")%in%names(candidate)))
          stop("Invalid OpenTopography registry schema.")
        if(!inherits(sf::st_geometry(candidate),"sfc") || is.na(sf::st_crs(candidate)))
          stop("Invalid OpenTopography registry geometry.")
        attr(candidate,"catalog_directory") <- dirname(candidate_path)
        candidate
      },error=function(e){warning(conditionMessage(e)," Keeping the last valid catalogue.",call.=FALSE);NULL})
      if(!is.null(result)){cached <<- result;signature <<- paste0("snapshot:",snapshot)}
    }
    if(!is.null(cached))return(cached)
  } else attempted <<- NULL
  path <- system.file("extdata", "opentopography-registry.rds", package="alsdownloader")
  if (!nzchar(path)) return(sf::st_sf(dataset=character(), geometry=sf::st_sfc(crs=4326)))
  info <- file.info(path)
  current <- paste(path,info$size,as.numeric(info$mtime))
  if (!identical(current,signature)) {
    cached <<- readRDS(path); attr(cached,"catalog_directory") <<- dirname(path); signature <<- current
  }
  cached
  }
})

ot_detail_registry <- local({
  cached <- NULL; cached_hash <- NULL
  function(x, fetch=function(url,path,size) {
    response <- httr::GET(url,httr::write_disk(path),httr::timeout(180),
      httr::config(maxfilesize_large=size))
    if(httr::status_code(response)!=200L)stop("Detailed OpenTopography coverage is temporarily unavailable.")
  }) {
    resource <- attr(x,"detail_resource")
    if(is.null(resource))return(x)
    if(!is.character(resource$sha256) || length(resource$sha256)!=1L ||
       !grepl("^[0-9a-f]{64}$",resource$sha256) ||
       !is.numeric(resource$size_bytes) || length(resource$size_bytes)!=1L ||
       !is.finite(resource$size_bytes) || resource$size_bytes<=0 || resource$size_bytes>150*1024^2 ||
       !grepl("^https://github[.]com/Cesarito2021/als_downloader/releases/download/[^/]+/opentopography-registry[.]rds$",resource$url))
      stop("Invalid detailed-coverage resource.")
    if(!identical(cached_hash,resource$sha256)) {
      path <- file.path(tempdir(),paste0("als-ot-",resource$sha256,".rds"))
      matches <- function(p)file.exists(p) && identical(digest::digest(file=p,algo="sha256"),resource$sha256)
      if(!matches(path)) {
        part <- tempfile(fileext=".rds");on.exit(unlink(part),add=TRUE)
        fetch(resource$url,part,resource$size_bytes)
        if(!matches(part))stop("Detailed OpenTopography coverage failed checksum verification.")
        if(!file.rename(part,path))stop("Could not cache detailed coverage.")
      }
      full <- readRDS(path)
      if(!inherits(full,"sf") || !inherits(sf::st_geometry(full),"sfc") ||
         !identical(full$dataset,x$dataset))stop("Detailed coverage does not match this catalogue.")
      cached <<- full;cached_hash <<- resource$sha256
    }
    cached
  }
})

ot_index_file <- function(record) {
  name <- record$dataset[[1]]
  if (!grepl("^[A-Za-z0-9_-]+$", name)) stop("Invalid OpenTopography dataset identifier.")
  url <- record$index_url[[1]]
  if (!grepl("^https://opentopography[.]s3[.]sdsc[.]edu/pc-bulk/", url))
    stop("Unexpected OpenTopography index host.")
  cache <- file.path(tempdir(), "als-opentopography-indexes")
  dir.create(cache, showWarnings=FALSE)
  dest <- file.path(cache, paste0(name, "-", record$index_sha256[[1]], ".zip"))
  matches <- function(path) file.exists(path) && identical(
    digest::digest(file=path, algo="sha256"), record$index_sha256[[1]])
  if (matches(dest)) return(dest)
  part <- tempfile(tmpdir=cache, fileext=".part")
  on.exit(unlink(part), add=TRUE)
  response <- httr::GET(url, httr::write_disk(part), httr::timeout(120),
    httr::config(maxfilesize_large=150*1024^2))
  if (httr::status_code(response) != 200L) stop("OpenTopography tile index is unavailable: ", name)
  if (!matches(part)) stop("OpenTopography index changed since verification: ", name,
    ". Refresh the packaged source catalogue before downloading this collection.")
  if (!file.rename(part, dest)) stop("Could not cache OpenTopography index.")
  dest
}

ot_read_index <- function(path) {
  members <- utils::unzip(path, list=TRUE)
  if (!nrow(members) || sum(members$Length) > 500*1024^2 ||
      any(grepl("(^/|^[A-Za-z]:|(^|/)\\.\\.(/|$))", gsub("\\\\", "/", members$Name))))
    stop("Unsafe or oversized OpenTopography index.")
  shp <- members$Name[grepl("[.]shp$", members$Name, ignore.case=TRUE)]
  if (length(shp) != 1L) stop("OpenTopography index must contain one Shapefile.")
  x <- sf::st_read(paste0("/vsizip/", normalizePath(path,winslash="/"), "/", shp), quiet=TRUE)
  names(x) <- tolower(names(x))
  if (!"url" %in% names(x) || is.na(sf::st_crs(x))) stop("OpenTopography index lacks URLs or a CRS.")
  if (!all(sf::st_geometry_type(x) %in% c("POLYGON","MULTIPOLYGON"))) stop("Invalid tile geometry type.")
  sf::st_transform(sf::st_make_valid(sf::st_zm(x,drop=TRUE,what="ZM")),4326)
}

search_ot_catalog <- function(aoi, max_items, registry=ot_registry(), index_reader=ot_index_file) {
  if (!nrow(registry)) return(empty_tiles())
  bounds_only <- !is.null(attr(registry,"detail_resource"))
  registry <- registry[registry$access_status == "ready", , drop=FALSE]
  if (all(c("west","south","east","north") %in% names(registry))) {
    bb <- sf::st_bbox(aoi)
    registry <- registry[registry$west<=bb[['xmax']] & registry$east>=bb[['xmin']] &
      registry$south<=bb[['ymax']] & registry$north>=bb[['ymin']],,drop=FALSE]
  }
  # Dissolved national footprints can contain hundreds of thousands of rings.
  # A projected intersection avoids expensive spherical cross-ring validation
  # on every search; the original individual tile polygons are queried below.
  if(!bounds_only)registry <- registry[lengths(sf::st_intersects(sf::st_transform(registry,3857),
    sf::st_transform(aoi,3857))) > 0, , drop=FALSE]
  if (!nrow(registry)) return(empty_tiles())
  results <- list(); count <- 0L
  for (i in seq_len(nrow(registry))) {
    r <- registry[i,,drop=FALSE]
    x <- ot_read_index(index_reader(r))
    x <- x[lengths(sf::st_intersects(x,aoi)) > 0,,drop=FALSE]
    if (!nrow(x)) next
    urls <- as.character(x$url)
    if (anyNA(urls) || any(!grepl("^https://opentopography[.]s3[.]sdsc[.]edu/pc-bulk/.+[.](laz|las)$",urls,ignore.case=TRUE)))
      stop("Unverified OpenTopography tile URLs: ", r$dataset)
    count <- count + nrow(x)
    if (count > max_items) stop("OpenTopography search exceeds max_items. Use a smaller AOI or raise the explicit limit.")
    results[[length(results)+1L]] <- sf::st_sf(tile_id=paste(r$dataset,basename(urls),sep="/"),
      provider="opentopography", dataset=r$dataset, filename=basename(urls), url=urls,
      acquired_start=r$acquired_start, acquired_end=r$acquired_end, size_bytes=NA_real_,
      license_url=r$license_url, citation=r$citation, geometry=sf::st_geometry(x))
  }
  if (!length(results)) return(empty_tiles())
  out <- do.call(rbind,results)
  out[!duplicated(out$url),,drop=FALSE]
}

ot_external_coverage <- function() {
  x <- ot_registry()
  if (!nrow(x)) return(NULL)
  sf::st_transform(x[x$access_status == "external", , drop=FALSE],4326)
}

ot_visible_coverage <- function(x, bounds) {
  if (is.null(bounds) || !nrow(x)) return(x[0,,drop=FALSE])
  coords <- unlist(bounds[c("west","south","east","north")],use.names=FALSE)
  if(length(coords)!=4L || any(!is.finite(coords))) return(x[0,,drop=FALSE])
  coords[2] <- max(coords[2],-85.05112878)
  coords[4] <- min(coords[4],85.05112878)
  if(coords[2]>=coords[4])return(x[0,,drop=FALSE])
  west <- coords[1];east <- coords[3]
  width <- east-west
  if(width<0)width <- width+360
  west <- (west+180)%%360-180;east <- west+min(width,360)
  boxes <- if(east<=180) list(c(west,coords[2],east,coords[4])) else
    list(c(west,coords[2],180,coords[4]),c(-180,coords[2],east-360,coords[4]))
  pieces <- lapply(boxes,function(b) {
    selected <- x[x$west<=b[3] & x$east>=b[1] & x$south<=b[4] & x$north>=b[2],,drop=FALSE]
    if(!nrow(selected))return(NULL)
    clip <- sf::st_as_sfc(sf::st_bbox(stats::setNames(b,c("xmin","ymin","xmax","ymax")),crs=4326))
    projected <- sf::st_transform(selected,3857)
    cropped <- suppressWarnings(sf::st_intersection(projected,sf::st_transform(clip,3857)))
    cropped <- cropped[!sf::st_is_empty(cropped),,drop=FALSE]
    sf::st_transform(cropped,4326)
  })
  pieces <- Filter(Negate(is.null),pieces)
  if(length(pieces))do.call(rbind,pieces) else x[0,,drop=FALSE]
}

add_ot_map_coverage <- function(map, zoom=2, bounds=NULL) {
  x <- ot_registry()
  if(!nrow(x))return(map)
  detailed <- is.finite(zoom) && zoom>=9 && !is.null(bounds)
  if(detailed && !is.null(attr(x,"detail_resource"))) {
    if(!nrow(ot_visible_coverage(x,bounds)))return(map)
    x <- ot_detail_registry(x)
  }
  x <- x[x$access_status=="ready",,drop=FALSE]
  map <- leaflet::removeShape(map,paste0("ot-survey:",x$dataset))
  if(detailed)x <- ot_visible_coverage(x,bounds)
  if(!nrow(x))return(map)
  popups <- lapply(seq_len(nrow(x)),function(i)as.character(shiny::tags$div(
    shiny::tags$strong(paste("OpenTopography:",x$title[i])),
    shiny::tags$p(if(detailed) "Verified original tile footprints. Define an AOI to search and download." else
      "Survey location, not its coverage boundary. Zoom in to see exact tile footprints, or define an AOI and search."),
    shiny::tags$p(paste(format(x$tile_count[i],big.mark=","),"tiles; verified",x$reviewed_on[i])),
    shiny::tags$a(href=x$info_url[i],target="_blank",rel="noopener noreferrer","Source information"))))
  if(detailed) leaflet::addPolygons(map,data=x,group="In-App Access",layerId=paste0("ot-survey:",x$dataset),
    color="#ff4b4b",weight=1,fillColor="#ef4444",fillOpacity=.4,label=x$title,popup=popups,
    options=leaflet::pathOptions(pane="reference",bubblingMouseEvents=FALSE)) else
    leaflet::addCircleMarkers(map,lng=x$map_lon,lat=x$map_lat,group="In-App Access",
      layerId=paste0("ot-survey:",x$dataset),radius=4,color="#ff4b4b",weight=1,fillOpacity=.7,
      label=paste0(x$title," (survey location)"),popup=popups,
      options=leaflet::pathOptions(pane="reference",bubblingMouseEvents=FALSE))
}

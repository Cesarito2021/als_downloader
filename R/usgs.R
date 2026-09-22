# Official products are the inventory; a COPC mirror is an optional access.
find_als_usgs <- function(aoi, max_items) {
  bounds<-as.numeric(sf::st_bbox(aoi))
  # TNM rounds spatial queries internally; pad tiny envelopes, then intersect
  # returned footprints with the original AOI below.
  for(axis in 1:2) if(bounds[axis+2]-bounds[axis]<0.0004) {
    mid<-mean(bounds[c(axis,axis+2)]);bounds[c(axis,axis+2)]<-mid+c(-0.0002,0.0002)
  }
  bbox <- paste(format(bounds, scientific=FALSE, trim=TRUE, digits=12), collapse=",")
  items <- list(); offset <- 0L; seen <- character()
  repeat {
    page <- request_json("https://tnmaccess.nationalmap.gov/api/v1/products", query=list(
      datasets="Lidar Point Cloud (LPC)", bbox=bbox, max=1000L, offset=offset))
    if (length(page$errors)||!is.null(page$errorMessage)) stop("USGS returned an error; search incomplete.")
    if (is.null(page$total) || !is.numeric(page$total)) stop("USGS total is unavailable; search incomplete.")
    batch <- page$items
    if (!length(batch)) {
      if (offset < page$total) stop("USGS pagination ended early; search incomplete.")
      break
    }
    fingerprint <- digest::digest(batch)
    if (fingerprint %in% seen) stop("USGS repeated a page; search incomplete.")
    seen <- c(seen,fingerprint); items <- c(items,batch); offset <- offset+length(batch)
    if (length(items)>max_items) stop("Search exceeds max_items; use a smaller area or raise the explicit limit.")
    if (offset>=page$total) break
  }
  scalar <- function(x) if(is.character(x)&&length(x)==1L&&!is.na(x)&&nzchar(x)) x else NA_character_
  rows <- lapply(items,function(x) {
    u <- scalar(x$downloadLazURL); if(is.na(u)) u <- scalar(x$downloadURL)
    if(is.na(u)||!grepl("[.]la[sz]($|[?])",u,ignore.case=TRUE)) stop("USGS product has no supported original asset; search incomplete.")
    b <- x$boundingBox
    coords <- suppressWarnings(as.numeric(c(b$minX,b$minY,b$maxX,b$maxY)))
    if(length(coords)!=4L||any(!is.finite(coords))||coords[1]>=coords[3]||coords[2]>=coords[4]) stop("USGS footprint is invalid; search incomplete.")
    g <- sf::st_as_sfc(sf::st_bbox(stats::setNames(coords,c("xmin","ymin","xmax","ymax")),crs=4326))
    if(!any(lengths(sf::st_intersects(g,aoi))>0L)) return(NULL)
    project <- sub("/LAZ/.*$","",redact_url(u),ignore.case=TRUE)
    bytes <- suppressWarnings(as.numeric(x$sizeInBytes)); if(length(bytes)!=1L||!is.finite(bytes)||bytes<=0) bytes<-NA_real_
    sf::st_sf(tile_id=scalar(x$sourceId),provider="usgs3dep",dataset=basename(project),
      campaign_id=basename(project),filename=basename(redact_url(u)),url=u,
      acquired_start=NA_character_,acquired_end=NA_character_,size_bytes=bytes,
      license_url="https://www.usgs.gov/information-policies-and-instructions/copyrights-and-credits",
      citation=paste("USGS 3DEP original;",scalar(x$title)),description=scalar(x$title),
      source_url=scalar(x$metaUrl),metadata_url=scalar(x$vendorMetaUrl),
      project_metadata_url=scalar(x$metaUrl),geometry=g)
  })
  rows<-Filter(Negate(is.null),rows)
  if(!length(rows)) return(empty_tiles())
  out<-do.call(rbind,rows); out[!duplicated(redact_url(out$url)),,drop=FALSE]
}

usgs_project_xml <- function(url) {
  path<-sub("^.*/Projects/","",sub("/LAZ/.*$","",url,ignore.case=TRUE))
  if(identical(path,url)) return(NA_character_)
  prefix<-paste0("StagedProducts/Elevation/metadata/",path,"/")
  keys<-character();token<-NULL;visited<-character()
  repeat {
    q<-list(`list-type`=2,prefix=prefix)
    if(!is.null(token)) q[['continuation-token']]<-token
    r<-httr::GET("https://prd-tnm.s3.amazonaws.com/",query=q,httr::timeout(20));httr::stop_for_status(r)
    d<-xml2::read_xml(httr::content(r,"raw"),options="NONET")
    keys<-c(keys,xml2::xml_text(xml2::xml_find_all(d,'//*[local-name()="Key"]')))
    if(length(keys)>10000L) stop("Metadata inventory exceeds lookup limit.")
    token<-xml2::xml_text(xml2::xml_find_first(d,'//*[local-name()="NextContinuationToken"]'))
    if(is.na(token)||!nzchar(token)) break
    if(token %in% visited) stop("Metadata inventory repeats a page.")
    visited<-c(visited,token)
  }
  candidates<-keys[grepl("[.]xml$",keys,ignore.case=TRUE)&
    grepl("ClassifiedPointCloud|point.?cloud",keys,ignore.case=TRUE)&!grepl("[.]shp[.]xml$",keys,ignore.case=TRUE)]
  # Do not choose a document arbitrarily when several products are present.
  if(!length(candidates)||length(candidates)>10L) return(NA_character_)
  paste0("https://prd-tnm.s3.amazonaws.com/",gsub(" ","%20",candidates,fixed=TRUE))
}

asset_access_url <- function(tile) {
  url <- tile$url[[1]]
  # Existing saved PC rows remain usable without treating originals as Azure.
  if(grepl("^https://usgslidareuwest[.]blob[.]core[.]windows[.]net/",url)) {
    signed<-request_json("https://planetarycomputer.microsoft.com/api/sas/v1/sign",query=list(href=redact_url(url)))
    if(is.null(signed$href)) stop("Asset signing failed.")
    return(signed$href)
  }
  url
}

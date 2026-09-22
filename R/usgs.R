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

# List only direct children, so large spatial indexes do not exhaust XML lookup.
usgs_metadata_listing <- function(prefix, delimiter = "/") {
  keys <- character(); folders <- character(); token <- NULL; visited <- character()
  for (page in seq_len(20L)) {
    q <- list(`list-type` = 2, prefix = prefix, delimiter = delimiter)
    if (!is.null(token)) q[["continuation-token"]] <- token
    r <- httr::GET("https://prd-tnm.s3.amazonaws.com/", query = q, httr::timeout(20))
    httr::stop_for_status(r)
    d <- xml2::read_xml(httr::content(r, "raw"), options = "NONET")
    keys <- c(keys, xml2::xml_text(xml2::xml_find_all(d, '//*[local-name()="Contents"]/*[local-name()="Key"]')))
    folders <- c(folders, xml2::xml_text(xml2::xml_find_all(d, '//*[local-name()="CommonPrefixes"]/*[local-name()="Prefix"]')))
    token <- xml2::xml_text(xml2::xml_find_first(d, '//*[local-name()="NextContinuationToken"]'))
    if (is.na(token) || !nzchar(token)) return(list(keys = unique(keys), folders = unique(folders)))
    if (token %in% visited) stop("Metadata inventory repeats a page.")
    visited <- c(visited, token)
  }
  stop("Metadata inventory exceeds lookup limit.")
}

usgs_project_xml <- function(url) {
  if (!grepl("/Projects/.+/LAZ/", url, ignore.case = TRUE)) return(NA_character_)
  path <- sub("^.*/Projects/", "", sub("/LAZ/.*$", "", url, ignore.case = TRUE))
  prefix <- paste0("StagedProducts/Elevation/metadata/", path, "/")
  root <- usgs_metadata_listing(prefix)
  keys <- root$keys
  # Reports contain vendor metadata. Never descend into spatial_metadata assets.
  queue <- root$folders[grepl("/(reports?|metadata|xml)/$", root$folders, ignore.case = TRUE)]
  visited <- character()
  while (length(queue)) {
    folder <- queue[1]; queue <- queue[-1]
    if (folder %in% visited) next
    if (length(visited) >= 30L) stop("Metadata directory lookup exceeds limit.")
    visited <- c(visited, folder)
    listing <- usgs_metadata_listing(folder)
    keys <- c(keys, listing$keys)
    # Ancillary report trees can contain thousands of non-metadata files.
    metadata_folders <- listing$folders[grepl("xml|metadata", substring(listing$folders, nchar(prefix) + 1L), ignore.case = TRUE)]
    queue <- c(queue, metadata_folders)
  }
  candidates <- unique(keys[grepl("[.]xml$", keys, ignore.case = TRUE) &
    !grepl("[.]shp[.]xml$|breakline|intensity|(^|[/_])dem([/_.]|$)", keys, ignore.case = TRUE)])
  cloud <- candidates[grepl("ClassifiedPointCloud|point.?cloud", candidates, ignore.case = TRUE)]
  if (length(cloud)) candidates <- cloud
  if (!length(candidates) || length(candidates) > 10L) return(NA_character_)
  paste0("https://prd-tnm.s3.amazonaws.com/", gsub(" ", "%20", candidates, fixed = TRUE))
}

# A legacy project folder can contain metadata for just one named LAS tile.
usgs_document_period <- function(text, asset_url) {
  doc <- xml2::read_xml(text, options = "NONET"); xml2::xml_ns_strip(doc)
  title <- trimws(xml2::xml_text(xml2::xml_find_first(doc, "//idinfo/citation/citeinfo/title")))
  if (is.na(title) || !nzchar(title)) return(NULL)
  tile_title <- grepl("[.]la[sz]$", title, ignore.case = TRUE)
  if (tile_title) {
    identity <- function(x) tolower(sub("[.]la[sz]$", "", basename(x), ignore.case = TRUE))
    if (!identical(identity(title), identity(redact_url(asset_url)))) return(NULL)
  } else if (!grepl("classified.*point.?cloud|lidar.*point.?cloud", title, ignore.case = TRUE)) return(NULL)
  result <- extract_als_dates_usgs(text)
  if (!is.null(result)) result$scope <- if (tile_title) "tile" else "project"
  result
}

usgs_asset_period <- function(asset, metadata, cache, deadline) {
  failure <- function() structure(list(), class = "metadata_failure")
  project <- sub("/LAZ/.*$", "", asset, ignore.case = TRUE)
  key <- paste0("project:", project)
  if (!exists(key, cache, inherits = FALSE)) {
    if (Sys.time() >= deadline) return(structure(list(), class = "metadata_budget"))
    assign(key, tryCatch(usgs_project_xml(asset), error = function(e) failure()), cache)
  }
  links <- get(key, cache, inherits = FALSE)
  discovery_failed <- inherits(links, "metadata_failure")
  if (discovery_failed) links <- character()
  links <- links[!is.na(links)]
  # Folder landing pages are not XML metadata. Use direct XML as a fallback only.
  if (!length(links) && !is.na(metadata) && grepl("[.]xml($|[?])", metadata, ignore.case = TRUE)) links <- metadata
  if (!length(links)) return(if (discovery_failed) failure() else NULL)
  periods <- lapply(links, function(link) {
    raw_key <- paste0("xml:", link)
    if (!exists(raw_key, cache, inherits = FALSE)) {
      if (Sys.time() >= deadline) return(structure(list(), class = "metadata_budget"))
      assign(raw_key, tryCatch(als_metadata_text(link), error = function(e) failure()), cache)
    }
    raw <- get(raw_key, cache, inherits = FALSE)
    if (inherits(raw, "metadata_failure")) return(raw)
    value <- tryCatch(usgs_document_period(raw, asset), error = function(e) failure())
    if (inherits(value, "metadata_failure")) return(value)
    if (!is.null(value)) value$source <- link
    value
  })
  failed <- vapply(periods, inherits, logical(1), "metadata_failure")
  had_failure <- any(failed)
  periods <- periods[!failed]
  if (any(vapply(periods, inherits, logical(1), "metadata_budget"))) return(structure(list(), class = "metadata_budget"))
  # Different tile-specific records may coexist in a legacy project folder.
  periods <- Filter(Negate(is.null), periods)
  if (!length(periods)) return(if (had_failure) failure() else NULL)
  if (any(vapply(periods, function(x) isTRUE(x$conflict), logical(1)))) return(list(conflict = TRUE))
  signatures <- vapply(periods, function(x) paste(x$start, x$end, x$year), character(1))
  if (length(unique(signatures)) == 1L) return(periods[[1]])
  years <- vapply(periods, function(x) as.integer(x$year), integer(1))
  if (length(unique(years)) != 1L) return(list(conflict = TRUE))
  list(start = NA_character_, end = NA_character_, year = years[1], precision = "year",
    scope = "project_year_consensus", source = periods[[1]]$source,
    evidence = paste(vapply(periods, `[[`, character(1), "source"), collapse = "; "))
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

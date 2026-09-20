# Public Zenodo links are metadata sources, never approval to publish coverage.
zenodo_record_id <- function(link) {
  if (!is.character(link) || length(link)!=1L || is.na(link) || nchar(link)>500) stop("Enter a Zenodo DOI or record link.")
  link <- trimws(link)
  pattern <- "^(?:https://zenodo\\.org/(?:records?|record)/|https://doi\\.org/10\\.5281/zenodo\\.|10\\.5281/zenodo\\.|)([0-9]+)(?:/?(?:\\?[^#]*)?)$"
  if (!grepl(pattern,link,perl=TRUE)) stop("Use a published zenodo.org record link or a 10.5281/zenodo DOI.")
  sub(pattern,"\\1",link,perl=TRUE)
}

zenodo_get <- function(id) {
  path <- tempfile(fileext=".json"); on.exit(unlink(path))
  r <- httr::GET(paste0("https://zenodo.org/api/records/",zenodo_record_id(id)),
    httr::timeout(25),httr::config(followlocation=FALSE,maxfilesize_large=8*1024^2),httr::write_disk(path))
  if (httr::status_code(r)!=200L) stop("Zenodo metadata could not be read. Check the public record link and try again.")
  if (file.size(path)>8*1024^2) stop("Zenodo metadata exceeds the 8 MiB limit.")
  jsonlite::fromJSON(path,simplifyVector=FALSE)
}

zenodo_metadata <- function(record) {
  m <- record$metadata; id <- as.character(record$id)
  if (length(id)!=1L || !grepl("^[0-9]+$",id) || !identical(m$access_right,"open")) stop("Only published, openly accessible Zenodo records can be proposed.")
  text <- function(x) if(is.null(x)) "" else paste(as.character(x),collapse=" ")
  title <- text(m$title); doi <- text(m$doi)
  if (!identical(doi,paste0("10.5281/zenodo.",id)) || !nzchar(title)) stop("Zenodo must provide a version DOI and title.")
  files <- lapply(record$files,function(f)list(key=text(f$key),size=as.numeric(f$size),checksum=text(f$checksum)))
  if (!length(files) || length(files)>2000L) stop("The record must list 1 to 2,000 files.")
  for(f in files) if(!nzchar(f$key) || nchar(f$key)>250 || grepl("[/\\\\]",f$key) ||
    length(f$size)!=1 || !is.finite(f$size) || f$size<=0) stop("Invalid Zenodo file metadata.")
  if(anyDuplicated(vapply(files,`[[`,"","key"))) stop("Duplicate Zenodo filenames.")
  licence <- text(m$license$id)
  known <- c('cc-by-4.0'='https://creativecommons.org/licenses/by/4.0/',
    'cc-by-sa-4.0'='https://creativecommons.org/licenses/by-sa/4.0/',
    'cc-by-nc-4.0'='https://creativecommons.org/licenses/by-nc/4.0/',
    'cc-by-nc-sa-4.0'='https://creativecommons.org/licenses/by-nc-sa/4.0/',
    'cc-zero'='https://creativecommons.org/publicdomain/zero/1.0/',
    'cc0-1.0'='https://creativecommons.org/publicdomain/zero/1.0/')
  licence_url <- if(licence %in% names(known)) unname(known[licence]) else text(m$license$url)
  authors <- paste(vapply(m$creators,function(x)text(x$name),""),collapse="; ")
  if(!nzchar(authors)) stop("Zenodo authors are missing.")
  citation <- paste(authors,paste0("(",substr(text(m$publication_date),1,4),")."),title,paste0("https://doi.org/",doi))
  list(id=id,doi=doi,title=title,authors=authors,citation=citation,license=licence,license_url=licence_url,
    description=text(m$description),acknowledgement=text(m$notes),files=files,
    fingerprint=digest::digest(list(id,doi,title,authors,citation,licence,licence_url,m$description,m$notes,files),algo="sha256"))
}

#' Inspect a public Zenodo record without downloading point clouds
#' @param link Published Zenodo record URL, DOI or numeric record ID.
#' @return Metadata including authors, licence, description, file sizes and links.
#' @details Only retrieves a bounded JSON metadata response from zenodo.org.
#'   Publication dates are never treated as acquisition dates. No catalogue changes.
#' @export
inspect_zenodo <- function(link) zenodo_metadata(zenodo_get(zenodo_record_id(link)))

zenodo_dates <- function(value) {
  if (is.null(value) || !nzchar(trimws(value))) return(c(NA_character_,NA_character_))
  value <- trimws(value)
  if(grepl("^[0-9]{4}(, *[0-9]{4})+$",value)) {
    years<-as.integer(trimws(strsplit(value,",",fixed=TRUE)[[1]]))
    if(anyDuplicated(years) || any(years<1900 | years>as.integer(format(Sys.Date(),"%Y"))))stop("Check the acquisition years.")
    value<-paste(range(years),collapse="-")
  }
  if(grepl("^[0-9]{4}(-[0-9]{4})?$",value)) {
    years <- strsplit(value,"-",fixed=TRUE)[[1]]
    dates <- c(paste0(years[1],"-01-01"),paste0(utils::tail(years,1),"-12-31"))
  } else dates <- trimws(strsplit(value,"/",fixed=TRUE)[[1]])
  if(length(dates)==1) dates <- rep(dates,2)
  if(length(dates)!=2 || any(!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$",dates))) stop("Use a year, year range, or YYYY-MM-DD / YYYY-MM-DD. Leave blank if unknown.")
  parsed <- suppressWarnings(as.Date(dates,format="%Y-%m-%d"))
  if(anyNA(parsed) || any(format(parsed,"%Y-%m-%d")!=dates) || dates[1]>dates[2] ||
    dates[1]<"1900-01-01" || dates[2]>paste0(format(Sys.Date(),"%Y"),"-12-31")) stop("Check the acquisition dates.")
  dates
}

zenodo_boundary <- function(boundary) {
  path <- if(is.list(boundary) && !inherits(boundary,"sf")) boundary$datapath else boundary
  if(is.character(path) && (length(path)!=1 || !file.exists(path) || file.size(path)>5*1024^2)) stop("Coverage file must be at most 5 MiB.")
  g <- read_aoi(boundary)
  if(nrow(g)>10000L || nrow(sf::st_coordinates(g))>100000L) stop("Use at most 10,000 polygons and 100,000 vertices.")
  g
}

zenodo_map_boundary <- function(boundary, key) {
  g <- zenodo_boundary(boundary)
  if ("file_key" %in% names(g) && anyNA(g$file_key)) stop("The polygon file contains an incomplete file_key mapping.")
  if ("file_key" %in% names(g) && any(g$file_key != key))
    stop("The selected file conflicts with the polygon file_key mapping. Use its existing mapping or correct the polygon file.")
  g$file_key <- key
  g
}

zenodo_build <- function(metadata,boundary,acquired,platform,email="") {
  if(!platform %in% c("ALS","UAV-LiDAR")) stop("Confirm aircraft/helicopter ALS or UAV LiDAR.")
  if(length(email)!=1 || is.na(email) || nchar(email)>254 || (nzchar(email) && !grepl("^[^[:space:]@]+@[^[:space:]@]+\\.[^[:space:]@]+$",email))) stop("Check the optional contact email.")
  if(!grepl("^https://[^[:space:]]+$",metadata$license_url)) stop("Zenodo has no supported explicit licence URL. Ask the depositor to complete the record before submission.")
  g <- zenodo_boundary(boundary); dates <- zenodo_dates(acquired)
  files <- metadata$files
  keys <- vapply(files,`[[`,"","key")
  direct <- grepl("\\.(las|laz)$",keys,ignore.case=TRUE)
  supported <- direct | grepl("\\.zip$",keys,ignore.case=TRUE)
  if(!any(supported)) stop("No LAS, LAZ or ZIP asset is listed. This importer does not yet support other archive formats.")
  if(!"file_key" %in% names(g)) {
    candidates <- keys[if(any(direct)) direct else supported]
    if(length(candidates)!=1) stop("Several data files are listed. Add a file_key column to the coverage file, using the exact Zenodo filename for each polygon.")
    g$file_key <- candidates[1]
  }
  if(anyNA(g$file_key) || any(!g$file_key %in% keys[supported])) stop("Every file_key must match a LAS, LAZ or ZIP filename in this Zenodo record.")
  g$file_key <- as.character(g$file_key)
  # Keep one downloadable asset per row even if its footprint has several pieces.
  unique_keys <- unique(g$file_key)
  approximate <- "coverage_method" %in% names(g) && any(g$coverage_method=="author_approximate_square",na.rm=TRUE)
  if(approximate) {
    fields<-c("extent_longitude","extent_latitude","extent_distance_m")
    if(!all(fields %in% names(g)) || anyNA(g$coverage_method) ||
       !all(g$coverage_method=="author_approximate_square") ||
       any(vapply(sf::st_drop_geometry(g)[fields],function(x)length(unique(x))!=1L,logical(1))))
      stop("An approximate proposal must describe one centre and distance for its selected files.")
    g<-zenodo_square(g$extent_longitude[1],g$extent_latitude[1],g$extent_distance_m[1],unique_keys)
  }
  shapes <- lapply(unique_keys,function(k)sf::st_union(sf::st_geometry(g[g$file_key==k,])))
  rows <- sf::st_sf(file_key=unique_keys,geometry=do.call(c,shapes))
  index <- report_geojson(rows)
  # Decimal serialization can collapse nearly identical vertices in unions.
  # Repair the serialized geometry, then validate the final index below.
  geometry_file<-tempfile(fileext=".geojson");on.exit(unlink(geometry_file),add=TRUE)
  jsonlite::write_json(index,geometry_file,auto_unbox=TRUE,null="null",digits=NA)
  serialized<-sf::st_read(geometry_file,quiet=TRUE)
  if(any(!sf::st_is_valid(serialized)))index<-report_geojson(sf::st_make_valid(serialized))
  index$name <- paste0("zenodo-",metadata$id)
  for(i in seq_along(index$features)) {
    file <- files[[match(unique_keys[i],keys)]]
    index$features[[i]]$properties <- list(tile_id=paste0(metadata$id,"-",i),dataset=metadata$title,
      url=paste0("https://zenodo.org/records/",metadata$id,"/files/",utils::URLencode(file$key,reserved=TRUE)),
      acquired_start=if(is.na(dates[1]))NULL else dates[1],acquired_end=if(is.na(dates[2]))NULL else dates[2],
      acquisition_years=if(grepl("^[0-9]{4}(, *[0-9]{4})*$",acquired))as.integer(trimws(strsplit(acquired,",",fixed=TRUE)[[1]])) else NULL,
      platform=platform,license_url=metadata$license_url,citation=metadata$citation,size_bytes=file$size,
      file_key=file$key,checksum=file$checksum,zenodo_doi=metadata$doi)
    if(approximate) {
      props<-index$features[[i]]$properties
      props$dataset<-paste(metadata$title,"[approximate extent]")
      props$citation<-paste(metadata$citation,"Search extent is an author-declared approximate square; LiDAR coverage within it is not verified.")
      props$coverage_method<-"author_approximate_square"
      props$extent_longitude<-g$extent_longitude[1]
      props$extent_latitude<-g$extent_latitude[1]
      props$extent_distance_m<-g$extent_distance_m[1]
      index$features[[i]]$properties<-props
    }
  }
  scratch <- tempfile(fileext=".geojson");on.exit(unlink(scratch))
  jsonlite::write_json(index,scratch,auto_unbox=TRUE,null="null",digits=NA)
  if(file.size(scratch)>5*1024^2) stop("The generated index exceeds 5 MiB; simplify the coverage.")
  read_tile_index(scratch)
  proposal <- list(schema="als-zenodo-proposal-v1",metadata=metadata,index=index,acquired=acquired,
    platform=platform,contact_email=email)
  proposal$id <- digest::digest(as.character(jsonlite::toJSON(list(metadata$fingerprint,index),auto_unbox=TRUE,null="null",digits=NA)),algo="sha256")
  proposal
}

#' Prepare a Zenodo contribution for maintainer review
#' @param link Public Zenodo DOI or record link.
#' @param boundary GeoJSON, GeoPackage, zipped Shapefile or sf polygons. For
#'   multiple assets include file_key with exact Zenodo filenames.
#' @param acquired Collection year, year range or YYYY-MM-DD / YYYY-MM-DD;
#'   blank means unknown and never substitutes the publication date.
#' @param platform ALS or UAV-LiDAR, confirmed by the contributor.
#' @param email Optional private contact email, excluded from approved indexes.
#' @return A pending proposal; no files are published and no point clouds read.
#' @export
prepare_zenodo_submission <- function(link,boundary,acquired="",platform="ALS",email="") {
  zenodo_build(inspect_zenodo(link),boundary,acquired,platform,email)
}

zenodo_write <- function(x,path) {
  dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
  tmp <- tempfile(tmpdir=dirname(path));on.exit(unlink(tmp))
  jsonlite::write_json(x,tmp,auto_unbox=TRUE,null="null",digits=NA,pretty=TRUE)
  if(!file.rename(tmp,path)) stop("Could not save the proposal atomically.")
}

#' Submit a proposal to a private local review queue
#' @param proposal Result of prepare_zenodo_submission().
#' @param queue Administrator-controlled private queue directory.
#' @return Proposal ID. Identical metadata/coverage proposals are deduplicated.
#' @details Does not activate coverage or download data. Optional administrator
#'   configuration in option `alsdownloader.submission_mail` enables notifications;
#'   see the notification setup guide in the repository documentation.
#' @export
submit_zenodo <- function(proposal,queue) {
  if(!identical(proposal$schema,"als-zenodo-proposal-v1") || !grepl("^[a-f0-9]{64}$",proposal$id)) stop("Invalid proposal.")
  if(length(queue)!=1 || !is.character(queue) || !nzchar(queue)) stop("Configure a private review queue.")
  path <- file.path(queue,"requests",paste0(proposal$id,".json"))
  dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
  lock <- paste0(path,".lock")
  if(!dir.create(lock,showWarnings=FALSE)) stop("This request is being saved. Retry shortly.")
  on.exit(unlink(lock,recursive=TRUE),add=TRUE)
  if(!file.exists(path)) {proposal$submitted_at<-format(Sys.time(),tz="UTC",usetz=TRUE);zenodo_write(proposal,path)}
  zenodo_notify(queue,proposal$id)
  proposal$id
}

#' List pending and reviewed Zenodo contributions
#' @inheritParams submit_zenodo
#' @return A table of request IDs, DOI, title and status. Private contacts are omitted.
#' @export
zenodo_submissions <- function(queue) {
  paths<-list.files(file.path(queue,"requests"),"^[a-f0-9]{64}\\.json$",full.names=TRUE)
  if(!length(paths)) return(data.frame(id=character(),doi=character(),title=character(),status=character()))
  do.call(rbind,lapply(paths,function(path){
    r<-zenodo_proposal(queue,sub("\\.json$","",basename(path)))
    decision<-file.path(queue,"decisions",paste0(r$id,".json"))
    status<-if(file.exists(decision))jsonlite::fromJSON(decision)$decision else "pending"
    data.frame(id=r$id,doi=r$metadata$doi,title=r$metadata$title,status=status)
  }))
}

zenodo_proposal <- function(queue,id) {
  if(length(id)!=1 || !grepl("^[a-f0-9]{64}$",id))stop("Invalid request ID.")
  path<-file.path(queue,"requests",paste0(id,".json"))
  if(!file.exists(path) || file.size(path)>12*1024^2)stop("Missing or oversized request.")
  r<-jsonlite::fromJSON(path,simplifyVector=FALSE)
  if(!identical(r$id,id) || !identical(r$schema,"als-zenodo-proposal-v1"))stop("Invalid stored proposal.")
  r
}

#' Accept or reject a queued Zenodo contribution
#' @inheritParams submit_zenodo
#' @param id Proposal ID.
#' @param decision approve or reject.
#' @param reviewer Maintainer name for the audit record.
#' @param confirmed Explicit confirmation of coverage/file mapping, dates,
#'   aerial LiDAR content, attribution and licence suitability. Required to approve.
#' @param reason Optional private review note.
#' @return Path to an approved index, or invisibly NULL on rejection.
#' @details Approval re-fetches Zenodo metadata and rejects changed records.
#'   Only approved indexes under queue/approved participate in catalogue search.
#'   Never downloads point clouds. Expose this operation only to trusted maintainers.
#' @export
review_zenodo_submission <- function(queue,id,decision=c("approve","reject"),reviewer,confirmed=FALSE,reason="") {
  decision<-match.arg(decision)
  if(missing(reviewer) || length(reviewer)!=1 || !nzchar(trimws(reviewer)))stop("Identify the reviewing maintainer.")
  r<-zenodo_proposal(queue,id)
  lock<-file.path(queue,paste0(id,".review-lock"))
  if(!dir.create(lock,showWarnings=FALSE))stop("This request is already being reviewed.")
  on.exit(unlink(lock,recursive=TRUE),add=TRUE)
  decision_path<-file.path(queue,"decisions",paste0(id,".json"))
  if(file.exists(decision_path))stop("This request already has a decision.")
  path<-NULL
  if(decision=="approve") {
    if(!isTRUE(confirmed))stop("Maintainer confirmation is required before approval.")
    fresh<-inspect_zenodo(r$metadata$id)
    if(!identical(fresh$fingerprint,r$metadata$fingerprint))stop("Zenodo metadata changed. Request a new proposal before approving.")
    temp<-tempfile(fileext=".geojson");on.exit(unlink(temp),add=TRUE)
    jsonlite::write_json(r$index,temp,auto_unbox=TRUE,null="null",digits=NA)
    boundary<-sf::st_read(temp,quiet=TRUE)
    verified<-zenodo_build(fresh,boundary,r$acquired,r$platform,"")
    stored_id<-digest::digest(as.character(jsonlite::toJSON(list(fresh$fingerprint,r$index),auto_unbox=TRUE,null="null",digits=NA)),algo="sha256")
    properties<-function(x)as.character(jsonlite::toJSON(lapply(x$features,`[[`,"properties"),auto_unbox=TRUE,null="null",digits=NA))
    if(!identical(properties(verified$index),properties(r$index)) || !identical(stored_id,r$id))stop("Proposal contents do not match the validated metadata and geometry.")
    path<-file.path(queue,"approved",paste0(id,".tiles.geojson"))
    zenodo_write(verified$index,path)
  }
  zenodo_write(list(id=id,decision=decision,reviewer=reviewer,reason=reason,
    reviewed_at=format(Sys.time(),tz="UTC",usetz=TRUE)),decision_path)
  invisible(path)
}

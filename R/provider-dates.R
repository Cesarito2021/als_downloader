extract_als_dates_ahn <- function(tiles) {
  # This auxiliary product is explicitly AHN6 2025, not all AHN editions.
  eligible<-which(tiles$provider=="ahn6" & grepl("AHN6_2025",tiles$filename,fixed=TRUE))
  if(!length(eligible)) return(tiles)
  url<-"https://basisdata.nl/hwh-ahn/AUX/omhullen/AHN6_2025_omhullen_clip.gpkg"
  path<-getOption("ALSdownloadeR.ahn_date_index",file.path(tempdir(),"als-ahn6-2025-dates.gpkg"))
  index<-tryCatch({
    if(!file.exists(path)) {
      part<-paste0(path,".part")
      on.exit(if(file.exists(part)) unlink(part),add=TRUE)
      r<-httr::GET(url,httr::write_disk(part,overwrite=TRUE),httr::timeout(90),httr::config(maxfilesize_large=60*1024^2))
      httr::stop_for_status(r)
      sf::st_read(part,quiet=TRUE)
      if(!file.rename(part,path)) stop("Cannot cache AHN date index.")
    }
    sf::st_read(path,quiet=TRUE)
  },error=function(e) NULL)
  if(is.null(index)||!"datum_str" %in% names(index)) {tiles$date_status[eligible]<-"lookup_failed";return(tiles)}
  matches<-sf::st_intersects(sf::st_transform(tiles[eligible,],sf::st_crs(index)),index)
  for(k in seq_along(eligible)) {
    i<-eligible[k];dates<-parse_als_date(as.character(index$datum_str[matches[[k]]]))
    if(!length(dates)||anyNA(dates)) next
    tiles$acquired_start[i]<-min(dates);tiles$acquired_end[i]<-max(dates)
    tiles$acquisition_year[i]<-as.integer(substr(max(dates),1,4));tiles$date_source[i]<-url
    tiles$metadata_url[i]<-url;tiles$date_scope[i]<-"intersecting_flight_strips"
    tiles$date_status[i]<-"documented_acquisition";tiles$date_precision[i]<-"day"
    tiles$date_evidence[i]<-paste(sort(unique(dates)),collapse="; ")
  }
  tiles
}

extract_als_dates_swisstopo <- function(tiles) {
  for(i in which(tiles$provider=="swisstopo")) {
    b<-sf::st_bbox(sf::st_transform(tiles[i,],2056))
    response<-tryCatch(request_json("https://api3.geo.admin.ch/rest/services/all/MapServer/identify",query=list(
      geometry=paste(mean(b[c(1,3)]),mean(b[c(2,4)]),sep=","),geometryType="esriGeometryPoint",sr=2056,
      layers="all:ch.swisstopo.swisssurface3d.metadata",tolerance=0,mapExtent=paste(b,collapse=","),
      imageDisplay="1000,1000,96",returnGeometry="false",lang="en")),error=function(e) NULL)
    if(is.null(response)) {tiles$date_status[i]<-"lookup_failed";next}
    key<-regmatches(tiles$filename[i],regexpr("[0-9]{4}-[0-9]{4}",tiles$filename[i]))
    matches<-Filter(function(x) identical(as.character(x$id),gsub("-","_",key)),response$results)
    if(length(matches)!=1L) next
    a<-matches[[1]]$attributes
    start<-suppressWarnings(as.integer(a$gpstime_min));end<-suppressWarnings(as.integer(a$gpstime_max))
    reference<-regmatches(tiles$filename[i],regexpr("(?<=swisssurface3d_)[0-9]{4}",tiles$filename[i],perl=TRUE))
    if(length(start)!=1L||length(end)!=1L||anyNA(c(start,end))||start<1900||end>2100||start>end) next
    # The identify layer may describe a newer version: never attach it to an old tile.
    if(length(reference)!=1L||!nzchar(reference)||!as.integer(reference) %in% seq.int(start,end)) next
    tiles$acquisition_year[i]<-end;tiles$date_precision[i]<-"year";tiles$date_scope[i]<-"tile_version"
    tiles$date_status[i]<-"documented_acquisition";tiles$date_source[i]<-"swisstopo LiDAR metadata"
    tiles$date_evidence[i]<-paste(start,end,sep="/")
    tiles$metadata_url[i]<-"https://map.geo.admin.ch/?layers=ch.swisstopo.swisssurface3d.metadata"
  }
  tiles
}

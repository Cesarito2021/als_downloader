als_metadata_columns <- function(tiles) {
  defaults<-list(acquisition_year=NA_integer_,date_source=NA_character_,date_status="not_checked",
    date_precision=NA_character_,date_scope=NA_character_,date_evidence=NA_character_,
    project_metadata_url=NA_character_,metadata_url=NA_character_,source_url=NA_character_,description=NA_character_,
    size_source=NA_character_,size_status="not_checked",size_checked_at=NA_character_)
  for(n in names(defaults)) if(!n %in% names(tiles)) tiles[[n]]<-rep(defaults[[n]],nrow(tiles))
  pages<-c(opentopography="https://portal.opentopography.org/",ahn6="https://www.ahn.nl/dataroom",
    ignfr="https://geoservices.ign.fr/lidarhd",swisstopo="https://www.swisstopo.admin.ch/en/height-model-swisssurface3d",
    canelevation="https://open.canada.ca/data/en/dataset/7069387e-9986-4297-9f55-0288e9676947")
  missing<-is.na(tiles$source_url)
  tiles$source_url[missing]<-unname(pages[tiles$provider[missing]])
  tiles
}

parse_als_date <- function(x) {
  x<-trimws(x); x<-sub("T.*$","",x)
  x<-ifelse(grepl("^[0-9]{8}$",x),paste0(substr(x,1,4),"-",substr(x,5,6),"-",substr(x,7,8)),x)
  valid<-grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$",x)
  parsed<-suppressWarnings(as.Date(ifelse(valid,x,NA_character_),format="%Y-%m-%d"))
  x[!valid|is.na(parsed)|(!is.na(parsed)&format(parsed,"%Y-%m-%d")!=x)]<-NA_character_
  x
}

extract_als_dates_usgs <- function(text) {
  doc<-xml2::read_xml(text,options="NONET"); xml2::xml_ns_strip(doc)
  nodes<-xml2::xml_find_all(doc,"//idinfo/timeperd")
  candidates<-list()
  for(node in nodes) {
    current<-trimws(xml2::xml_text(xml2::xml_find_first(node,"./current")))
    if(is.na(current)||!grepl("ground|acquisition|collection|flight",current,ignore.case=TRUE)||
       grepl("publication|processing",current,ignore.case=TRUE)) next
    begins<-xml2::xml_text(xml2::xml_find_all(node,"./timeinfo/rngdates/begdate"))
    ends<-xml2::xml_text(xml2::xml_find_all(node,"./timeinfo/rngdates/enddate"))
    days<-xml2::xml_text(xml2::xml_find_all(node,"./timeinfo/sngdate/caldate | ./timeinfo/mdattim/sngdate/caldate"))
    if(length(days)) {begins<-days;ends<-days}
    if(!length(begins)||!length(ends)) next
    dates<-c(begins,ends)
    if(all(grepl("^[12][0-9]{3}$",dates))) {
      if(min(as.integer(begins))>max(as.integer(ends))) next
      candidates[[length(candidates)+1L]]<-list(start=NA_character_,end=NA_character_,
        year=max(as.integer(ends)),precision="year",evidence=paste(dates,collapse="; "))
    } else {
      a<-parse_als_date(begins);b<-parse_als_date(ends)
      if(anyNA(c(a,b))||min(a)>max(b)) next
      candidates[[length(candidates)+1L]]<-list(start=min(a),end=max(b),year=as.integer(substr(max(b),1,4)),
        precision="day",evidence=paste(dates,collapse="; "))
    }
  }
  if(!length(candidates)) return(NULL)
  if(length(unique(vapply(candidates,function(x)paste(x$start,x$end,x$year),character(1))))>1L)
    return(list(conflict=TRUE))
  candidates[[1]]
}

als_metadata_text <- function(url) {
  response<-httr::GET(url,httr::timeout(20))
  httr::stop_for_status(response)
  httr::content(response,"text",encoding="UTF-8")
}

#' Extract acquisition years and retain their evidence
#' @param tiles An ALS asset table.
#' @param lookup Whether to consult linked provider metadata.
#' @param lookup_budget Maximum elapsed seconds before starting more metadata lookups.
#' @return The table with acquisition_year, source, precision, scope and status.
#'   Missing values remain NA. Filename years are labelled unverified references.
#' @export
extract_als_dates <- function(tiles, lookup=TRUE, lookup_budget=60) {
  stopifnot(length(lookup_budget)==1L,is.finite(lookup_budget),lookup_budget>0)
  deadline<-Sys.time()+lookup_budget
  tiles<-als_metadata_columns(tiles); cache<-new.env(parent=emptyenv())
  for(i in seq_len(nrow(tiles))) {
    period<-c(tiles$acquired_start[i],tiles$acquired_end[i])
    if(!anyNA(period)&&period[1]<=period[2]) {
      tiles$acquisition_year[i]<-as.integer(substr(period[2],1,4))
      tiles$date_source[i]<-"provider_catalog"; tiles$date_status[i]<-"catalog_reported"
      tiles$date_precision[i]<-"day";tiles$date_scope[i]<-if(tiles$provider[i]=="opentopography") "dataset" else "tile"
    }
    if(lookup&&Sys.time()<deadline&&tiles$provider[i]=="usgs3dep") {
      u<-tiles$metadata_url[i]
      if(is.na(u)) u<-"metadata_not_provided"
      project<-sub("/LAZ/.*$","",tiles$url[i],ignore.case=TRUE)
      result<-tryCatch({
        project_key<-paste0("project:",project)
        if(!exists(project_key,cache,inherits=FALSE)) assign(project_key,tryCatch(usgs_project_xml(tiles$url[i]),error=function(e) NA_character_),cache)
        project_xml<-get(project_key,cache,inherits=FALSE)
        if(length(project_xml)>1L) {
          periods<-lapply(project_xml,function(link) {
            if(!exists(link,cache,inherits=FALSE)) assign(link,tryCatch(extract_als_dates_usgs(als_metadata_text(link)),error=function(e) NULL),cache)
            get(link,cache,inherits=FALSE)
          })
          signatures<-vapply(periods,function(x) if(is.null(x)||isTRUE(x$conflict)) "unresolved" else paste(x$start,x$end,x$year),character(1))
          years<-vapply(periods,function(x) if(is.null(x)||isTRUE(x$conflict)) NA_integer_ else as.integer(x$year),integer(1))
          if(!anyNA(years)&&length(unique(years))==1L&&length(unique(signatures))>1L) {
            consensus<-list(start=NA_character_,end=NA_character_,year=years[1],precision="year",
              scope="project_year_consensus",evidence=paste(project_xml,collapse="; "))
            assign(project_xml[1],consensus,cache);project_xml<-project_xml[1]
          } else if(length(unique(signatures))!=1L||signatures[1]=="unresolved") {
            project_xml<-NA_character_
            assign(u,list(conflict=TRUE),cache)
          } else project_xml<-project_xml[1]
        }
        if(length(project_xml)==1L&&!is.na(project_xml)) {u<-project_xml;tiles$metadata_url[i]<-u}

        if(!exists(u,cache,inherits=FALSE)) assign(u,if(identical(u,"metadata_not_provided")) NULL else tryCatch(extract_als_dates_usgs(als_metadata_text(u)),error=function(e) structure(list(),class="metadata_failure")),cache)
        get(u,cache,inherits=FALSE)
      },error=function(e) structure(list(),class="metadata_failure"))
      if(inherits(result,"metadata_failure")) tiles$date_status[i]<-"lookup_failed"
      else if(isTRUE(result$conflict)) {
        tiles$date_status[i]<-"conflict";tiles$acquisition_year[i]<-NA_integer_
        tiles$acquired_start[i]<-NA_character_;tiles$acquired_end[i]<-NA_character_
      }
      else if(!is.null(result)) {
        tiles$acquired_start[i]<-result$start;tiles$acquired_end[i]<-result$end
        tiles$acquisition_year[i]<-result$year;tiles$date_precision[i]<-result$precision
        tiles$date_evidence[i]<-result$evidence;tiles$date_source[i]<-u
        tiles$date_status[i]<-"documented_acquisition";tiles$date_scope[i]<-if(is.null(result$scope)) "metadata_record" else result$scope
      } else tiles$date_status[i]<-"not_provided"
    }
    if(is.na(tiles$acquisition_year[i])&&tiles$date_status[i]!="conflict") {
      years<-unique(regmatches(tiles$filename[i],gregexpr("(?<![[:alnum:]])(?:19|20)[0-9]{2}(?![[:alnum:]])",tiles$filename[i],perl=TRUE))[[1]])
      if(length(years)==1L) {
        tiles$acquisition_year[i]<-as.integer(years);tiles$date_source[i]<-"filename_reference"
        # Preserve a failed lookup separately from an unverified year reference.
        if(tiles$date_status[i]!="lookup_failed") tiles$date_status[i]<-"filename_reference"
        tiles$date_precision[i]<-"year"; tiles$date_scope[i]<-"filename"
      } else if(length(years)>1L) tiles$date_status[i]<-"ambiguous"
    }
  }
  if(lookup) {tiles<-extract_als_dates_ahn(tiles);tiles<-extract_als_dates_swisstopo(tiles)}
  tiles
}

acquisition_year_label <- function(tiles) {
  tiles<-als_metadata_columns(tiles)
  fallback<-is.na(tiles$acquisition_year)&!is.na(tiles$acquired_end)
  if(any(fallback)) tiles$acquisition_year[fallback]<-suppressWarnings(as.integer(substr(tiles$acquired_end[fallback],1,4)))
  value<-ifelse(is.na(tiles$acquisition_year),"NA",as.character(tiles$acquisition_year))
  reference<-!is.na(tiles$date_source)&tiles$date_source=="filename_reference"
  value[reference]<-paste0(value[reference]," (filename reference)")
  value
}

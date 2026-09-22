http_asset_size <- function(status, headers, partial=FALSE) {
  h<-curl::parse_headers_list(headers)
  encoding<-h[["content-encoding"]]
  if(!is.null(encoding)&&!identical(tolower(encoding),"identity")) return(NA_real_)
  type<-h[["content-type"]]
  if(!is.null(type)&&grepl("html|json|xml",type,ignore.case=TRUE)) return(NA_real_)
  if(partial) {
    range<-h[["content-range"]]
    if(status!=206L||is.null(range)||!grepl("^bytes 0-0/[0-9]+$",range)) return(NA_real_)
    n<-as.numeric(sub(".*/","",range))
  } else {
    if(status!=200L) return(NA_real_)
    n<-suppressWarnings(as.numeric(h[["content-length"]]))
  }
  if(length(n)!=1L||!is.finite(n)||n<=0||n!=floor(n)) NA_real_ else n
}

#' Retrieve sizes of ALS files without downloading point clouds
#' @param tiles An ALS asset table.
#' @param timeout Maximum seconds per HTTP request.
#' @param budget Maximum seconds for each batch of requests.
#' @param connections Maximum simultaneous connections (1 to 8).
#' @return The asset table with size_bytes and size provenance/status. NA sizes
#'   are not zero. Requests use HEAD and, when necessary, a bounded byte range.
#' @export
get_als_file_sizes <- function(tiles, timeout=15, budget=60, connections=4L) {
  stopifnot(length(timeout)==1L,is.finite(timeout),timeout>0,length(budget)==1L,is.finite(budget),budget>0,
    length(connections)==1L,is.finite(connections),connections>=1,connections<=8)
  tiles<-als_metadata_columns(tiles)
  known<-is.finite(tiles$size_bytes)&tiles$size_bytes>0
  tiles$size_status[known]<-"available"
  tiles$size_source[known&is.na(tiles$size_source)]<-"provider_catalog"
  groups<-split(which(!known),asset_identity_url(tiles$url[!known]))
  run<-function(groups,partial=FALSE) {
    pool<-curl::new_pool(total_con=connections,host_con=min(2L,connections))
    handles<-list(); unresolved<-list()
    for(indices in groups) local({
      ids<-indices
      url<-tryCatch(asset_access_url(tiles[ids[1],,drop=FALSE]),error=function(e) NA_character_)
      if(is.na(url)) {tiles$size_status[ids]<<-"lookup_failed";return(NULL)}
      h<-curl::new_handle(url=url,followlocation=TRUE,timeout=timeout,connecttimeout=min(10,timeout),accept_encoding="identity")
      curl::handle_setopt(h,nobody=!partial)
      if(partial) curl::handle_setopt(h,range="0-0",maxfilesize=1)
      handles[[length(handles)+1L]]<<-h
      received<-0L
      curl::curl_fetch_multi(url,handle=h,pool=pool,
        data=function(chunk, ...) {received<<-received+length(chunk); received<=1L},
        done=function(response) {
          size<-http_asset_size(response$status_code,response$headers,partial)
          tiles$size_checked_at[ids]<<-format(Sys.time(),tz="UTC",usetz=TRUE)
          if(is.finite(size)) {
            tiles$size_bytes[ids]<<-size;tiles$size_status[ids]<<-"available"
            tiles$size_source[ids]<<-if(partial) "HTTP Content-Range" else "HTTP Content-Length"
          } else {tiles$size_status[ids]<<-"not_provided";unresolved[[length(unresolved)+1L]]<<-ids}
        },fail=function(message) {
          tiles$size_status[ids]<<-"lookup_failed";unresolved[[length(unresolved)+1L]]<<-ids
        })
    })
    curl::multi_run(timeout=budget,pool=pool)
    for(h in handles) try(curl::multi_cancel(h),silent=TRUE)
    unresolved
  }
  missing<-run(groups)
  if(length(missing)) run(missing,TRUE)
  tiles
}

#' Summarize the total download size of a selection
#' @param tiles An ALS asset table, optionally enriched with get_als_file_sizes().
#' @return A one-row data frame with file count, known bytes, missing sizes and
#'   complete total bytes (NA when any size is missing). GB uses 10^9 bytes.
#' @export
summarize_als_download <- function(tiles) {
  tiles<-tiles[!duplicated(asset_identity_url(tiles$url)),,drop=FALSE]
  known<-is.finite(tiles$size_bytes)&tiles$size_bytes>0
  total<-sum(tiles$size_bytes[known])
  data.frame(files=nrow(tiles),known_bytes=total,missing_sizes=sum(!known),
    complete=all(known),total_bytes=if(all(known)) total else NA_real_)
}

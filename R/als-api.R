#' Find airborne laser scanning data
#' @inheritParams find_tiles
#' @return An sf asset table including acquisition evidence and file sizes when
#'   retrievable. The download summary is available with summarize_als_download().
#' @export
find_als_data <- function(aoi, provider=c("usgs3dep","planetary","opentopography","contributed","ahn6","swisstopo","ignfr","canelevation"),
                          start=NULL,end=NULL,tile_index_dir=NULL,max_items=10000L) {
  als_metadata_columns(tile_campaign_metadata(find_tiles(aoi,match.arg(provider),start,end,tile_index_dir,max_items)))
}

#' Download selected airborne laser scanning files
#' @param ... Arguments passed to download_tiles().
#' @return A transfer status table, as returned by download_tiles().
#' @export
download_als_data <- function(...) download_tiles(...)

#' Create an ALS download planning report
#' @param ... Arguments passed to als_report().
#' @return The generated report path, invisibly.
#' @export
create_als_report <- function(...) als_report(...)

#' Summarize an ALS data selection
#' @param tiles An ALS asset table.
#' @return Counts and total download size; incomplete totals remain NA.
#' @export
summarize_als_data <- function(tiles) summarize_als_download(tiles)

#' Make a bounded point-cloud preview
#' @param points A data frame or matrix with numeric `X`, `Y`, `Z` columns.
#' @param max_points Maximum displayed points, between one and one million.
#' @return A data frame of sampled coordinates with an `origin` attribute.
#'   Coordinates are translated to a local origin for display precision.
#' @details Sampling is deterministic and affects only the returned preview.
#'   The source cloud is not modified. Elevation is not normalized canopy height.
#' @export
#' @examples
#' preview_points(data.frame(X = 1:10, Y = 1:10, Z = 11:20), 5)
preview_points <- function(points, max_points = 100000L) {
  points <- as.data.frame(points)
  if (!all(c("X", "Y", "Z") %in% names(points)) ||
      !all(vapply(points[c("X", "Y", "Z")], is.numeric, logical(1))))
    stop("points must contain numeric X, Y and Z columns.", call. = FALSE)
  if (length(max_points) != 1L || !is.finite(max_points) || max_points < 1 ||
      max_points > 1e6 || max_points != floor(max_points)) stop("Invalid preview point budget.", call. = FALSE)
  points <- points[is.finite(points$X) & is.finite(points$Y) & is.finite(points$Z), c("X", "Y", "Z")]
  if (!nrow(points)) stop("No finite points to preview.", call. = FALSE)
  idx <- unique(round(seq(1, nrow(points), length.out = min(nrow(points), max_points))))
  points <- points[idx, , drop = FALSE]
  origin <- vapply(points, min, numeric(1))
  points[] <- Map(function(x, offset) x - offset, points, origin)
  attr(points, "origin") <- origin
  points
}

#' Read a small preview from a local LAS or LAZ file
#' @param path Local point-cloud filename.
#' @param max_points Maximum preview size, between one and one million.
#' @return The same translated coordinate table as [preview_points()].
#' @details Requires the optional package 'lidR'. Reader-side decimation limits
#'   retained points before they enter R. The file may still need to be scanned
#'   in full; this function is not a remote COPC range reader.
#' @export
#' @examples
#' if (interactive() && requireNamespace("lidR", quietly = TRUE)) {
#'   # p <- read_preview("tile.laz", max_points = 50000)
#' }
read_preview <- function(path, max_points = 100000L) {
  if (!requireNamespace("lidR", quietly = TRUE)) stop("Install lidR to preview LAS/LAZ files.", call. = FALSE)
  if (length(max_points) != 1L || !is.finite(max_points) || max_points < 1 || max_points > 1e6 || max_points != floor(max_points))
    stop("Invalid preview point budget.", call. = FALSE)
  if (length(path) != 1L || !file.exists(path)) stop("Point-cloud file not found.", call. = FALSE)
  header <- lidR::readLASheader(path)
  n <- header@PHB[["Number of point records"]]
  if (!is.null(header@PHB[["Extended Number of point records"]]) && header@PHB[["Extended Number of point records"]] > 0)
    n <- header@PHB[["Extended Number of point records"]]
  if (is.null(n) || !is.finite(n) || n < 1) stop("Point count is missing from LAS header.")
  every <- max(1, ceiling(n / max_points))
  las <- lidR::readLAS(path, select = "xyz", filter = paste("-keep_every_nth", every))
  if (is.null(las)) stop("Reader returned no preview points.")
  preview_points(las@data, max_points)
}

# Temporary, single-file preview; never persists a cloud in the source catalog.
preview_remote_tile <- function(tile, max_bytes = 200 * 1024^2, path = tempfile(fileext = ".laz")) {
  if (!requireNamespace("lidR", quietly = TRUE)) stop("Install lidR to preview tiles.")
  url <- tile$url[[1]]
  if (!grepl("^https://", url)) stop("Only HTTPS tile URLs are supported.")
  if (tile$provider[[1]] == "usgs3dep")
    url <- request_json("https://planetarycomputer.microsoft.com/api/sas/v1/sign", query = list(href = redact_url(url)))$href
  head <- httr::HEAD(url, httr::timeout(30))
  httr::stop_for_status(head)
  size <- suppressWarnings(as.numeric(httr::headers(head)[["content-length"]]))
  if (length(size) != 1L || !is.finite(size) || size <= 0 || size > max_bytes)
    stop("Preview requires a known file size up to 200 MB. Download locally and use the upload preview for other files.")
  on.exit(unlink(path), add = TRUE)
  response <- httr::GET(url, httr::write_disk(path), httr::timeout(180), httr::config(maxfilesize_large = max_bytes))
  httr::stop_for_status(response)
  if (httr::status_code(response) != 200L || file.size(path) != size || !valid_las_header(path))
    stop("Incomplete or invalid LAS/LAZ preview download.")
  read_preview(path, 100000L)
}

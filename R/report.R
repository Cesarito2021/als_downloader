#' Generate an HTML or PDF summary of a search/download selection
#' @param tiles An `sf` or data frame as returned by [find_tiles()] or
#'   [download_tiles()]. Only used to summarise the selection; no network
#'   request is made and no source file is read.
#' @param output_dir Directory to write the report into. Created if it does
#'   not already exist.
#' @param format `"html"` (default; always available, self-contained, no
#'   extra install required) or `"pdf"` (requires a working `tinytex`
#'   installation; falls back to `"html"` with a warning otherwise).
#' @param aoi_area_km2 Optional study-area size in square kilometres, shown
#'   for context only.
#' @return Invisibly, the path to the rendered report file.
#' @details Content is limited to data already carried by `tiles`: filename,
#'   dataset, provider, provider-reported acquisition dates, known size and
#'   the recorded citation/licence per tile. Nothing is inferred, estimated
#'   or fetched. The report is written only to `output_dir` and is never
#'   transmitted anywhere.
#' @export
#' @examples
#' if (interactive()) {
#'   # als_report(tiles, "session-report-out")
#' }
als_report <- function(tiles, output_dir, format = c("html", "pdf"), aoi_area_km2 = NA_real_) {
  format <- match.arg(format)
  if (!requireNamespace("rmarkdown", quietly = TRUE))
    stop("Install rmarkdown to generate a session report.", call. = FALSE)
  if (inherits(tiles, "sf")) tiles <- sf::st_drop_geometry(tiles)
  if (!is.data.frame(tiles)) stop("tiles must be a data frame returned by find_tiles().", call. = FALSE)
  if (length(output_dir) != 1L || !is.character(output_dir) || !nzchar(output_dir))
    stop("Choose an output directory.", call. = FALSE)
  if (format == "pdf" && !(requireNamespace("tinytex", quietly = TRUE) && isTRUE(tinytex::is_tinytex()))) {
    warning("PDF requires a working tinytex installation; writing an HTML report instead.", call. = FALSE)
    format <- "html"
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(output_dir)) stop("Output directory cannot be created.", call. = FALSE)
  rmd <- system.file("report", "session-report.Rmd", package = "alsdownloader")
  if (!nzchar(rmd)) stop("Report template not found in the installed package.", call. = FALSE)
  output_file <- paste0("als-session-report.", format)
  output_format <- if (format == "pdf") rmarkdown::pdf_document() else rmarkdown::html_document(self_contained = TRUE)
  rmarkdown::render(rmd, output_format = output_format, output_file = output_file,
    output_dir = output_dir, intermediates_dir = tempdir(),
    params = list(tiles = tiles, aoi_area_km2 = aoi_area_km2),
    envir = new.env(parent = globalenv()), quiet = TRUE)
  invisible(file.path(output_dir, output_file))
}

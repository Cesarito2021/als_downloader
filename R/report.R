#' Generate an HTML or PDF summary of a search/download selection
#' @param tiles An `sf` or data frame as returned by [find_tiles()] or
#'   [download_tiles()]. Only used to summarise the selection; no network
#'   request is made and no source file is read.
#' @param output_dir Directory to write the report into. Created if it does
#'   not already exist.
#' @param format `"html"` (default; self-contained, requires Pandoc) or
#'   `"pdf"` (requires Pandoc and a working `tinytex`
#'   installation; falls back to `"html"` with a warning otherwise).
#' @param aoi_area_km2 Optional study-area size in square kilometres, shown
#'   for context only.
#' @param aoi Optional `sf` polygon (the study area) as passed to
#'   [find_tiles()]. When supplied alongside `tiles` with geometry, the
#'   report includes a simple map figure of the tile footprints and the
#'   study area outline. Geometry only -- not a basemap image, and nothing
#'   is fetched to draw it.
#' @param figures Optional paths to up to six exported PNG figures, included
#'   unchanged with their embedded legends and credits. Maximum 10 MiB each.
#' @param details Include a technical appendix with transfer-time scenarios
#'   and a sample file table. Defaults to `FALSE` for a concise visual report.
#' @return Invisibly, the path to the rendered report file.
#' @details Content is limited to data already carried by `tiles`: filename,
#'   dataset, provider, provider-reported acquisition dates, known size and
#'   the recorded citation/licence per tile. Transfer-time scenarios use
#'   known bytes and illustrative aggregate speeds, not CPU core counts.
#'   Unknown file sizes are explicitly excluded. Nothing is fetched.
#'   The report is written only to `output_dir` and is never
#'   transmitted anywhere.
#' @export
#' @examples
#' if (interactive()) {
#'   # als_report(tiles, "session-report-out")
#' }
als_report <- function(tiles, output_dir, format = c("html", "pdf"), aoi_area_km2 = NA_real_, aoi = NULL,
                       figures = character(), details = FALSE) {
  format <- match.arg(format)
  if (!is.logical(details) || length(details) != 1L || is.na(details))
    stop("details must be TRUE or FALSE.", call. = FALSE)
  if (!requireNamespace("rmarkdown", quietly = TRUE))
    stop("Install rmarkdown to generate a session report.", call. = FALSE)
  if (!is.data.frame(tiles)) stop("tiles must be a data frame returned by find_tiles().", call. = FALSE)
  if (!is.null(aoi) && !inherits(aoi, "sf")) stop("aoi must be an sf polygon, as passed to find_tiles().", call. = FALSE)
  if (length(output_dir) != 1L || !is.character(output_dir) || !nzchar(output_dir))
    stop("Choose an output directory.", call. = FALSE)
  if (!is.character(figures) || length(figures) > 6L || anyNA(figures))
    stop("Supply up to six PNG figure paths.", call. = FALSE)
  for (p in figures) {
    if (!file.exists(p) || isTRUE(file.info(p)$isdir) || file.info(p)$size > 10 * 1024^2 ||
        !identical(readBin(p, "raw", n = 8L), as.raw(c(137, 80, 78, 71, 13, 10, 26, 10))))
      stop("Report figures must be PNG files of at most 10 MiB each.", call. = FALSE)
  }
  if (format == "pdf" && !(requireNamespace("tinytex", quietly = TRUE) && isTRUE(tinytex::is_tinytex()))) {
    warning("PDF requires a working tinytex installation; writing an HTML report instead.", call. = FALSE)
    format <- "html"
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(output_dir)) stop("Output directory cannot be created.", call. = FALSE)
  stage <- tempfile("als-report-figures-"); dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  if (length(figures)) {
    staged <- file.path(stage, paste0("figure-", seq_along(figures), ".png"))
    if (!all(file.copy(figures, staged))) stop("Could not prepare report figures.", call. = FALSE)
    figures <- staged
  }
  rmd <- system.file("report", "session-report.Rmd", package = "alsdownloader")
  if (!nzchar(rmd)) stop("Report template not found in the installed package.", call. = FALSE)
  output_file <- paste0("als-session-report.", format)
  output_format <- if (format == "pdf") rmarkdown::pdf_document() else rmarkdown::html_document(self_contained = TRUE)
  rmarkdown::render(rmd, output_format = output_format, output_file = output_file,
    output_dir = output_dir, intermediates_dir = tempdir(),
    params = list(tiles = tiles, aoi_area_km2 = aoi_area_km2, aoi = aoi,
      figures = normalizePath(figures, winslash = "/", mustWork = TRUE), details = details,
      software_citation = paste(format(utils::readCitationFile(system.file("CITATION", package = "alsdownloader")), style = "text"), collapse = " ")),
    envir = new.env(parent = globalenv()), quiet = TRUE)
  invisible(file.path(output_dir, output_file))
}

report_pandoc <- function() {
  if(rmarkdown::pandoc_available())return(invisible(TRUE))
  # RStudio bundles Pandoc, but an app launched outside RStudio may not inherit
  # RSTUDIO_PANDOC. Discover that existing installation without downloading one.
  candidates<-c(Sys.getenv("RSTUDIO_PANDOC"),
    file.path(Sys.getenv("ProgramFiles"),"RStudio/resources/app/bin/quarto/bin/tools"),
    file.path(Sys.getenv("ProgramFiles"),"RStudio/bin/pandoc"))
  for(path in candidates[nzchar(candidates)]) {
    executable<-file.path(path,if(.Platform$OS.type=="windows")"pandoc.exe" else "pandoc")
    if(file.exists(executable)) {
      rmarkdown::find_pandoc(cache=FALSE,dir=path)
      if(rmarkdown::pandoc_available())return(invisible(TRUE))
    }
  }
  stop("Reports require Pandoc. Launch the app from RStudio or install Pandoc.",call.=FALSE)
}

#' Generate an HTML or PDF summary of a search/download selection
#' @param tiles An `sf` or data frame as returned by [find_tiles()] or
#'   [download_tiles()]. Only used to summarise the selection; no network
#'   request is made and no source file is read.
#' @param output_dir Directory to write the report into. Created if it does
#'   not already exist.
#' @param format `"html"` (default; self-contained, requires Pandoc) or
#'   `"pdf"` (requires Pandoc and a working `tinytex`
#'   installation; falls back to `"html"` with a warning otherwise).
#' @param aoi_area_km2 Optional AOI size in square kilometres, shown
#'   for context only.
#' @param aoi Optional `sf` polygon (the AOI) as passed to
#'   [find_tiles()]. When supplied alongside `tiles` with geometry, the
#'   report includes a simple map figure of the tile footprints and the
#'   AOI outline. Geometry only -- not a basemap image, and nothing
#'   is fetched to draw it.
#' @param figures Optional paths to up to six exported PNG figures, included
#'   unchanged with their embedded legends and credits. Maximum 10 MiB each.
#' @param details Include a technical appendix with transfer-time scenarios
#'   and a sample file table. Defaults to `FALSE` for a concise visual report.
#' @param map_image Optional single PNG path, used instead of the geometry-only
#'   map. The app captures a centred basemap with AOI and selected tiles.
#' @param map_credits Basemap attribution accompanying `map_image`.
#' @param figure_captions Optional short captions, one per attached figure.
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
                       figures = character(), details = FALSE, map_image = character(), map_credits = "",
                       figure_captions = NULL) {
  format <- match.arg(format)
  if (!is.logical(details) || length(details) != 1L || is.na(details))
    stop("details must be TRUE or FALSE.", call. = FALSE)
  if (!requireNamespace("rmarkdown", quietly = TRUE))
    stop("Install rmarkdown to generate a session report.", call. = FALSE)
  report_pandoc()
  if (!is.data.frame(tiles)) stop("tiles must be a data frame returned by find_tiles().", call. = FALSE)
  if ("url" %in% names(tiles)) tiles <- tiles[!duplicated(asset_identity_url(tiles$url)),,drop=FALSE]
  if (!is.null(aoi) && !inherits(aoi, "sf")) stop("aoi must be an sf polygon, as passed to find_tiles().", call. = FALSE)
  if (length(output_dir) != 1L || !is.character(output_dir) || !nzchar(output_dir))
    stop("Choose an output directory.", call. = FALSE)
  if (!is.character(figures) || length(figures) > 6L || anyNA(figures))
    stop("Supply up to six PNG figure paths.", call. = FALSE)
  if (is.null(figure_captions)) figure_captions <- vapply(figures, report_figure_caption, character(1))
  if (!is.character(figure_captions) || length(figure_captions) != length(figures) ||
      anyNA(figure_captions) || any(!nzchar(trimws(figure_captions))))
    stop("Supply one nonempty caption per figure.", call. = FALSE)
  if (!is.character(map_image) || length(map_image) > 1L || anyNA(map_image) ||
      !is.character(map_credits) || length(map_credits) != 1L || is.na(map_credits))
    stop("Supply one map PNG path and its attribution text.", call. = FALSE)
  for (p in c(figures, map_image)) {
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
  if (length(map_image)) {
    staged_map <- file.path(stage, "aoi-rgb.png")
    if (!file.copy(map_image, staged_map)) stop("Could not prepare the report map.", call. = FALSE)
    map_image <- normalizePath(staged_map, winslash = "/", mustWork = TRUE)
  }
  rmd <- system.file("report", "session-report.Rmd", package = "ALSdownloadeR")
  if (!nzchar(rmd)) stop("Report template not found in the installed package.", call. = FALSE)
  output_file <- paste0("als-session-report.", format)
  output_format <- if (format == "pdf") rmarkdown::pdf_document() else rmarkdown::html_document(self_contained = TRUE)
  rmarkdown::render(rmd, output_format = output_format, output_file = output_file,
    output_dir = output_dir, intermediates_dir = tempdir(),
    params = list(tiles = tiles, aoi_area_km2 = aoi_area_km2, aoi = aoi,
      figures = normalizePath(figures, winslash = "/", mustWork = TRUE), details = details,
      figure_captions = figure_captions,
      map_image = map_image, map_credits = map_credits,
      software_citation = paste(format(utils::readCitationFile(system.file("CITATION", package = "ALSdownloadeR")), style = "text"), collapse = " ")),
    envir = new.env(parent = globalenv()), quiet = TRUE)
  invisible(file.path(output_dir, output_file))
}

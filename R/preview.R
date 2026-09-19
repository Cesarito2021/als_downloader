#' Make a bounded point-cloud preview
#' @param points A data frame or matrix with numeric `X`, `Y`, `Z` columns.
#' @param max_points Maximum displayed points, between one and one million.
#' @return A data frame of sampled coordinates with an `origin` attribute.
#'   Coordinates are translated to a local origin for display precision.
#'   An optional numeric `Classification` column is retained as an aligned
#'   `classification` attribute; missing or invalid codes become `NA`.
#'   Optional numeric `Intensity` is retained as an aligned `intensity`
#'   attribute. No classification or intensity is inferred.
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
  finite <- is.finite(points$X) & is.finite(points$Y) & is.finite(points$Z)
  classification <- if (is.numeric(points$Classification)) points$Classification[finite] else NULL
  intensity <- if (is.numeric(points$Intensity)) points$Intensity[finite] else NULL
  points <- points[finite, c("X", "Y", "Z")]
  if (!nrow(points)) stop("No finite points to preview.", call. = FALSE)
  idx <- unique(round(seq(1, nrow(points), length.out = min(nrow(points), max_points))))
  points <- points[idx, , drop = FALSE]
  origin <- vapply(points, min, numeric(1))
  points[] <- Map(function(x, offset) x - offset, points, origin)
  attr(points, "origin") <- origin
  if (!is.null(classification)) {
    classification <- classification[idx]
    classification[!is.finite(classification) | classification < 0 | classification > 255 |
      classification != floor(classification)] <- NA_real_
    attr(points, "classification") <- as.integer(classification)
  }
  if (!is.null(intensity)) {
    intensity <- intensity[idx]
    intensity[!is.finite(intensity) | intensity < 0 | intensity > 65535] <- NA_real_
    attr(points, "intensity") <- intensity
  }
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
  las <- lidR::readLAS(path, select = "xyzci", filter = paste("-keep_every_nth", every))
  if (is.null(las)) stop("Reader returned no preview points.")
  preview_points(las@data, max_points)
}

#' Download one remote tile temporarily and read a bounded preview
#' @param tile A single-row tile data frame, as returned by [find_tiles()].
#' @param max_bytes Maximum known HTTP file size accepted for the temporary
#'   download, in bytes.
#' @param path Temporary local path for the downloaded file. Removed after
#'   `reader` runs or on error.
#' @param reader Function called with `path` once the download completes;
#'   defaults to [read_preview()]. Used by [read_forest_preview()] for the
#'   app's forest close-up preview.
#' @param progress Optional function receiving short preview-stage messages.
#' @return Whatever `reader` returns.
#' @details A temporary, single-file preview; never persists a cloud in the
#'   source catalog. Exported (rather than internal-only) so a background
#'   [callr::r_bg()] worker can call it by namespace-qualified name -- a
#'   plain closure passed as a callr argument can lose access to sibling
#'   package-internal helpers it calls, such as [require_data_terms()].
#' @export
#' @examples
#' if (interactive()) {
#'   # preview_remote_tile(tile_row)
#' }
preview_remote_tile <- function(tile, max_bytes = 1024 * 1024^2, path = tempfile(fileext = ".laz"), reader = function(path) read_preview(path, 100000L), progress = NULL) {
  stage <- function(message) if (is.function(progress)) progress(message)
  stage("Checking source access...")
  require_data_terms(tile)
  if (grepl("\\.zip$",tile$filename[[1]],ignore.case=TRUE))
    stop("This source delivers an original LAS ZIP. Download and extract it locally, then open the LAS for preview.")
  if (!requireNamespace("lidR", quietly = TRUE)) stop("Install lidR to preview tiles.")
  url <- tile$url[[1]]
  if (!grepl("^https://", url)) stop("Only HTTPS tile URLs are supported.")
  if (tile$provider[[1]] == "usgs3dep")
    url <- request_json("https://planetarycomputer.microsoft.com/api/sas/v1/sign", query = list(href = redact_url(url)))$href
  stage("Checking source file size...")
  head <- httr::HEAD(url, httr::timeout(30))
  httr::stop_for_status(head)
  size <- suppressWarnings(as.numeric(httr::headers(head)[["content-length"]]))
  if (length(size) != 1L || !is.finite(size) || size <= 0 || size > max_bytes)
    stop(paste("Preview requires a known file size up to", round(max_bytes / 1024^2), "MB. Use a smaller source tile."))
  on.exit(unlink(path), add = TRUE)
  stage(sprintf("Downloading %.1f MiB from the provider...", size / 1024^2))
  response <- httr::GET(url, httr::write_disk(path), httr::timeout(600), httr::config(maxfilesize_large = max_bytes))
  httr::stop_for_status(response)
  if (httr::status_code(response) != 200L || file.size(path) != size || !valid_las_header(path))
    stop("Incomplete or invalid LAS/LAZ preview download.")
  stage("Reading and sampling the downloaded cloud. Large files may take several minutes...")
  reader(path)
}

# Display-only sampling inspired by the supplied biomass viewer. Source files and
# comparison-analysis points are never rewritten or filtered by these controls.
forest_display_sample <- function(points, window = 100, center_x = 50, center_y = 50,
                                  voxel = 0, max_points = 150000L) {
  p <- as.data.frame(points)
  p <- p[intersect(c("X", "Y", "Z", "Classification", "Intensity"), names(p))]
  p <- p[is.finite(p$X) & is.finite(p$Y) & is.finite(p$Z), , drop = FALSE]
  if (!nrow(p)) stop("No finite display points.")
  if (!is.finite(window) || window < 5 || window > 100 ||
      any(!is.finite(c(center_x, center_y))) || any(c(center_x, center_y) < 0 | c(center_x, center_y) > 100))
    stop("Invalid display window or center.")
  if (!is.finite(voxel) || voxel < 0) stop("Invalid display voxel size.")
  if (window < 100) {
    axis_bounds <- function(x, center) {
      extent <- range(x); width <- diff(extent) * window / 100
      low <- min(max(extent[1], extent[1] + diff(extent) * center / 100 - width / 2), extent[2] - width)
      c(low, low + width)
    }
    xb <- axis_bounds(p$X, center_x); yb <- axis_bounds(p$Y, center_y)
    p <- p[p$X >= xb[1] & p$X <= xb[2] & p$Y >= yb[1] & p$Y <= yb[2], , drop = FALSE]
  }
  if (!nrow(p)) stop("The display window has no sampled points. Move its center or increase the reader percentage.")
  if (voxel > 0) {
    key <- paste(floor(p$X / voxel), floor(p$Y / voxel), floor(p$Z / voxel), sep = ":")
    # A real return per occupied voxel; coordinates are never averaged or shifted.
    p <- p[!duplicated(key), , drop = FALSE]
  }
  preview_points(p, max_points)
}

#' Read a decimated, spatially windowed preview for the forest close-up viewer
#' @param path Local LAS/LAZ file path.
#' @param percent Approximate percentage of source points the reader keeps,
#'   between 0.1 and 100. The reader pool is also capped at 750,000 points.
#' @param window,center_x,center_y,voxel Display windowing and optional
#'   voxel thinning; see [forest_display_sample()].
#' @return The same translated coordinate table as [preview_points()], with
#'   a `display_note` attribute describing the sampling actually used.
#' @details Requires the optional package 'lidR'. Exported so a background
#'   [callr::r_bg()] worker can call it by namespace-qualified name; see
#'   [preview_remote_tile()] for why.
#' @export
#' @examples
#' if (interactive() && requireNamespace("lidR", quietly = TRUE)) {
#'   # read_forest_preview("tile.laz", percent = 5)
#' }
read_forest_preview <- function(path, percent = 2, window = 100, center_x = 50, center_y = 50,
                                voxel = 0) {
  if (!requireNamespace("lidR", quietly = TRUE)) stop("Install lidR to preview LAS/LAZ files.")
  if (!is.finite(percent) || percent < .1 || percent > 100) stop("Choose a display percentage between 0.1 and 100.")
  h <- lidR::readLASheader(path)
  n <- h@PHB[["Extended Number of point records"]]
  if (is.null(n) || n == 0) n <- h@PHB[["Number of point records"]]
  if (is.null(n) || !is.finite(n) || n < 1) stop("Missing LAS point count.")
  every <- max(1, ceiling(100 / percent), ceiling(n / 750000))
  las <- lidR::readLAS(path, select = "xyzci", filter = paste("-keep_every_nth", every))
  if (is.null(las) || !nrow(las@data)) stop("No points read for display.")
  out <- forest_display_sample(las@data, window, center_x, center_y, voxel)
  attr(out, "display_note") <- sprintf("%s source points; reader retains about %.2f%% (bounded pool); XY window %.0f%% of each axis; %s. No denoising or height normalization.",
    format(n, scientific = FALSE, trim = TRUE), 100 / every, window,
    if (voxel > 0) paste("one sampled return per", voxel, "source-unit voxel") else "no voxel thinning")
  out
}

forest_preview_controls <- function(prefix) {
  shiny::tags$details(shiny::tags$summary("Forest close-up and display sampling"),
    shiny::helpText("Choose a small wooded window to see crown silhouettes. These controls affect the preview only; no noise removal, normalization or changes to downloaded files. Rebuild the preview after changes."),
    shiny::numericInput(paste0(prefix, "percent"), "Reader sampling target (%)", 2, min = .1, max = 100, step = .5),
    shiny::selectInput(paste0(prefix, "window"), "XY display window", c("Full tile" = 100, "Close-up: 25% of each axis" = 25, "Detail: 10% of each axis" = 10), selected = 100),
    shiny::sliderInput(paste0(prefix, "center_x"), "Window center X (%)", 0, 100, 50),
    shiny::sliderInput(paste0(prefix, "center_y"), "Window center Y (%)", 0, 100, 50),
    shiny::selectInput(paste0(prefix, "voxel"), "Optional spatial thinning (source coordinate units)", c("None" = 0, "0.5-unit voxels" = .5, "1-unit voxels" = 1, "2-unit voxels" = 2)),
    shiny::selectInput(paste0(prefix, "pose"), "Camera", c("Top-down" = "top", "Forest silhouette" = "forest", "Oblique overview" = "overview"), selected = "top"),
    shiny::sliderInput(paste0(prefix, "point_size"), "Display point size", .7, 3, 1.5, step = .1),
    shiny::helpText("Reader pool: at most 750,000 points; display: at most 150,000. Very large files may retain less than the requested percentage. This is not automatic forest detection."))
}

preview_palettes <- function() c("Greyscale", "Viridis", "Magma", "Plasma", "Cividis",
  "Grey", "Black", "Light purple", "Pale yellow", "Blue", "Red", "Cyan", "Orange")

figure_button <- function(id, label, disabled = FALSE) {
  shiny::tags$button(id = id, type = "button", class = "btn btn-default als-camera-button",
    disabled = if (disabled) NA else NULL, title = label, `aria-label` = label,
    shiny::icon("camera"), label)
}

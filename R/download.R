redact_url <- function(x) sub("\\?.*$", "", x)

asset_path <- function(tile, output_dir) {
  key <- digest::digest(paste(tile$provider, tile$tile_id, redact_url(tile$url)), algo = "sha256", serialize = FALSE)
  filename <- substr(gsub("[^A-Za-z0-9._-]", "_", basename(tile$filename)), 1, 110)
  file.path(output_dir, paste0(substr(key, 1, 16), "_", filename))
}

valid_las_header <- function(path) {
  if (!file.exists(path) || is.na(file.size(path)) || file.size(path) < 227) return(FALSE)
  con <- file(path, "rb"); on.exit(close(con))
  identical(readBin(con, "raw", n = 4), charToRaw("LASF"))
}

fetch_asset <- function(url, part, timeout) {
  httr::GET(url, httr::write_disk(part, overwrite = TRUE), httr::timeout(timeout))
}

transfer_tile <- function(tile, output_dir, retries, timeout, progress_dir = NULL) {
  dest <- asset_path(tile, output_dir)
  record <- paste0(dest, ".rds")
  part <- paste0(dest, ".part")
  result <- list(tile_id = tile$tile_id, provider = tile$provider, dataset = tile$dataset,
    filename = basename(dest), source_url = redact_url(tile$url), status = "failed",
    bytes = NA_real_, checksum = NA_character_, message = "", path = dest)
  publish <- function(x) {
    if (!is.null(progress_dir)) saveRDS(x, file.path(progress_dir, paste0(basename(dest), ".rds")))
    as.data.frame(x, stringsAsFactors = FALSE)
  }
  if (file.exists(record) && valid_las_header(dest)) {
    previous <- tryCatch(readRDS(record), error = function(e) NULL)
    if (!is.null(previous) && identical(previous$source_url, result$source_url) &&
        identical(previous$checksum, unname(tools::md5sum(dest)))) {
      previous$status <- "verified_existing"
      return(publish(previous))
    }
  }
  # Do not overwrite an untracked file that happens to share the destination.
  if (file.exists(dest) && !file.exists(record)) {
    result$message <- "Untracked destination already exists; choose another output directory."
    return(publish(result))
  }
  for (attempt in seq_len(retries + 1L)) {
    ok <- tryCatch({
      url <- tile$url
      if (!grepl("^https://", url)) stop("Only HTTPS asset URLs are supported.")
      if (identical(tile$provider, "usgs3dep")) {
        signed <- request_json("https://planetarycomputer.microsoft.com/api/sas/v1/sign",
                               query = list(href = redact_url(url)))
        if (is.null(signed$href)) stop("Asset signing failed.")
        url <- signed$href
      }
      response <- fetch_asset(url, part, timeout)
      status <- httr::status_code(response)
      if (status != 200L) {
        # Leave throttled and forbidden jobs for an explicit later retry.
        if (status %in% c(401L, 403L, 429L)) {
          result$message <- paste("Access or rate limit: HTTP", status, "- retry later after checking provider access.")
          if (file.exists(part)) unlink(part)
          return(publish(result))
        }
        stop(paste("Transfer returned HTTP", status))
      }
      expected <- suppressWarnings(as.numeric(httr::headers(response)[["content-length"]]))
      if (length(expected) && is.finite(expected) && file.size(part) != expected)
        stop("Downloaded byte count does not match Content-Length.")
      if (is.finite(tile$size_bytes) && file.size(part) != tile$size_bytes)
        stop("Downloaded size does not match the catalog.")
      if (!valid_las_header(part)) stop("Downloaded file lacks a valid LAS/LAZ signature and minimum header size.")
      if (file.exists(dest)) unlink(dest)
      if (!file.rename(part, dest)) stop("Could not finalize the downloaded file.")
      result$status <- "downloaded"
      result$bytes <- as.numeric(file.size(dest))
      result$checksum <- unname(tools::md5sum(dest))
      result$message <- "Transport size/header verified; full point decoding not performed."
      saveRDS(result, record)
      TRUE
    }, error = function(e) {
      result$message <<- "Transfer failed validation or connectivity checks; retry the tile."
      FALSE
    })
    if (isTRUE(ok)) break
    if (attempt <= retries) Sys.sleep(min(2^attempt, 15))
  }
  if (file.exists(part)) unlink(part)
  publish(result)
}

#' Download selected tiles with resumable transfer records
#' @param tiles An `sf` or data frame returned by [find_tiles()].
#' @param output_dir Explicit writable destination directory. A directory is
#'   created only when this function is called.
#' @param workers Requested transfer workers. Defaults to one for scripted use.
#' @param mode `"local"` or `"hosted"`. Hosted forces serial transfers.
#' @param provider_limit Provider concurrency ceiling; defaults to two until
#'   an operator has verified the provider's applicable terms.
#' @param retries Number of retries for transient failures. Authentication and
#'   rate-limit responses are not retried automatically.
#' @param timeout Per-transfer timeout in seconds.
#' @param progress_dir Optional directory for per-tile progress records.
#' @return A data frame with one row per asset, transfer status, canonical source
#'   URL, byte count, checksum and local path. Failed tiles remain in the result.
#' @details Uses `future.apply::future_lapply()` and restores the caller's future
#'   plan. Resume requires both a completed sidecar record and a matching file
#'   checksum. HTTP status, reported length and LAS signature are checked;
#'   complete point-stream decompression is not verified. Existing untracked
#'   destination files are never overwritten. Credentials are not written to
#'   the exported manifest. Large datasets should be downloaded locally.
#' @export
#' @examples
#' if (interactive()) {
#'   # download_tiles(tiles, output_dir = "selected-tiles", workers = 2)
#' }
download_tiles <- function(tiles, output_dir, workers = 1L, mode = c("local", "hosted"),
                           provider_limit = 2L, retries = 2L, timeout = 600,
                           progress_dir = NULL) {
  mode <- match.arg(mode)
  if (inherits(tiles, "sf")) tiles <- sf::st_drop_geometry(tiles)
  required <- c("tile_id", "provider", "dataset", "filename", "url", "size_bytes", "citation", "license_url")
  if (!is.data.frame(tiles) || !all(required %in% names(tiles)))
    stop("tiles must be an asset table returned by find_tiles().", call. = FALSE)
  if (!nrow(tiles)) stop("No tiles selected.", call. = FALSE)
  if (anyNA(tiles$url) || any(!grepl("^https://", tiles$url))) stop("Invalid HTTPS asset URL.", call. = FALSE)
  if (length(retries) != 1 || !is.finite(retries) || retries < 0 || retries != floor(retries)) stop("retries must be a nonnegative integer.")
  if (length(timeout) != 1 || !is.finite(timeout) || timeout <= 0) stop("timeout must be positive.")
  if (length(output_dir) != 1L || !is.character(output_dir) || !nzchar(output_dir)) stop("Choose an output directory.")
  tiles <- tiles[!duplicated(paste(tiles$provider, tiles$tile_id, tiles$url)), , drop = FALSE]
  policy <- download_worker_policy(mode, as.numeric(parallelly::availableCores()), workers, provider_limit, nrow(tiles))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(output_dir)) stop("Output directory cannot be created.")
  output_dir <- normalizePath(output_dir, winslash = "/")
  lock <- file.path(output_dir, ".als-transfer-lock")
  if (!dir.create(lock, showWarnings = FALSE)) stop("This output directory is locked by another transfer. After a crashed job, remove .als-transfer-lock only when no job is running.")
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  writeLines(as.character(Sys.getpid()), file.path(lock, "owner"))
  if (!is.null(progress_dir)) dir.create(progress_dir, recursive = TRUE, showWarnings = FALSE)
  old <- future::plan(); on.exit(future::plan(old), add = TRUE)
  if (policy$effective == 1L) future::plan(future::sequential)
  else future::plan(future::multisession, workers = policy$effective)
  rows <- lapply(seq_len(nrow(tiles)), function(i) as.list(tiles[i, , drop = FALSE]))
  result <- do.call(rbind, future.apply::future_lapply(rows, transfer_tile,
    output_dir = output_dir, retries = retries, timeout = timeout,
    progress_dir = progress_dir, future.seed = TRUE, future.chunk.size = 1L))
  utils::write.csv(result, file.path(output_dir, "manifest.csv"), row.names = FALSE)
  writeLines(unique(paste(tiles$citation, tiles$license_url, sep = "\n")),
              file.path(output_dir, "CITATIONS.txt"), useBytes = TRUE)
  invisible(result)
}

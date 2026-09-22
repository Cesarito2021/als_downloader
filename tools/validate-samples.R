# Explicit live acceptance test; never run by package checks.
# Usage: Rscript tools/validate-samples.R INDEX_DIRECTORY OUTPUT_DIRECTORY
# Run from the repository root after installing this package and lidR.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Supply an OpenTopography index directory and a sample output directory.")
library(ALSdownloadeR)
if (!requireNamespace("lidR", quietly = TRUE)) stop("Install lidR for full point decoding.")
samples <- utils::read.csv("docs/validation.csv", stringsAsFactors = FALSE)
dir.create(args[2], recursive = TRUE, showWarnings = FALSE)
report <- vector("list", nrow(samples))
for (i in seq_len(nrow(samples))) {
  x <- samples[i, ]
  report[[i]] <- tryCatch({
    if (x$country == "United States") {
      tiles <- find_tiles("tools/sample-usgs-aoi.geojson")
    } else {
      # Limit discovery to the selected dataset rather than scanning all archives.
      archive <- file.path(args[1], paste0(x$dataset, "_TileIndex.zip"))
      if (!file.exists(archive)) stop("Required dataset index is unavailable.")
      work <- tempfile(); dir.create(work)
      extracted <- tempfile(); dir.create(extracted)
      tiles <- tryCatch({
        file.copy(archive, work)
        utils::unzip(archive, exdir = extracted)
        index <- sf::st_read(list.files(extracted, "\\.shp$", full.names = TRUE, recursive = TRUE), quiet = TRUE)
        names(index) <- tolower(names(index))
        aoi <- index[as.character(index$url) == x$source_url, ]
        if (nrow(aoi) != 1L) stop("Sample is missing or duplicated in the supplied index.")
        find_tiles(aoi, "opentopography", tile_index_dir = work)
      }, finally = unlink(c(work, extracted), recursive = TRUE))
    }
    tile <- tiles[tiles$url == x$source_url, ]
    if (nrow(tile) != 1L) stop("AOI discovery did not resolve the sample.")
    tile$size_bytes <- x$bytes
    destination <- file.path(args[2], gsub(" ", "_", x$country))
    result <- download_tiles(tile, destination, workers = 1L, retries = 0L)
    if (result$status[1] == "failed") stop(result$message[1])
    cloud <- lidR::readLAS(result$path[1])
    points <- lidR::npoints(cloud)
    if (is.null(cloud) || points < 1L) stop("Decoded sample is empty.")
    resume <- download_tiles(tile, destination, workers = 1L, retries = 0L)$status[1]
    if (resume != "verified_existing") stop("Checksum restart failed.")
    data.frame(country = x$country, acquisition_method = x$acquisition_method, status = "pass", points = points,
      checksum = result$checksum[1], checked_on = as.character(Sys.Date()), message = "")
  }, error = function(e) data.frame(country = x$country, acquisition_method = x$acquisition_method, status = "pending_or_failed",
    points = NA_real_, checksum = NA_character_, checked_on = as.character(Sys.Date()), message = conditionMessage(e)))
  utils::write.csv(do.call(rbind, report[seq_len(i)]), file.path(args[2], "sample-report.csv"), row.names = FALSE)
}
if (any(vapply(report, function(x) x$status != "pass", logical(1)))) stop("Some sample checks did not pass; inspect sample-report.csv.")

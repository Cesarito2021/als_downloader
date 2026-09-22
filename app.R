# Install the package before launching this repository entry point.
if (!requireNamespace("ALSdownloadeR", quietly = TRUE)) {
  stop("Install ALSdownloadeR first: see the README installation instructions.")
}
mode <- Sys.getenv("ALS_MODE", "hosted")
options(shiny.maxRequestSize = if (identical(mode, "hosted")) 250 * 1024^2 else 1024 * 1024^2)
# Declare optional viewer/report dependencies for deployment discovery.
for (package in c("lidR", "rmarkdown", "tinytex")) {
  if (!requireNamespace(package, quietly = TRUE))
    stop("Install the deployment dependency: ", package)
}
ALSdownloadeR::als_app(
  mode = mode,
  tile_index_dir = Sys.getenv("ALS_TILE_INDEX_DIR", "")
)

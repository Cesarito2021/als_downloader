# Install the package before launching this repository entry point.
if (!requireNamespace("alsdownloader", quietly = TRUE)) {
  stop("Install alsdownloader first: see the README installation instructions.")
}
options(shiny.maxRequestSize = 200 * 1024^2)
alsdownloader::als_app(
  mode = Sys.getenv("ALS_MODE", "local"),
  tile_index_dir = Sys.getenv("ALS_TILE_INDEX_DIR", "")
)

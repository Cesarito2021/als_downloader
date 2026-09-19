# Load the same package revision as the parent, including load_all() sessions.
# Pass a task name and data, never a closure with a partially serialized namespace.
background_job <- function(task, args = list()) {
  ns <- asNamespace("alsdownloader")
  root <- getNamespaceInfo(ns, "path")
  source <- if (file.exists(file.path(root, "DESCRIPTION")) &&
                !file.exists(file.path(root, "Meta", "package.rds"))) root else NULL
  callr::r_bg(function(task, args, source) {
    if (!is.null(source)) {
      if (!requireNamespace("pkgload", quietly = TRUE))
        stop("Install pkgload for development sessions, or install alsdownloader before launching the app.")
      pkgload::load_all(source, quiet = TRUE, export_all = FALSE, helpers = FALSE)
    }
    do.call(get(task, envir = asNamespace("alsdownloader"), inherits = FALSE), args)
  }, args = list(task, args, source), libpath = .libPaths(), supervise = TRUE)
}

local_preview_job <- function(path, percent, window, x, y, voxel, xy_units = "auto", z_units = "auto") {
  on.exit(unlink(path))
  read_forest_preview(path, percent, window, x, y, voxel, xy_units, z_units)
}

remote_preview_job <- function(tile, path, percent, window, x, y, voxel, xy_units = "auto", z_units = "auto", zip_member = NULL) {
  tryCatch(preview_remote_tile(tile, path = path, zip_member = zip_member,
    progress = function(message) writeLines(message, paste0(path, ".status")),
    reader = function(file) read_forest_preview(file, percent, window, x, y, voxel, xy_units, z_units)),
    als_zip_selection = function(e) list(zip_members=e$members))
}

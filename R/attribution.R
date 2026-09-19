# Preserve dataset notices in figures; metadata presence is not legal clearance.
figure_attribution <- function(tiles = NULL) {
  if (is.null(tiles) || !nrow(tiles)) return("Source credit and licence not supplied. Check the original source before publication or redistribution.")
  value <- function(name, fallback) {
    x <- if (name %in% names(tiles)) as.character(tiles[[name]]) else rep(NA_character_, nrow(tiles))
    x[is.na(x) | !nzchar(trimws(x))] <- fallback
    x
  }
  credit <- value("citation", "credit not supplied")
  # The provider specifies different notices for original copies and derived views.
  if ("dataset" %in% names(tiles)) {
    auckland <- !is.na(tiles$dataset) & tiles$dataset == "Auckland_2013"
    credit[auckland] <- sub("Copyright in this work is owned by Auckland Council",
      "Copyright in the underlying dataset from which this work has been derived is owned by Auckland Council",
      credit[auckland], fixed = TRUE)
  }
  unique(paste0("Source: ", credit,
    " | Licence: ", value("license_url", "not supplied; verify with provider")))
}

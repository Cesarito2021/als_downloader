# Preserve dataset notices in figures; metadata presence is not legal clearance.
figure_attribution <- function(tiles = NULL) {
  if (is.null(tiles) || !nrow(tiles)) return("Source credit and licence not supplied. Check the original source before publication or redistribution.")
  value <- function(name, fallback) {
    x <- if (name %in% names(tiles)) as.character(tiles[[name]]) else rep(NA_character_, nrow(tiles))
    x[is.na(x) | !nzchar(trimws(x))] <- fallback
    x
  }
  unique(paste0("Source: ", value("citation", "credit not supplied"),
    " | Licence: ", value("license_url", "not supplied; verify with provider")))
}

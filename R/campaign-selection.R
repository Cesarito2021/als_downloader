campaign_scalar <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x) ||
      !nzchar(trimws(x)) || tolower(trimws(x)) %in% c("null", "none", "unknown", "n/a")) return(NA_character_)
  trimws(x)
}

# Preserve reported campaign IDs; expose fallback grouping and its provenance
# separately. A delivery block or dataset does not prove a separate flight.
tile_campaign_metadata <- function(tiles) {
  n <- nrow(tiles)
  if (!"campaign_id" %in% names(tiles)) tiles$campaign_id <- rep(NA_character_, n)
  tiles$campaign_group <- rep(NA_character_, n)
  tiles$campaign_basis <- rep("Not supplied", n)
  for (i in seq_len(n)) {
    id <- campaign_scalar(tiles$campaign_id[i])
    if (!is.na(id)) {
      tiles$campaign_group[i] <- id
      tiles$campaign_basis[i] <- "Provider metadata"
      next
    }
    href <- if ("url" %in% names(tiles)) campaign_scalar(tiles$url[i]) else NA_character_
    provider <- as.character(tiles$provider[i])
    if (!is.na(href) && provider == "usgs3dep" &&
        grepl("^https://usgslidar[a-z0-9]*[.]blob[.]core[.]windows[.]net/usgs-3dep-copc/usgs-copc/[^/]+/copc/", href)) {
      tiles$campaign_group[i] <- utils::URLdecode(strsplit(href, "/", fixed=TRUE)[[1]][6])
      tiles$campaign_basis[i] <- "Project directory"
    } else if (!is.na(href) && provider == "ignfr") {
      directory <- basename(dirname(sub("[?].*$", "", href)))
      pattern <- "^NUALHD_[0-9]+-[0-9]+__LAZ_[A-Z0-9]+_([A-Z]{2})_[0-9]{4}-[0-9]{2}-[0-9]{2}$"
      if (startsWith(href, "https://data.geopf.fr/telechargement/download/") && grepl(pattern, directory)) {
        tiles$campaign_group[i] <- paste("Block", sub(pattern, "\\1", directory))
        tiles$campaign_basis[i] <- "Delivery block"
      }
    } else if (provider %in% c("opentopography", "contributed", "ahn6")) {
      tiles$campaign_group[i] <- campaign_scalar(as.character(tiles$dataset[i]))
      if (!is.na(tiles$campaign_group[i])) tiles$campaign_basis[i] <- "Dataset"
    }
  }
  tiles
}

campaign_display <- function(tiles) {
  tiles <- tile_campaign_metadata(tiles)
  value <- tiles$campaign_group
  fallback <- !is.na(value) & tiles$campaign_basis != "Provider metadata"
  value[fallback] <- paste0(value[fallback], " (", tolower(tiles$campaign_basis[fallback]), ")")
  value[is.na(value)] <- "Not supplied"
  value
}

# Year membership follows reported acquisition intervals, never filename years.
tile_year_membership <- function(tiles) {
  if (is.null(tiles) || !nrow(tiles)) return(list())
  lapply(seq_len(nrow(tiles)), function(i) {
    dates <- c(tiles$acquired_start[i], tiles$acquired_end[i])
    years <- suppressWarnings(as.integer(substr(dates, 1, 4)))
    years <- years[!is.na(years) & years >= 1000L & years <= 9999L]
    if (!length(years)) return("unknown")
    as.character(seq.int(min(years), max(years)))
  })
}

selection_campaign_groups <- function(tiles, year="all") {
  if (is.null(tiles) || !nrow(tiles)) return(list())
  years <- tile_year_membership(tiles)
  ids <- which(vapply(years, function(x) identical(year, "all") || year %in% x, logical(1)))
  if (!length(ids)) return(list())
  campaign <- campaign_display(tiles)
  labels <- ifelse(campaign == "Not supplied",
    paste(tiles$provider, tiles$dataset, "(campaign not supplied)", sep=" / "),
    paste(tiles$provider, campaign, sep=" / "))
  split(ids, labels[ids])
}

campaign_tile_rows <- function(tiles, year="all", campaigns="all") {
  groups <- selection_campaign_groups(tiles, year)
  if (!length(groups) || !length(campaigns)) return(integer())
  if (!"all" %in% campaigns) groups <- groups[intersect(names(groups), campaigns)]
  sort(unique(as.integer(unlist(groups, use.names=FALSE))))
}

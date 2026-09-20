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
  campaign <- if ("campaign_id" %in% names(tiles)) tiles$campaign_id else rep(NA_character_, nrow(tiles))
  # Other sources expose datasets, not a verified campaign identifier.
  labels <- ifelse(is.na(campaign) | !nzchar(trimws(campaign)),
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

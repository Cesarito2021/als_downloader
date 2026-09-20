report_date_label <- function(start, end) {
  dates <- sort(unique(c(as.character(start), as.character(end))))
  dates <- dates[!is.na(dates) & nzchar(dates)]
  if (!length(dates)) return("Not reported")
  first <- min(dates); last <- max(dates)
  if (first == last) return(first)
  # A whole calendar year is more readable as its year alone.
  if (substr(first, 1, 4) == substr(last, 1, 4) &&
      substr(first, 6, 10) == "01-01" && substr(last, 6, 10) == "12-31") return(substr(first, 1, 4))
  paste(first, "to", last)
}

report_storage_label <- function(bytes) {
  bytes <- suppressWarnings(as.numeric(bytes))
  known <- is.finite(bytes) & bytes > 0
  if (!length(bytes)) return("No tiles selected")
  if (!any(known)) return("Size not reported by the provider")
  total <- sum(bytes[known])
  value <- if (total >= 1e9) sprintf("%.2f GB", total / 1e9) else
    if (total < 1e4) "<0.01 MB" else sprintf("%.2f MB", total / 1e6)
  if (all(known)) value else paste0(value, " reported; ", sum(!known), " file(s) with size not reported")
}

report_figure_caption <- function(filename) {
  name <- tolower(basename(filename))
  if (grepl("compar|profile|distribution", name)) return("Comparison of point-cloud acquisitions.")
  if (grepl("map|aoi", name)) return("Study area and selected tile footprints.")
  if (grepl("cloud|preview|3d", name)) return("3D point-cloud view.")
  paste("Exported view:", basename(filename))
}

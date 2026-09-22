report_date_label <- function(start, end) {
  if (!length(start) && !length(end)) return("NA")
  missing <- is.na(start) | is.na(end) | !nzchar(start) | !nzchar(end)
  years <- sort(unique(substr(as.character(end[!missing]),1,4)))
  label <- if(length(years)) paste(years,collapse=", ") else "NA"
  if(any(missing)) paste0(label,"; ",sum(missing)," file(s) with incomplete acquisition dates") else label
}

report_storage_label <- function(bytes) {
  bytes <- suppressWarnings(as.numeric(bytes))
  known <- is.finite(bytes) & bytes > 0
  if (!length(bytes)) return("No tiles selected")
  if (!any(known)) return("Total unavailable: all file sizes are NA")
  total <- sum(bytes[known])
  value <- if (total >= 1e9) sprintf("%.2f GB", total / 1e9) else
    if (total < 1e4) "<0.01 MB" else sprintf("%.2f MB", total / 1e6)
  if (all(known)) value else paste0(value, " known subtotal; total incomplete; ", sum(!known), " file(s) with size not reported")
}

report_figure_caption <- function(filename) {
  name <- tolower(basename(filename))
  if (grepl("compar|profile|distribution", name)) return("Comparison of point-cloud acquisitions.")
  if (grepl("map|aoi", name)) return("Study area and selected tile footprints.")
  if (grepl("cloud|preview|3d", name)) return("3D point-cloud view.")
  paste("Exported view:", basename(filename))
}

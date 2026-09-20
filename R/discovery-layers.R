# Two access masks: configured tile geometry and country-level external links.
discovery_groups <- function() c("Countries", "In-App Access", "External Access", "AOI")

add_discovery_layers <- function(map, world, catalog, overview) {
  map <- leaflet::addMapPane(map, "reference", zIndex = 410)
  for (code in unique(catalog$country_code[!catalog$implemented & catalog$country_code > 0])) {
    country <- world[which(world$code == code), , drop = FALSE]
    rows <- catalog[catalog$country_code == code & !catalog$implemented, , drop = FALSE]
    if (!nrow(country)) next
    popup <- as.character(shiny::tags$div(class = "als-provider-popup",
      shiny::tags$strong(paste(rows$country[1], "- External Access")),
      shiny::tags$ul(lapply(seq_len(nrow(rows)), function(i) shiny::tags$li(
        shiny::tags$a(href = rows$info_url[i], target = "_blank", rel = "noopener noreferrer", rows$name[i])))),
      shiny::tags$small("Country-level access link, not complete LiDAR coverage. Check the provider's coverage and conditions.")))
    map <- leaflet::addPolygons(map, data = country,
      group = "External Access", layerId = paste0("portal:", code),
      color = "#eab308", weight = 1, fillColor = "#eab308", fillOpacity = .22,
      label = paste(rows$country[1], "- External Access"), popup = popup,
      options = leaflet::pathOptions(pane = "reference", bubblingMouseEvents = FALSE))
  }
  external <- ot_external_coverage()
  if (!is.null(external) && nrow(external)) map <- leaflet::addPolygons(map, data=external,
    group="External Access", color="#eab308", weight=1, fillColor="#eab308", fillOpacity=.22,
    label=paste0("OpenTopography: ", external$title, " - External Access"),
    popup=lapply(seq_len(nrow(external)), function(i) as.character(shiny::tags$div(
      shiny::tags$strong(external$title[i]), shiny::tags$p(external$access_reason[i]),
      shiny::tags$p("Provider-reported survey extent. Download through the source portal; in-app tile download is unavailable."),
      shiny::tags$a(href=external$info_url[i],target="_blank",rel="noopener noreferrer","Open source portal")))),
    options=leaflet::pathOptions(pane="reference",bubblingMouseEvents=FALSE))
  map <- add_in_app_coverage(map, overview)
  map <- add_ot_map_coverage(map)
  leaflet::addControl(map, html = as.character(shiny::tags$div(class = "als-discovery-legend",
    shiny::tags$strong("LiDAR access"),
    shiny::tags$div(shiny::tags$span(style = "color:#ef4444", "\u25a0"), " In-App Access"),
    shiny::tags$div(shiny::tags$span(style = "color:#eab308", "\u25a0"), " External Access"),
    shiny::tags$small("Red: in-app access. OpenTopography dots locate surveys; zoom in for exact footprints. Yellow: external source links. Search confirms tiles."))),
    position = "bottomleft", layerId = "discovery_legend")
}

add_in_app_coverage <- function(map, overview) {
  if ("provider" %in% names(overview)) overview <- overview[overview$provider != "opentopography",,drop=FALSE]
  if (nrow(overview)) map <- leaflet::addPolygons(map, data = overview,
    group = "In-App Access", color = "#ff4b4b", weight = 2,
    fillColor = "#ef4444", fillOpacity = .4, label = ~dataset,
    popup = lapply(seq_len(nrow(overview)), function(i) as.character(shiny::tags$div(
      shiny::tags$strong(overview$dataset[i]),
      shiny::tags$p(if("coverage_note" %in% names(overview)) overview$coverage_note[i] else "Configured tile footprints."),
      shiny::tags$p("Define an AOI and use Find ALS data to retrieve tiles."),
      if("info_url" %in% names(overview) && nzchar(overview$info_url[i])) shiny::tags$a(href=overview$info_url[i],target="_blank",rel="noopener noreferrer","Source information"),
      if("citation" %in% names(overview)) shiny::tags$small(overview$citation[i]),
      if("license_url" %in% names(overview) && nzchar(overview$license_url[i])) shiny::tags$p(shiny::tags$a(href=overview$license_url[i],target="_blank",rel="noopener noreferrer","Source terms")),
      if("reviewed_on" %in% names(overview) && nzchar(overview$reviewed_on[i])) shiny::tags$p(paste("Coverage index:",overview$reviewed_on[i]))))),
    options = leaflet::pathOptions(pane = "reference", bubblingMouseEvents = FALSE))
  map
}

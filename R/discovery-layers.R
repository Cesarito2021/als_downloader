# Reference points locate catalogue entries, not the extent of a survey.
discovery_groups <- function() c("Countries", "Indexed survey areas", "In-app access", "External portals", "AOI")

add_discovery_layers <- function(map, world, catalog, overview) {
  map <- leaflet::addMapPane(map, "reference", zIndex = 410)
  if (nrow(overview)) map <- leaflet::addPolygons(map, data = overview,
    group = "Indexed survey areas", color = "#eab308", weight = 2,
    fillColor = "#eab308", fillOpacity = .2, label = ~dataset,
    popup = lapply(overview$dataset, function(name) as.character(shiny::tags$div(
      shiny::tags$strong(name), shiny::tags$p("Indexed survey footprint. Define an AOI and use Find ALS data to retrieve individual tiles.")))),
    options = leaflet::pathOptions(pane = "reference", bubblingMouseEvents = FALSE))
  for (code in unique(catalog$country_code[catalog$country_code > 0])) {
    country <- world[which(world$code == code), , drop = FALSE]
    if (!nrow(country)) next
    # Project before selecting a representative land point; do not use it as a boundary.
    point <- suppressWarnings(sf::st_transform(sf::st_point_on_surface(
      sf::st_make_valid(sf::st_transform(country, 3857))), 4326))
    # Larger external marker underneath the in-app marker keeps both selectable.
    for (implemented in c(FALSE, TRUE)) {
      rows <- catalog[catalog$country_code == code & catalog$implemented == implemented, , drop = FALSE]
      if (!nrow(rows)) next
      group <- if (implemented) "In-app access" else "External portals"
      popup <- as.character(shiny::tags$div(class = "als-provider-popup",
        shiny::tags$strong(paste(rows$country[1], "-", group)),
        shiny::tags$p(if (implemented) "AOI search is supported where source data or configured indexes are available." else "Consult these providers on their own websites. In-app tile search is not available for these entries."),
        shiny::tags$ul(lapply(seq_len(nrow(rows)), function(i) shiny::tags$li(
          shiny::tags$a(href = rows$info_url[i], target = "_blank", rel = "noopener noreferrer", rows$name[i])))),
        shiny::tags$small("Reference location only; not a survey footprint. Coverage and access conditions vary by source.")))
      map <- leaflet::addCircleMarkers(map, data = point,
        layerId = paste0("source:", code, ":", implemented), group = group,
        radius = if (implemented) 6 else 11, color = "#ffffff", weight = 1,
        fillColor = if (implemented) "#ef4444" else "#60a5fa", fillOpacity = .95,
        label = paste(rows$country[1], "-", group), popup = popup,
        options = leaflet::pathOptions(pane = "reference", bubblingMouseEvents = FALSE))
    }
  }
  leaflet::addControl(map, html = as.character(shiny::tags$div(class = "als-discovery-legend",
    shiny::tags$strong("LiDAR discovery"),
    shiny::tags$div(shiny::tags$span(style = "color:#eab308", "\u25a0"), " Indexed survey areas"),
    shiny::tags$div(shiny::tags$span(style = "color:#ef4444", "\u25cf"), " In-app access"),
    shiny::tags$div(shiny::tags$span(style = "color:#60a5fa", "\u25cf"), " External portals"),
    shiny::tags$small("Markers locate sources, not coverage. Unmarked areas may also have data."))),
    position = "bottomleft", layerId = "discovery_legend")
}

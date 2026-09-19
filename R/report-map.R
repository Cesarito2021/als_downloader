# Report map preparation is explicit: only the browser retrieves RGB imagery.
report_geojson <- function(x) {
  path <- tempfile(fileext = ".geojson")
  on.exit(unlink(path), add = TRUE)
  sf::st_write(sf::st_sf(geometry = sf::st_geometry(sf::st_transform(x, 4326))),
    path, driver = "GeoJSON", quiet = TRUE)
  jsonlite::fromJSON(paste(readLines(path, warn = FALSE), collapse = ""), simplifyVector = FALSE)
}

report_map_server <- function(input, output, session, aoi, tiles) {
  current <- shiny::reactive(digest::digest(list(aoi(), tiles())))
  map <- shiny::reactiveValues(path = NULL, key = NULL, token = NULL, action = NULL, credits = "")
  clear <- function() {
    if (!is.null(map$path)) unlink(map$path)
    map$path <- NULL; map$key <- NULL; map$token <- NULL
  }
  shiny::observeEvent(current(), {clear()}, ignoreInit = TRUE)
  session$onSessionEnded(function() shiny::isolate(clear()))
  shiny::observeEvent(input$report_map_request, {
    tryCatch({
      x <- tiles(); region <- aoi()
      if (is.null(region) || !nrow(region) || !inherits(x, "sf") || !nrow(x))
        stop("Draw or upload an AOI and select tiles before creating a report map.")
      action <- input$report_map_request$action
      if (!action %in% c("download_report_pdf", "download_report_map")) stop("Unknown report action.")
      clear()
      map$key <- current(); map$token <- digest::digest(list(map$key, Sys.time(), stats::runif(1)))
      map$action <- action
      session$sendCustomMessage("als-report-map", list(token = map$token,
        aoi = report_geojson(region), tiles = report_geojson(x), sources = as.list(figure_attribution(x))))
    }, error = function(e) {
      message <- conditionMessage(e)
      if (!nzchar(message)) message <- "Draw or upload an AOI and select tiles first."
      session$sendCustomMessage("als-report-map-error", message)
    })
  })
  shiny::observeEvent(input$report_map_result, {
    reply <- input$report_map_result
    if (is.null(map$token) || !identical(reply$token, map$token) || !identical(map$key, current())) {
      session$sendCustomMessage("als-report-map-error", "Selection changed during capture. Create the report again.")
      return()
    }
    tryCatch({
      if (!is.null(reply$error)) stop(reply$error)
      if (!is.character(reply$png) || length(reply$png) != 1L || nchar(reply$png) > 14 * 1024^2 ||
          !startsWith(reply$png, "data:image/png;base64,")) stop("Invalid or oversized map image.")
      raw <- jsonlite::base64_dec(sub("^data:image/png;base64,", "", reply$png))
      if (length(raw) < 24 || !identical(raw[1:8], as.raw(c(137,80,78,71,13,10,26,10)))) stop("Invalid map PNG.")
      map$path <- tempfile(fileext = ".png"); writeBin(raw, map$path)
      credit <- if (is.null(reply$credits)) "Imagery attribution is embedded in the map." else as.character(reply$credits)[1]
      map$credits <- chartr("\\`<>[]{}$", "         ", gsub("[\r\n]", " ", substr(credit, 1, 4000)))
      session$sendCustomMessage("als-report-map-ready", map$action)
    }, error = function(e) {clear(); session$sendCustomMessage("als-report-map-error", conditionMessage(e))})
  })
  get <- function() {
    if (is.null(map$path) || !identical(map$key, current()) || !file.exists(map$path))
      stop("The RGB map is not ready. Please create the report again.", call. = FALSE)
    list(path = map$path, credits = map$credits)
  }
  output$download_report_map <- shiny::downloadHandler(filename = "als-aoi-rgb.png",
    contentType = "image/png", content = function(file) file.copy(get()$path, file))
  get
}


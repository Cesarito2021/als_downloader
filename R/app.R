#' Launch the ALS Downloader Shiny application
#' @param mode `"local"` for configurable transfers or `"hosted"` for a serial
#'   download service. Set by the administrator, not by browser input.
#' @param tile_index_dir Optional administrator-supplied OpenTopography or contributed index
#'   directory. Not exposed as a server path control in hosted mode.
#' @param provider_limit Transfer concurrency ceiling after checking provider
#'   terms. Defaults to two; hosted mode always uses one.
#' @param host,port Bind address and optional port passed to [shiny::runApp()].
#' @param launch.browser Whether to open a browser. Defaults to interactive use.
#' @return Invisibly, the return value of [shiny::runApp()]. Runs until stopped.
#' @details Launch is explicit: loading the package does not start a browser,
#'   contact providers, change the working directory or install packages.
#'   Background transfer processes are terminated when the owning session ends.
#' @export
#' @examples
#' if (interactive()) launch_app()
launch_app <- function(mode = c("local", "hosted"), tile_index_dir = NULL,
                       provider_limit = 2L, host = "127.0.0.1", port = NULL,
                       launch.browser = interactive()) {
  old <- options(shiny.maxRequestSize = 200 * 1024^2)
  on.exit(options(old), add = TRUE)
  shiny::runApp(als_app(match.arg(mode), tile_index_dir, provider_limit),
                host = host, port = port, launch.browser = launch.browser)
}

#' Create the Shiny application object
#' @inheritParams launch_app
#' @return A `shiny.appobj` suitable for [shiny::runApp()] or a deployment entry point.
#' @export
#' @examples
#' if (interactive()) shiny::runApp(als_app())
als_app <- function(mode = "local", tile_index_dir = NULL, provider_limit = 2L) {
  mode <- match.arg(mode, c("local", "hosted"))
  assets <- system.file("app", "www", package = "alsdownloader")
  shiny::addResourcePath("als-assets", assets)
  shiny::addResourcePath("als-data", system.file("extdata", package = "alsdownloader"))
  catalog <- provider_catalog()
  cores <- as.numeric(parallelly::availableCores())
  policy <- download_worker_policy(mode, cores)
  world <- sf::st_read(system.file("extdata", "world-countries.geojson", package = "alsdownloader"), quiet = TRUE)
  world$code <- suppressWarnings(as.numeric(world$id))
  # One lock across sessions served by this app process. Multi-process hosting
  # must configure ALS_HOST_LOCK_DIR to a shared writable location.
  hosted_lock <- Sys.getenv("ALS_HOST_LOCK_DIR", file.path(tempdir(), "als-host-transfer-lock"))
  ui <- shiny::fluidPage(
    shiny::tags$head(shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      shiny::tags$link(rel = "stylesheet", href = "als-assets/explorer.css"),
      shiny::tags$script(src = "als-assets/profile.js"),
      shiny::tags$script(src = "als-assets/preview.js"),
      shiny::tags$script(src = "als-assets/globe.js")),
    shiny::div(class = "als-header", shiny::div(shiny::h1("ALS DOWNLOADER"),
      shiny::span("Discover, inspect and download airborne LiDAR")),
      shiny::div(class = "als-header-actions",
        shiny::actionButton("suggest_source", "Share your dataset"),
        shiny::span(class = "mode-label", paste(toupper(mode), "MODE")))),
    shiny::div(class = "als-layout",
      shiny::conditionalPanel("input.enter_map > 0", class = "als-sidebar-toggle",
        shiny::tags$details(class = "als-sidebar", open = "open",
        shiny::tags$summary("Study area and downloads"),
          shiny::fileInput("aoi_file", "Upload study area", accept = c(".zip", ".gpkg", ".geojson", ".json", ".fgb")),
        shiny::uiOutput("layer_control"),
        shiny::helpText("Or draw a polygon or rectangle on the map. ZIP uploads must include Shapefile companion files."),
        shiny::textOutput("aoi_status"),
        if (mode == "local") shiny::textInput("indexes", "Local tile-index directory (optional)", value = if (is.null(tile_index_dir)) "" else tile_index_dir),
        if (mode == "local") shiny::helpText("Only needed to also check OpenTopography, CanElevation or an approved contributed index against a local folder of tile indexes."),
        shiny::dateRangeInput("dates", "Acquisition interval", start = "2000-01-01", end = Sys.Date()),
        shiny::actionButton("search", "Find intersecting tiles", class = "als-primary"),
        shiny::helpText("Checks every configured source automatically; no provider to pick."),
        shiny::tags$hr(),
        if (mode == "local") shiny::tagList(
          shiny::textInput("destination", "Local output directory (optional)", value = "", placeholder = "Leave blank to use a temporary folder"),
          shiny::numericInput("workers", "Download workers", policy$recommended, min = 1, max = policy$maximum),
          shiny::helpText(paste("Recommended:", policy$recommended, "| maximum:", policy$maximum, "| provider ceiling:", provider_limit)))
        else shiny::helpText("Hosted downloads use one worker. Select up to 10 tiles per batch. Files are delivered through your browser."),
        shiny::actionButton("download", "Download selected tiles", class = "als-primary"),
        shiny::textOutput("selection_summary"),
        shiny::helpText("Downloads preserve original tiles, including portions outside the AOI. 3D inspection is optional."),
        shiny::actionButton("cancel", "Cancel job"),
        shiny::textOutput("job_status"), shiny::uiOutput("bundle_control"))),
      shiny::div(class = "als-main",
        shiny::tabsetPanel(id = "view",
          shiny::tabPanel("Explore",
            shiny::conditionalPanel("input.enter_map == 0",
              shiny::div(class = "als-globe-intro",
                shiny::div(class = "als-globe-wrap",
                  shiny::tags$canvas(id = "als-globe", tabindex = "0", role = "img", `aria-label` = "Rotatable world globe. Drag or use arrow keys to rotate. Red countries have an in-app download adapter; yellow countries link to an official source you must visit directly.",
                    `data-countries` = paste(unique(catalog$country_code), collapse = ","),
                    `data-implemented` = paste(unique(catalog$country_code[catalog$implemented]), collapse = ",")),
                  shiny::tags$div(class = "als-globe-legend", `aria-hidden` = "true",
                    shiny::tags$div(class = "als-globe-legend-row", shiny::tags$span(class = "als-globe-swatch als-globe-swatch-red"), "In-app download"),
                    shiny::tags$div(class = "als-globe-legend-row", shiny::tags$span(class = "als-globe-swatch als-globe-swatch-yellow"), "Portal link only"))),
                shiny::p(id = "globe_status", class = "als-globe-caption", "Drag to rotate. Neither colour reflects measured survey coverage."),
                shiny::div(class = "als-globe-actions",
                  shiny::tags$button(id = "globe_reset", type = "button", class = "als-link-btn", "Reset globe"),
                  shiny::actionButton("enter_map", "Open map", class = "als-primary"),
                  shiny::tags$a(class = "als-globe-credit", href = "https://www.naturalearthdata.com/about/terms-of-use/", "Natural Earth")))),
            shiny::conditionalPanel("input.enter_map > 0", leaflet::leafletOutput("map", height = "60vh"),
            shiny::div(class = "map-caption", "Red marks countries with an in-app download adapter; yellow marks countries with only a linked official source you must visit directly. Neither reflects continuous survey coverage. Search results show source tile footprints."),
            shiny::textOutput("search_status"), DT::DTOutput("tiles"),
            shiny::actionButton("select_all_tiles", "Select all results"),
            shiny::actionButton("clear_tiles", "Clear selection"),
            shiny::downloadButton("export_manifest", "Export all tile metadata"),
            shiny::downloadButton("export_selection", "Export selected metadata"),
            shiny::downloadButton("export_script", "Download selection as R script"),
            shiny::downloadButton("download_report", "Download session report"),
            shiny::div(class = "als-tile-preview",
              shiny::textOutput("tile_selection"),
              shiny::actionButton("plot_tile", "Plot selected tile in 3D", class = "als-primary"),
              shiny::helpText("Select one footprint or result. Plot opens the shared 3D preview; one source file up to 200 MB is downloaded temporarily.")))),
          shiny::tabPanel("3D preview",
            shiny::p("One viewer for the tile selected in Explore or a local LAS/LAZ file."),
            shiny::actionButton("replot_tile", "Rebuild selected map tile"),
            shiny::fileInput("point_file", "Upload a local LAS/LAZ tile (up to 200 MB)", accept = c(".las", ".laz")),
            forest_preview_controls("local_"),
            shiny::actionButton("preview", "Build bounded preview"),
            shiny::helpText("Preview decimation does not alter your source file. Large local tiles can also be read with read_preview() in R."),
            shiny::textOutput("preview_status"),
            shiny::tags$canvas(id = "als-cloud", role = "img", tabindex = "0", `aria-label` = "Interactive point-cloud preview. Arrow keys rotate; plus and minus zoom; zero resets."),
            shiny::selectInput("palette", "Elevation palette", c("Viridis", "Magma")),
            shiny::sliderInput("exaggeration", "Vertical exaggeration", min = 1, max = 12, value = 1, step = 1),
            shiny::div(class = "map-caption", "Drag or arrow keys to rotate | scroll or +/- to zoom | 0 to reset. Colors show source elevation, not canopy height.")),
          comparison_ui(),
          shiny::tabPanel("Sources and access", shiny::p("Discovery covers aircraft, helicopter and UAV laser scanning. Research deposits require verified polygon coverage or a tile index. Official national portals also provide external access; choose a country to open its source link. Terrestrial, spaceborne and photogrammetric acquisitions are outside the curated selection. Only providers marked Implemented have an in-app search adapter. Verify dataset terms and citations before downloading."),
            shiny::tags$a(href = "https://github.com/Cesarito2021/als_downloader/issues/new?template=suggest-dataset.yml", target = "_blank", rel = "noopener noreferrer", "Open the GitHub source suggestion form"),
            DT::DTOutput("sources")))))
  )
  server <- function(input, output, session) {
    state <- shiny::reactiveValues(aoi = NULL, tiles = NULL, search = "Draw or upload a study area to begin.",
      job = NULL, jobdir = NULL, destination = NULL, jobtext = "No active download.", finished = FALSE,
      preview_job = NULL, previewtext = "Upload one tile to inspect its structure.", lock_owned = FALSE,
      preview_target = "als-cloud", tiletext = "Select exactly one tile to preview.", preview_label = "", preview_path = NULL, preview_locked = FALSE)
    notify <- function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = 12)
    source_check_summary <- source_preflight_server(input, output, session, state, mode, hosted_lock)
    source_submission_server(input, output, session, source_check_summary)
    comparison_server(input, output, session, state, mode, hosted_lock)
    output$map <- leaflet::renderLeaflet({
      world$catalog <- world$code %in% catalog$country_code
      world$implemented <- world$code %in% catalog$country_code[catalog$implemented]
      leaflet::leaflet(world) |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite RGB") |>
        leaflet::addTiles("https://services.arcgisonline.com/arcgis/rest/services/Elevation/World_Hillshade/MapServer/tile/{z}/{y}/{x}",
          group = "Terrain relief", attribution = "Terrain: Esri, Airbus DS, USGS, NGA, NASA, CGIAR, NLS, OS, NMA, Geodatastyrelsen, GSA, GSI and GIS User Community",
          options = leaflet::tileOptions(maxZoom = 16, className = "als-relief-tiles")) |>
        leaflet::addPolygons(layerId = ~id, group = "Countries", color = "#60717c", weight = .5,
          fillColor = ~ifelse(implemented, "#dc2626", ifelse(catalog, "#eab308", "#25313c")),
          fillOpacity = ~ifelse(catalog, .14, 0), label = ~name) |>
        leaflet.extras::addDrawToolbar(targetGroup = "Study area", polygonOptions = leaflet.extras::drawPolygonOptions(showArea = TRUE),
          rectangleOptions = leaflet.extras::drawRectangleOptions(), polylineOptions = FALSE,
          markerOptions = FALSE, circleOptions = FALSE, circleMarkerOptions = FALSE,
          editOptions = leaflet.extras::editToolbarOptions()) |>
        leaflet::addLayersControl(baseGroups = c("Satellite RGB", "Terrain relief"), overlayGroups = c("Countries", "Study area", "Tiles")) |>
        leaflet::hideGroup("Terrain relief") |>
        leaflet::setView(0, 20, 2)
    })
    set_aoi <- function(x) {
      state$aoi <- read_aoi(x); state$tiles <- NULL
      state$search <- "Study area updated. Search to verify tile coverage."
      bb <- sf::st_bbox(state$aoi)
      leaflet::leafletProxy("map") |> leaflet::clearGroup("Tiles") |> leaflet::clearGroup("Study area") |>
        leaflet::addPolygons(data = state$aoi, group = "Study area", color = "#f1cb82", fillOpacity = .1) |>
        leaflet::fitBounds(bb[[1]], bb[[2]], bb[[3]], bb[[4]])
    }
    output$layer_control <- shiny::renderUI({
      shiny::req(input$aoi_file)
      if (tolower(tools::file_ext(input$aoi_file$name)) == "gpkg")
        shiny::selectInput("aoi_layer", "GeoPackage layer", sf::st_layers(input$aoi_file$datapath)$name)
    })
    shiny::observeEvent(list(input$aoi_file, input$aoi_layer), {
      shiny::req(input$aoi_file)
      if (tolower(tools::file_ext(input$aoi_file$name)) == "gpkg" && is.null(input$aoi_layer)) return()
      tryCatch(set_aoi(read_aoi(input$aoi_file, input$aoi_layer)), error = notify)
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$map_draw_new_feature, {
      tryCatch(set_aoi(sf::st_read(jsonlite::toJSON(input$map_draw_new_feature, auto_unbox = TRUE), quiet = TRUE)), error = notify)
    })
    shiny::observeEvent(input$map_draw_edited_features, {
      tryCatch(set_aoi(sf::st_read(jsonlite::toJSON(input$map_draw_edited_features, auto_unbox = TRUE), quiet = TRUE)), error = notify)
    })
    shiny::observeEvent(input$map_draw_deleted_features, {
      state$aoi <- NULL; state$tiles <- NULL; state$search <- "Study area removed."
      leaflet::leafletProxy("map") |> leaflet::clearGroup("Tiles") |> leaflet::clearGroup("Study area")
    })
    shiny::observeEvent(input$map_shape_click, {
      id <- input$map_shape_click$id
      if (!is.null(id) && startsWith(as.character(id), "tile:")) {
        index <- suppressWarnings(as.integer(sub("^tile:", "", id)))
        if (!is.null(state$tiles) && !is.na(index) && index >= 1L && index <= nrow(state$tiles))
          DT::selectRows(DT::dataTableProxy("tiles"), index)
      }
    })
    output$aoi_status <- shiny::renderText(if (is.null(state$aoi)) "No study area selected." else sprintf("Study area: %.4f km^2", aoi_area(state$aoi)))
    shiny::observeEvent(input$search, {
      shiny::req(state$aoi)
      state$tiles <- NULL
      leaflet::leafletProxy("map") |> leaflet::clearGroup("Tiles") |> leaflet::removeControl("tile_year_legend")
      state$search <- "Checking every configured source for this area..."
      tryCatch({
        # No provider picker: every network source is checked automatically, plus
        # any local-index sources for which a tile-index directory is configured.
        # A failure on one source (no coverage, network error, missing index) never
        # blocks the others; it is reported alongside whatever results did come back.
        local_dir <- if (mode == "local") input$indexes else tile_index_dir
        providers <- c("usgs3dep", "ahn6", "swisstopo", "ignfr")
        if (!is.null(local_dir) && nzchar(local_dir)) providers <- c(providers, "opentopography", "contributed", "canelevation")
        found <- list(); failed <- character()
        shiny::withProgress(message = "Checking sources", value = 0, {
          for (p in providers) {
            shiny::incProgress(1 / length(providers), detail = p)
            tiles <- tryCatch(find_tiles(state$aoi, p, as.character(input$dates[1]), as.character(input$dates[2]),
                if (p %in% c("opentopography", "contributed", "canelevation")) local_dir else NULL),
              error = function(e) {failed[[p]] <<- conditionMessage(e); NULL})
            if (!is.null(tiles) && nrow(tiles)) found[[p]] <- tiles
          }
        })
        result <- if (length(found)) do.call(rbind, found) else empty_tiles()
        state$tiles <- result
        state$search <- paste(c(
          if (nrow(result)) paste(nrow(result), "intersecting tiles across", length(found), "source(s). Select rows below; acquisition dates may be unknown.")
            else "No matching records from any configured source for this area and interval. This does not establish that no LiDAR data exist here.",
          if (length(failed)) paste0(names(failed), ": ", failed)), collapse = " | ")
        if (nrow(result)) {
          # Colour footprints by acquisition year (final date; start when the
          # end date is unknown) instead of one flat colour, so overlapping
          # surveys from different years are visually distinguishable at a
          # glance. Tiles with no provider-reported date stay a neutral grey,
          # never a guessed year.
          reported <- ifelse(is.na(result$acquired_end), result$acquired_start, result$acquired_end)
          year <- suppressWarnings(as.integer(substr(reported, 1, 4)))
          pal <- leaflet::colorNumeric("viridis", domain = year, na.color = "#8a97a1")
          leaflet::leafletProxy("map") |>
            leaflet::addPolygons(data = result, group = "Tiles",
              layerId = paste0("tile:", seq_len(nrow(result))), color = "#1c2b35", weight = 1,
              fillColor = pal(year), fillOpacity = .55, label = ~filename) |>
            leaflet::addLegend("bottomright", group = "Tiles", layerId = "tile_year_legend",
              pal = pal, values = year, title = "Acquisition year", na.label = "Unknown")
        }
      }, error = function(e) {state$search <- conditionMessage(e); notify(e)})
    })
    output$search_status <- shiny::renderText(state$search)
    output$tiles <- DT::renderDT({
      if (is.null(state$tiles)) return(DT::datatable(data.frame(Status = "No search results yet."), rownames = FALSE))
      DT::datatable(sf::st_drop_geometry(state$tiles)[c("filename", "dataset", "provider", "acquired_end", "acquired_start", "size_bytes", "license_url", "citation")],
        colnames = c("File", "Dataset", "Source adapter", "Collection date (end)", "Collection start", "Size (bytes)", "License", "Producer / citation"),
        rownames = FALSE, selection = "multiple", options = list(scrollX = TRUE, pageLength = 8))
    })
    output$sources <- DT::renderDT(DT::datatable(catalog, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 15)))
    selected_tiles <- shiny::reactive({
      shiny::req(state$tiles)
      ids <- input$tiles_rows_selected
      ids <- ids[ids %in% seq_len(nrow(state$tiles))]
      state$tiles[ids, , drop = FALSE]
    })
    shiny::observeEvent(input$select_all_tiles, {shiny::req(state$tiles); DT::selectRows(DT::dataTableProxy("tiles"), seq_len(nrow(state$tiles)))})
    shiny::observeEvent(input$clear_tiles, DT::selectRows(DT::dataTableProxy("tiles"), integer()))
    output$selection_summary <- shiny::renderText({
      x <- selected_tiles(); known <- is.finite(x$size_bytes)
      sprintf("%s tiles selected | %.1f MiB known | %s files with unknown size", nrow(x), sum(x$size_bytes[known])/1024^2, sum(!known))
    })
    output$export_selection <- shiny::downloadHandler(filename = "selected-tiles.csv", content = function(file) {
      x <- sf::st_drop_geometry(selected_tiles()); shiny::req(nrow(x)); x$url <- redact_url(x$url)
      utils::write.csv(x, file, row.names = FALSE)
    })
    output$export_script <- shiny::downloadHandler(filename = "download-selected-tiles.R", content = function(file) {
      x <- selected_tiles(); shiny::req(nrow(x)); writeLines(selection_script(x), file, useBytes = TRUE)
    })
    output$download_report <- shiny::downloadHandler(filename = "als-session-report.html", content = function(file) {
      x <- selected_tiles(); shiny::req(nrow(x))
      area <- if (is.null(state$aoi)) NA_real_ else aoi_area(state$aoi)
      dir <- tempfile("als-report-"); dir.create(dir)
      on.exit(unlink(dir, recursive = TRUE), add = TRUE)
      path <- als_report(x, dir, aoi_area_km2 = area)
      file.copy(path, file, overwrite = TRUE)
    })
    output$tile_selection <- shiny::renderText({
      selected <- input$tiles_rows_selected
      if (is.null(state$tiles) || length(selected) != 1L || !selected %in% seq_len(nrow(state$tiles)))
        "Select exactly one result for the 3D preview."
      else paste("Selected:", state$tiles$filename[selected])
    })
    output$export_manifest <- shiny::downloadHandler(filename = "tile-metadata.csv", content = function(file) {
      shiny::req(state$tiles)
      df <- sf::st_drop_geometry(state$tiles); df$url <- redact_url(df$url)
      utils::write.csv(df, file, row.names = FALSE)
    })
    release_lock <- function() {if (isTRUE(state$lock_owned)) {unlink(hosted_lock, recursive = TRUE); state$lock_owned <- FALSE}}
    release_output_lock <- function() {
      if (is.null(state$destination) || is.null(state$job) || state$job$is_alive()) return()
      lock <- file.path(state$destination, ".als-transfer-lock")
      owner <- file.path(lock, "owner")
      if (file.exists(owner) && identical(readLines(owner, warn = FALSE), as.character(state$job$get_pid())))
        unlink(lock, recursive = TRUE)
    }
    shiny::observeEvent(input$download, {
      if ((isTRUE(state$comparison_busy) || isTRUE(state$source_test_busy))) {shiny::showNotification("Wait for the campaign comparison or cancel it first."); return()}
      if (!is.null(state$preview_job) && state$preview_job$is_alive()) {
        shiny::showNotification("Wait for the tile preview before starting another transfer."); return()
      }
      if (!is.null(state$job) && state$job$is_alive()) {shiny::showNotification("A download is already running."); return()}
      shiny::req(state$tiles, input$tiles_rows_selected)
      rows <- state$tiles[input$tiles_rows_selected, , drop = FALSE]
      tryCatch({
        require_data_terms(rows)
        if (mode == "hosted" && nrow(rows) > 10) stop("Hosted batches are limited to 10 tiles. Run locally for larger batches.")
        if (mode == "hosted" && anyNA(rows$size_bytes)) {
          for (i in which(is.na(rows$size_bytes))) {
            url <- rows$url[i]
            if (rows$provider[i] == "usgs3dep") url <- request_json("https://planetarycomputer.microsoft.com/api/sas/v1/sign", query = list(href = url))$href
            response <- httr::HEAD(url, httr::timeout(30))
            if (httr::status_code(response) == 200L) {
              size <- suppressWarnings(as.numeric(httr::headers(response)[["content-length"]]))
              if (length(size) == 1L && is.finite(size)) rows$size_bytes[i] <- size
            }
          }
        }
        if (mode == "hosted" && (anyNA(rows$size_bytes) || sum(rows$size_bytes) > 500 * 1024^2))
          stop("Hosted download requires known total size below 500 MB. Use local mode for unknown or larger transfers.")
        if (mode == "hosted") {
          if (!dir.create(hosted_lock, showWarnings = FALSE)) stop("Another hosted download is running. Try again later.")
          state$lock_owned <- TRUE
        }
        jobdir <- tempfile("als-job-"); dir.create(jobdir)
        destination <- if (mode == "hosted") file.path(jobdir, "files")
          else if (nzchar(trimws(input$destination))) input$destination
          else {auto <- file.path(jobdir, "files"); dir.create(auto); auto}
        state$jobdir <- jobdir; state$destination <- destination; state$finished <- FALSE
        state$job <- callr::r_bg(function(tiles, path, workers, mode, ceiling, progress) {
          alsdownloader::download_tiles(tiles, path, workers = workers, mode = mode,
            provider_limit = ceiling, progress_dir = progress)
        }, args = list(rows, destination, if (mode == "hosted") 1L else input$workers,
          mode, provider_limit, file.path(jobdir, "progress")), supervise = TRUE)
        state$jobtext <- paste("Download started in a background process. Saving to", destination)
      }, error = function(e) {release_lock(); notify(e)})
    })
    shiny::observe({
      shiny::invalidateLater(700, session)
      job <- state$job
      if (is.null(job) || isTRUE(state$finished)) return()
      completed <- length(list.files(file.path(state$jobdir, "progress"), "\\.rds$"))
      state$jobtext <- paste(completed, "tiles reported.")
      if (!job$is_alive()) {
        state$finished <- TRUE; release_lock(); release_output_lock()
        tryCatch({result <- job$get_result(); state$jobtext <- sprintf("Complete: %s successful, %s failed. Files saved in %s (see manifest.csv).",
          sum(result$status != "failed"), sum(result$status == "failed"), state$destination)},
          error = function(e) state$jobtext <- "Download stopped. Retry the selection to resume verified files.")
      }
    })
    shiny::observeEvent(input$cancel, {
      if (isTRUE(state$preview_locked)) {
        shiny::showNotification("The preview is using the transfer slot; wait for it to finish."); return()
      }
      if (!is.null(state$job) && state$job$is_alive()) state$job$kill_tree()
      state$finished <- TRUE; state$jobtext <- "Canceled. Completed local tiles can be resumed."; release_lock(); release_output_lock()
    })
    output$job_status <- shiny::renderText(state$jobtext)
    output$bundle_control <- shiny::renderUI(if (mode == "hosted" && isTRUE(state$finished) && !is.null(state$destination) && file.exists(file.path(state$destination, "manifest.csv"))) shiny::downloadButton("bundle", "Save hosted download ZIP"))
    output$bundle <- shiny::downloadHandler(filename = "als-tiles.zip", content = function(file) {
      files <- list.files(state$destination, full.names = TRUE)
      files <- files[!grepl("\\.(rds|part)$", files)]
      zip::zipr(file, files, root = state$destination)
    })
    shiny::observeEvent(input$preview, {
      if ((isTRUE(state$comparison_busy) || isTRUE(state$source_test_busy))) {shiny::showNotification("Wait for the campaign comparison or cancel it first."); return()}
      shiny::req(input$point_file)
      if (!is.null(state$preview_job) && state$preview_job$is_alive()) return()
      state$preview_target <- "als-cloud"
      state$preview_label <- input$point_file$name
      state$previewtext <- "Reading a bounded point sample..."
      preview_path <- tempfile(fileext = paste0(".", tools::file_ext(input$point_file$name)))
      state$preview_path <- preview_path
      file.copy(input$point_file$datapath, preview_path)
      state$preview_job <- callr::r_bg(function(reader, path, percent, window, x, y, voxel) {
        on.exit(unlink(path))
        reader(path, percent, window, x, y, voxel)
      }, args = list(read_forest_preview, preview_path, input$local_percent, as.numeric(input$local_window), input$local_center_x, input$local_center_y, as.numeric(input$local_voxel)), supervise = TRUE)
    })
    shiny::observeEvent(c(input$plot_tile, input$replot_tile), ignoreInit = TRUE, {
      if ((isTRUE(state$comparison_busy) || isTRUE(state$source_test_busy))) {shiny::showNotification("Wait for the campaign comparison or cancel it first."); return()}
      if (!is.null(state$job) && state$job$is_alive()) {
        shiny::showNotification("Wait for the current download before plotting a remote tile."); return()
      }
      if (!is.null(state$preview_job) && state$preview_job$is_alive()) {
        shiny::showNotification("A preview is already running."); return()
      }
      selected <- input$tiles_rows_selected
      if (is.null(state$tiles) || length(selected) != 1L || !selected %in% seq_len(nrow(state$tiles))) {
        shiny::showNotification("Select exactly one tile in the table or click its footprint."); return()
      }
      tile <- sf::st_drop_geometry(state$tiles[selected, , drop = FALSE])
      if (mode == "hosted") {
        if (!dir.create(hosted_lock, showWarnings = FALSE)) {
          shiny::showNotification("Another hosted transfer is running. Try again later."); return()
        }
        state$lock_owned <- TRUE; state$preview_locked <- TRUE
      }
      state$preview_target <- "als-cloud"
      shiny::updateTabsetPanel(session, "view", selected = "3D preview")
      state$preview_label <- tile$filename[[1]]
      state$previewtext <- paste("Downloading and sampling:", state$preview_label)
      session$sendCustomMessage("als-points", list(target = "als-cloud", points = list(), origin = c(0, 0, 0)))
      state$preview_path <- tempfile(fileext = ".laz")
      tryCatch({state$preview_job <- callr::r_bg(function(remote_reader, forest_reader, tile, path, percent, window, x, y, voxel)
        remote_reader(tile, path = path, reader = function(file) forest_reader(file, percent, window, x, y, voxel)),
        args = list(preview_remote_tile, read_forest_preview, tile, state$preview_path, input$local_percent, as.numeric(input$local_window), input$local_center_x, input$local_center_y, as.numeric(input$local_voxel)), supervise = TRUE)}, error = function(e) {
          if (isTRUE(state$preview_locked)) {release_lock(); state$preview_locked <- FALSE}
          state$previewtext <- "Could not start the preview process."
        })
    })
    shiny::observe({
      shiny::invalidateLater(500, session)
      job <- state$preview_job
      if (is.null(job) || job$is_alive()) return()
      state$preview_job <- NULL
      if (isTRUE(state$preview_locked)) {release_lock(); state$preview_locked <- FALSE}
      if (!is.null(state$preview_path)) unlink(state$preview_path)
      tryCatch({p <- job$get_result()
        caption <- paste(nrow(p), "preview points. Elevation uses source units; confirm CRS and vertical datum.")
        if (!is.null(attr(p, "display_note"))) caption <- paste(caption, attr(p, "display_note"))
        if (state$preview_target == "als-cloud") state$previewtext <- paste(state$preview_label, "-", caption)
        else state$tiletext <- paste(state$preview_label, "-", caption)
        session$sendCustomMessage("als-points", list(target = state$preview_target, points = unname(as.matrix(p)), origin = unname(attr(p, "origin"))))},
        error = function(e) {
          message <- "Preview failed: check provider access, known file size (up to 200 MB), LAS/LAZ format and lidR installation."
          if (state$preview_target == "als-cloud") state$previewtext <- message else state$tiletext <- message
        })
    })
    output$preview_status <- shiny::renderText(state$previewtext)
    output$tile_preview_status <- shiny::renderText(state$tiletext)
    shiny::observeEvent(input$local_pose, session$sendCustomMessage("als-view", list(target = "als-cloud", pose = input$local_pose)))
    shiny::observeEvent(input$local_point_size, session$sendCustomMessage("als-view", list(target = "als-cloud", pointSize = input$local_point_size)))
    shiny::observeEvent(input$exaggeration, session$sendCustomMessage("als-view", list(target = "als-cloud", exaggeration = input$exaggeration)))
    shiny::observeEvent(input$palette, session$sendCustomMessage("als-view", list(target = "als-cloud", palette = input$palette)))
    session$onSessionEnded(function() shiny::isolate({
      if (!is.null(state$job) && state$job$is_alive()) state$job$kill_tree()
      if (!is.null(state$preview_job) && state$preview_job$is_alive()) state$preview_job$kill_tree()
      if (!is.null(state$preview_path)) unlink(state$preview_path)
      release_lock()
      release_output_lock()
      if (!is.null(state$jobdir)) unlink(state$jobdir, recursive = TRUE)
    }))
  }
  shiny::shinyApp(ui, server, options = list(shiny.maxRequestSize = 200 * 1024^2))
}

#' Launch the ALS Downloader Shiny application
#' @param mode `"local"` for configurable transfers or `"hosted"` for a serial
#'   download service. Set by the administrator, not by browser input.
#' @param tile_index_dir Optional administrator-supplied OpenTopography index
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
      shiny::tags$script(src = "als-assets/preview.js")),
    shiny::div(class = "als-header", shiny::div(shiny::h1("ALS DOWNLOADER"),
      shiny::span("Global point cloud explorer")),
      shiny::div(class = "als-header-actions",
        shiny::actionButton("suggest_source", "Submit a data source"),
        shiny::span(class = "mode-label", paste(toupper(mode), "MODE")))),
    shiny::div(class = "als-layout",
      shiny::tags$details(class = "als-sidebar", open = "open",
        shiny::tags$summary("Study area and downloads"),
        shiny::selectInput("country", "Explore a country", choices = c("World" = "", stats::setNames(catalog$country_code[catalog$country_code > 0 & !duplicated(catalog$country_code)], catalog$country[catalog$country_code > 0 & !duplicated(catalog$country_code)]))),
        shiny::fileInput("aoi_file", "Upload study area", accept = c(".zip", ".gpkg", ".geojson", ".json", ".fgb")),
        shiny::uiOutput("layer_control"),
        shiny::helpText("Or draw a polygon or rectangle on the map. ZIP uploads must include Shapefile companion files."),
        shiny::textOutput("aoi_status"),
        shiny::selectInput("provider", "Search provider", c("USGS 3DEP" = "usgs3dep", "OpenTopography" = "opentopography")),
        if (mode == "local") shiny::textInput("indexes", "OpenTopography TileIndex directory", value = if (is.null(tile_index_dir)) "" else tile_index_dir),
        shiny::dateRangeInput("dates", "Acquisition interval", start = "2000-01-01", end = Sys.Date()),
        shiny::actionButton("search", "Find intersecting tiles", class = "als-primary"),
        shiny::tags$hr(),
        if (mode == "local") shiny::tagList(
          shiny::textInput("destination", "Local output directory", value = ""),
          shiny::numericInput("workers", "Download workers", policy$recommended, min = 1, max = policy$maximum),
          shiny::helpText(paste("Recommended:", policy$recommended, "| maximum:", policy$maximum, "| provider ceiling:", provider_limit)))
        else shiny::helpText("Hosted downloads use one worker. Select up to 10 tiles per batch. Files are delivered through your browser."),
        shiny::actionButton("download", "Download selected tiles", class = "als-primary"),
        shiny::actionButton("cancel", "Cancel job"),
        shiny::textOutput("job_status"), shiny::uiOutput("bundle_control")),
      shiny::div(class = "als-main",
        shiny::tabsetPanel(id = "view",
          shiny::tabPanel("Explore", leaflet::leafletOutput("map", height = "60vh"),
            shiny::div(class = "map-caption", "Country shading indicates catalog candidates, not continuous survey coverage. Search results show source tile footprints."),
            shiny::textOutput("search_status"), DT::DTOutput("tiles"),
            shiny::downloadButton("export_manifest", "Export tile metadata"),
            shiny::div(class = "als-tile-preview",
              shiny::h3("Selected tile | 3D landscape"),
              shiny::helpText("Click a tile footprint or select one table row, then plot it. Preview downloads one source file (up to 200 MB) temporarily; source elevation is not canopy height."),
              shiny::actionButton("plot_tile", "Plot selected tile", class = "als-primary"),
              forest_preview_controls("tile_"),
              shiny::textOutput("tile_selection"),
              shiny::textOutput("tile_preview_status"),
              shiny::tags$canvas(id = "als-tile-cloud", class = "als-point-cloud", role = "img", tabindex = "0", `aria-label` = "Selected tile point cloud. Arrow keys rotate; plus and minus zoom; zero fits the view."),
              shiny::fluidRow(shiny::column(4, shiny::selectInput("tile_palette", "Elevation palette", c("Viridis", "Magma"))),
                shiny::column(4, shiny::sliderInput("tile_exaggeration", "Vertical exaggeration", min = 1, max = 12, value = 1, step = 1)),
                shiny::column(4, shiny::actionButton("tile_fit", "Fit landscape"))),
              shiny::div(class = "map-caption", "Drag to orbit | scroll to zoom | double-click to fit. Display sample only; source files are unchanged."))),
          shiny::tabPanel("3D preview", shiny::fileInput("point_file", "Upload a local LAS/LAZ tile (up to 200 MB)", accept = c(".las", ".laz")),
            forest_preview_controls("local_"),
            shiny::actionButton("preview", "Build bounded preview"),
            shiny::helpText("Preview decimation does not alter your source file. Large local tiles can also be read with read_preview() in R."),
            shiny::textOutput("preview_status"),
            shiny::tags$canvas(id = "als-cloud", role = "img", tabindex = "0", `aria-label` = "Interactive point-cloud preview. Arrow keys rotate; plus and minus zoom; zero resets."),
            shiny::selectInput("palette", "Elevation palette", c("Viridis", "Magma")),
            shiny::sliderInput("exaggeration", "Vertical exaggeration", min = 1, max = 12, value = 1, step = 1),
            shiny::div(class = "map-caption", "Drag or arrow keys to rotate | scroll or +/- to zoom | 0 to reset. Colors show source elevation, not canopy height.")),
          comparison_ui(),
          shiny::tabPanel("Sources and access", shiny::p("Discovery covers aircraft, helicopter and UAV laser scanning. Zenodo entries are complementary research deposits, not official national coverage. Terrestrial, spaceborne and photogrammetric acquisitions are outside the curated selection. Only providers marked Implemented have a search adapter. Verify dataset terms and citations before downloading."),
            shiny::tags$a(href = "https://github.com/Cesarito2021/als_downloader/issues/new?template=suggest-dataset.yml", target = "_blank", rel = "noopener noreferrer", "Open the GitHub source suggestion form"),
            zenodo_source_ui(), DT::DTOutput("sources")))))
  )
  server <- function(input, output, session) {
    zenodo_source_server(input, output, session)
    shiny::observeEvent(input$suggest_source, {
      shiny::showModal(shiny::modalDialog(
        title = "Submit a data source", size = "l", easyClose = FALSE,
        shiny::p("Help expand the aerial LiDAR catalog. Share links to existing sources; do not upload point clouds. Suggestions are reviewed before integration."),
        shiny::textInput("source_name", "Dataset or product name *"),
        shiny::textInput("source_owner", "Data producer / institution *", placeholder = "Who collected the LiDAR data?"),
        shiny::textInput("source_url", "Dataset access or download link *", placeholder = "Public file, catalog, API or download page URL"),
        shiny::textInput("source_storage", "Where are the files stored? *", placeholder = "Host and public location: Zenodo record, AWS bucket, institutional repository, etc."),
        shiny::textAreaInput("source_description", "Short dataset description *", rows = 2, placeholder = "One or two sentences about the aerial LiDAR data and coverage."),
        shiny::textAreaInput("source_acknowledgement", "Short required acknowledgement *", rows = 2, placeholder = "Preferred credit text for the data producer or project; write None if none is requested."),
        shiny::textInput("source_origin", "Dataset DOI or original platform link *", placeholder = "Persistent DOI or original provider's dataset landing page"),
        shiny::selectInput("source_platform", "Laser acquisition platform *", c("Choose a platform" = "", "Aircraft / helicopter ALS", "UAV LiDAR", "Mixed aerial laser platforms", "Unknown - needs review")),
        shiny::helpText("Only aerial laser scanning is eligible. Terrestrial, spaceborne and photogrammetric acquisitions are outside scope."),
        shiny::textInput("source_area", "Country / site and acquisition years *"),
        shiny::textInput("source_paper", "Related paper, preprint or DOI (optional)"),
        shiny::textAreaInput("source_details", "License, access and spatial metadata *", rows = 3,
          placeholder = "License/citation; LAS/LAZ files; download or login requirements; footprint/index URL; CRS. Identify aerial files in mixed deposits. Write unknown where needed."),
        shiny::textInput("source_contact", "Your name / contact (optional)"),
        shiny::checkboxInput("source_review", "I understand that this is a suggestion and requires review before inclusion.", FALSE),
        shiny::uiOutput("source_submission"),
        shiny::helpText("Prepare email opens your email application: review and send it there. GitHub opens a public draft and requires an account. Nothing is sent automatically; no dataset files are stored by this form."),
        footer = shiny::modalButton("Close")))
    })
    source_proposal <- shiny::reactive({
      required <- c("source_name", "source_owner", "source_url", "source_storage", "source_description", "source_acknowledgement", "source_origin", "source_platform", "source_area", "source_details")
      values <- vapply(required, function(id) if (is.null(input[[id]])) "" else trimws(input[[id]]), character(1))
      shiny::req(all(nzchar(values)), isTRUE(input$source_review))
      # Bound outgoing proposal text to keep draft links manageable.
      limits <- c(150L, 200L, 400L, 400L, 500L, 400L, 400L, 80L, 250L, 1400L)
      values <- mapply(substr, values, 1L, limits, USE.NAMES = TRUE)
      optional <- function(id, limit) if (is.null(input[[id]])) "" else substr(trimws(input[[id]]), 1L, limit)
      list(title = paste("Dataset suggestion:", values[[1]]), body = paste(
        paste(c("Dataset", "Data producer / institution", "Dataset access / download link", "Storage host / public location", "Short description", "Required acknowledgement", "Dataset DOI / original platform", "Platform", "Country / site / acquisition years", "License / access / spatial metadata"), values, sep = ": ", collapse = "\n\n"),
        paste("Paper / preprint:", optional("source_paper", 400L)),
        paste("Contributor contact:", optional("source_contact", 150L)),
        "Submitted for review; inclusion and automatic downloads are not yet approved.", sep = "\n\n"))
    })
    output$source_submission <- shiny::renderUI({
      proposal <- source_proposal()
      encode <- function(x) utils::URLencode(enc2utf8(x), reserved = TRUE)
      shiny::tagList(
        shiny::tags$a(class = "btn als-primary", href = paste0("mailto:calvites1990@gmail.com?subject=", encode(proposal$title), "&body=", encode(proposal$body)), "Prepare email"),
        " ", shiny::tags$a(class = "btn", target = "_blank", rel = "noopener noreferrer",
          href = paste0("https://github.com/Cesarito2021/als_downloader/issues/new?title=", encode(proposal$title), "&body=", encode(proposal$body)), "Open public GitHub draft"),
        shiny::downloadButton("source_proposal_file", "Save proposal (.txt)"),
        shiny::tags$details(shiny::tags$summary("Review proposal text"), shiny::tags$pre(proposal$body)))
    })
    output$source_proposal_file <- shiny::downloadHandler(
      filename = function() "als-source-proposal.txt",
      content = function(file) {
        proposal <- source_proposal()
        writeLines(enc2utf8(c(proposal$title, "", proposal$body)), file, useBytes = TRUE)
      }, contentType = "text/plain; charset=utf-8")
    state <- shiny::reactiveValues(aoi = NULL, tiles = NULL, search = "Draw or upload a study area to begin.",
      job = NULL, jobdir = NULL, destination = NULL, jobtext = "No active download.", finished = FALSE,
      preview_job = NULL, previewtext = "Upload one tile to inspect its structure.", lock_owned = FALSE,
      preview_target = "als-cloud", tiletext = "Select exactly one tile to preview.", preview_label = "", preview_path = NULL, preview_locked = FALSE)
    notify <- function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = 12)
    comparison_server(input, output, session, state, mode, hosted_lock)
    output$map <- leaflet::renderLeaflet({
      world$catalog <- world$code %in% catalog$country_code
      leaflet::leaflet(world) |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite RGB") |>
        leaflet::addTiles("https://services.arcgisonline.com/arcgis/rest/services/Elevation/World_Hillshade/MapServer/tile/{z}/{y}/{x}",
          group = "Terrain relief", attribution = "Terrain: Esri, Airbus DS, USGS, NGA, NASA, CGIAR, NLS, OS, NMA, Geodatastyrelsen, GSA, GSI and GIS User Community",
          options = leaflet::tileOptions(maxZoom = 16, className = "als-relief-tiles")) |>
        leaflet::addPolygons(layerId = ~id, group = "Countries", color = "#60717c", weight = .5,
          fillColor = ~ifelse(catalog, "#2c7565", "#25313c"), fillOpacity = .06, label = ~name) |>
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
    navigate <- function(id) {
      if (!nzchar(id)) {leaflet::leafletProxy("map") |> leaflet::setView(0, 20, 2); return()}
      row <- world[which(world$code == suppressWarnings(as.numeric(id))), ]
      if (nrow(row)) {bb <- sf::st_bbox(row); leaflet::leafletProxy("map") |> leaflet::fitBounds(bb[[1]], bb[[2]], bb[[3]], bb[[4]])}
    }
    shiny::observeEvent(input$country, navigate(input$country))
    shiny::observeEvent(input$map_shape_click, {
      id <- input$map_shape_click$id
      if (!is.null(id) && startsWith(as.character(id), "tile:")) {
        index <- suppressWarnings(as.integer(sub("^tile:", "", id)))
        if (!is.null(state$tiles) && !is.na(index) && index >= 1L && index <= nrow(state$tiles))
          DT::selectRows(DT::dataTableProxy("tiles"), index)
        return()
      }
      if (!is.null(id) && id %in% world$id) navigate(as.character(id))
    })
    output$aoi_status <- shiny::renderText(if (is.null(state$aoi)) "No study area selected." else sprintf("Study area: %.4f km^2", aoi_area(state$aoi)))
    shiny::observeEvent(input$search, {
      shiny::req(state$aoi)
      state$tiles <- NULL; leaflet::leafletProxy("map") |> leaflet::clearGroup("Tiles")
      state$search <- "Searching provider..."
      tryCatch({
        result <- shiny::withProgress(message = "Finding source tiles", value = .2, {
          find_tiles(state$aoi, input$provider, as.character(input$dates[1]), as.character(input$dates[2]),
            if (mode == "local") input$indexes else tile_index_dir)
        })
        state$tiles <- result
        state$search <- paste(nrow(result), "intersecting tiles. Select rows below; acquisition dates may be unknown.")
        if (nrow(result)) leaflet::leafletProxy("map") |> leaflet::addPolygons(data = result, group = "Tiles",
          layerId = paste0("tile:", seq_len(nrow(result))), color = "#7bdfcd", weight = 1.5, fillOpacity = .10, label = ~filename)
      }, error = function(e) {state$search <- conditionMessage(e); notify(e)})
    })
    output$search_status <- shiny::renderText(state$search)
    output$tiles <- DT::renderDT({
      if (is.null(state$tiles)) return(DT::datatable(data.frame(Status = "No search results yet."), rownames = FALSE))
      DT::datatable(sf::st_drop_geometry(state$tiles)[c("filename", "dataset", "acquired_start", "acquired_end", "size_bytes")],
        rownames = FALSE, selection = "multiple", options = list(scrollX = TRUE, pageLength = 8))
    })
    output$sources <- DT::renderDT(DT::datatable(catalog, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 15)))
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
      if (isTRUE(state$comparison_busy)) {shiny::showNotification("Wait for the campaign comparison or cancel it first."); return()}
      if (!is.null(state$preview_job) && state$preview_job$is_alive() && state$preview_target == "als-tile-cloud") {
        shiny::showNotification("Wait for the tile preview before starting another transfer."); return()
      }
      if (!is.null(state$job) && state$job$is_alive()) {shiny::showNotification("A download is already running."); return()}
      shiny::req(state$tiles, input$tiles_rows_selected)
      rows <- state$tiles[input$tiles_rows_selected, , drop = FALSE]
      tryCatch({
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
        destination <- if (mode == "hosted") file.path(jobdir, "files") else input$destination
        if (is.null(destination) || !nzchar(trimws(destination))) stop("Choose a local output directory.")
        state$jobdir <- jobdir; state$destination <- destination; state$finished <- FALSE
        state$job <- callr::r_bg(function(tiles, path, workers, mode, ceiling, progress) {
          alsdownloader::download_tiles(tiles, path, workers = workers, mode = mode,
            provider_limit = ceiling, progress_dir = progress)
        }, args = list(rows, destination, if (mode == "hosted") 1L else input$workers,
          mode, provider_limit, file.path(jobdir, "progress")), supervise = TRUE)
        state$jobtext <- "Download started in a background process."
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
        tryCatch({result <- job$get_result(); state$jobtext <- sprintf("Complete: %s successful, %s failed. See manifest.csv.",
          sum(result$status != "failed"), sum(result$status == "failed"))},
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
      if (isTRUE(state$comparison_busy)) {shiny::showNotification("Wait for the campaign comparison or cancel it first."); return()}
      shiny::req(input$point_file)
      if (!is.null(state$preview_job) && state$preview_job$is_alive()) return()
      state$preview_target <- "als-cloud"
      state$previewtext <- "Reading a bounded point sample..."
      preview_path <- tempfile(fileext = paste0(".", tools::file_ext(input$point_file$name)))
      state$preview_path <- preview_path
      file.copy(input$point_file$datapath, preview_path)
      state$preview_job <- callr::r_bg(function(path, percent, window, x, y, voxel) {
        on.exit(unlink(path))
        alsdownloader:::read_forest_preview(path, percent, window, x, y, voxel)
      }, args = list(preview_path, input$local_percent, as.numeric(input$local_window), input$local_center_x, input$local_center_y, as.numeric(input$local_voxel)), supervise = TRUE)
    })
    shiny::observeEvent(input$plot_tile, {
      if (isTRUE(state$comparison_busy)) {shiny::showNotification("Wait for the campaign comparison or cancel it first."); return()}
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
      state$preview_target <- "als-tile-cloud"
      state$preview_label <- tile$filename[[1]]
      state$tiletext <- paste("Downloading and sampling:", state$preview_label)
      session$sendCustomMessage("als-points", list(target = "als-tile-cloud", points = list(), origin = c(0, 0, 0)))
      state$preview_path <- tempfile(fileext = ".laz")
      tryCatch({state$preview_job <- callr::r_bg(function(tile, path, percent, window, x, y, voxel)
        alsdownloader:::preview_remote_tile(tile, path = path, reader = function(file) alsdownloader:::read_forest_preview(file, percent, window, x, y, voxel)),
        args = list(tile, state$preview_path, input$tile_percent, as.numeric(input$tile_window), input$tile_center_x, input$tile_center_y, as.numeric(input$tile_voxel)), supervise = TRUE)}, error = function(e) {
          if (isTRUE(state$preview_locked)) {release_lock(); state$preview_locked <- FALSE}
          state$tiletext <- "Could not start the preview process."
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
        if (state$preview_target == "als-cloud") state$previewtext <- caption
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
    shiny::observeEvent(input$tile_pose, session$sendCustomMessage("als-view", list(target = "als-tile-cloud", pose = input$tile_pose)))
    shiny::observeEvent(input$local_point_size, session$sendCustomMessage("als-view", list(target = "als-cloud", pointSize = input$local_point_size)))
    shiny::observeEvent(input$tile_point_size, session$sendCustomMessage("als-view", list(target = "als-tile-cloud", pointSize = input$tile_point_size)))
    shiny::observeEvent(input$exaggeration, session$sendCustomMessage("als-view", list(target = "als-cloud", exaggeration = input$exaggeration)))
    shiny::observeEvent(input$tile_exaggeration, session$sendCustomMessage("als-view", list(target = "als-tile-cloud", exaggeration = input$tile_exaggeration)))
    shiny::observeEvent(input$palette, session$sendCustomMessage("als-view", list(target = "als-cloud", palette = input$palette)))
    shiny::observeEvent(input$tile_palette, session$sendCustomMessage("als-view", list(target = "als-tile-cloud", palette = input$tile_palette)))
    shiny::observeEvent(input$tile_fit, session$sendCustomMessage("als-view", list(target = "als-tile-cloud", fit = TRUE)))
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

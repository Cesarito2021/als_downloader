#' Launch the ALS Downloader Shiny application
#' @param mode `"local"` for configurable transfers or `"hosted"` for a serial
#'   download service. Set by the administrator, not by browser input.
#' @param tile_index_dir Optional administrator-supplied OpenTopography or contributed index
#'   directory. Not exposed as a server path control in hosted mode.
#' @param provider_limit Transfer concurrency ceiling after checking provider
#'   terms. Defaults to two; hosted mode always uses one.
#' @param host,port Bind address and optional port passed to [shiny::runApp()].
#' @param launch.browser Whether to open a browser. Defaults to interactive use.
#' @param submission_dir Optional private directory for Zenodo proposals and
#'   approved indexes. No directory is created until a proposal is submitted.
#' @param reviewer Optional trusted maintainer name; enables private review UI
#'   only in local mode with email-link authentication configured by
#'   [configure_reviewer_access()]. Never expose the reviewer app publicly.
#' @return Invisibly, the return value of [shiny::runApp()]. Runs until stopped.
#' @details Launch is explicit: loading the package does not start a browser,
#'   contact providers, change the working directory or install packages.
#'   Background transfer processes are terminated when the owning session ends.
#'   Optional browser submissions use option `alsdownloader.formspree`, a list
#'   with `endpoint` (a public https://formspree.io/f/ form URL) and `attachments`
#'   (FALSE by default; TRUE requires a file-upload plan). Alternatively set
#'   environment variable `ALS_FORMSPREE_ENDPOINT` for links-only submissions.
#'   The community form is the default when no local submission directory is set.
#'   Set option `alsdownloader.formspree = FALSE` to disable remote submission.
#'   An explicitly configured transport takes precedence over the local queue button. It requires
#'   explicit submission and Internet access, but no contributor account.
#'   Service quotas apply; receiving a proposal never approves its publication.
#' @export
#' @examples
#' if (interactive()) launch_app()
launch_app <- function(mode = c("local", "hosted"), tile_index_dir = NULL,
                       provider_limit = 2L, host = "127.0.0.1", port = NULL,
                       launch.browser = interactive(), submission_dir = NULL, reviewer = NULL) {
  if (!is.null(reviewer) && !host %in% c("127.0.0.1", "localhost", "::1"))
    stop("The private reviewer interface must bind to localhost.")
  old <- options(shiny.maxRequestSize = 1024 * 1024^2)
  on.exit(options(old), add = TRUE)
  shiny::runApp(als_app(match.arg(mode), tile_index_dir, provider_limit, submission_dir, reviewer),
                host = host, port = port, launch.browser = launch.browser)
}

#' Create the Shiny application object
#' @inheritParams launch_app
#' @return A `shiny.appobj` suitable for [shiny::runApp()] or a deployment entry point.
#' @export
#' @examples
#' if (interactive()) shiny::runApp(als_app())
als_app <- function(mode = "local", tile_index_dir = NULL, provider_limit = 2L, submission_dir = NULL, reviewer = NULL) {
  mode <- match.arg(mode, c("local", "hosted"))
  valid_setting <- function(x) is.character(x) && length(x)==1L && !is.na(x) && nzchar(trimws(x))
  if (!is.null(submission_dir) && !valid_setting(submission_dir)) stop("Use a private submission directory path.")
  if (!is.null(reviewer) && !valid_setting(reviewer)) stop("Identify the reviewing maintainer.")
  if (!is.null(reviewer) && (mode != "local" || is.null(submission_dir)))
    stop("Private review requires local mode and a configured submission_dir.")
  assets <- system.file("app", "www", package = "alsdownloader")
  shiny::addResourcePath("als-assets", assets)
  shiny::addResourcePath("als-data", system.file("extdata", package = "alsdownloader"))
  catalog <- provider_catalog()
  review_access <- reviewer_access_controller(submission_dir)
  overview <- discovery_coverage(tile_index_dir,
    if (is.null(submission_dir)) NULL else file.path(submission_dir, "approved"))
  cores <- as.numeric(parallelly::availableCores())
  policy <- download_worker_policy(mode, cores)
  world <- sf::st_read(system.file("extdata", "world-countries.geojson", package = "alsdownloader"), quiet = TRUE)
  world$code <- suppressWarnings(as.numeric(world$id))
  # One lock across sessions served by this app process. Multi-process hosting
  # must configure ALS_HOST_LOCK_DIR to a shared writable location.
  hosted_lock <- Sys.getenv("ALS_HOST_LOCK_DIR", file.path(tempdir(), "als-host-transfer-lock"))
  ui <- shiny::fluidPage(
    shiny::tags$head(shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      shiny::tags$meta(name="referrer",content="strict-origin-when-cross-origin"),
      shiny::tags$link(rel = "stylesheet", href = "als-assets/explorer.css"),
      shiny::tags$script(src = "als-assets/html2canvas.js"),
      shiny::tags$script(src = "als-assets/figures.js"),
      shiny::tags$script(src = "als-assets/report-map.js"),
      shiny::tags$script(src = "als-assets/app.js"),
      shiny::tags$script(src = "als-assets/profile.js"),
      shiny::tags$script(src = "als-assets/preview.js"),
      shiny::tags$script(src = "als-assets/globe.js")),
    shiny::div(class = "als-header", shiny::div(shiny::h1("ALS DOWNLOADER"),
      shiny::span("Discover, inspect and download airborne LiDAR")),
      shiny::div(class = "als-header-actions",
        shiny::actionButton("suggest_source", "Submit ALS data - Zenodo"),
        shiny::conditionalPanel("input.enter_map > 0", shiny::actionLink("open_catalogue", "Source catalogue")),
        shiny::span(class = "mode-label", paste(toupper(mode), "MODE")))),
    shiny::div(class = "als-layout",
      shiny::conditionalPanel("input.enter_map > 0", class = "als-sidebar-toggle",
        shiny::tags$details(class = "als-sidebar", open = "open",
        shiny::tags$summary("Options"),
        shiny::h3(class = "als-sidebar-heading", "Search inputs"),
        shiny::div(class = "als-section",
          shiny::radioButtons("aoi_method", "Area of interest (AOI)", c("Draw on the map" = "draw", "Upload a file" = "upload"), selected = "draw", inline = TRUE),
          shiny::conditionalPanel("input.aoi_method == 'upload'",
            shiny::fileInput("aoi_file", "Upload AOI", accept = c(".zip", ".gpkg", ".geojson", ".json", ".fgb")),
            shiny::uiOutput("layer_control"),
            shiny::helpText("ZIP uploads must include Shapefile companion files.")),
          shiny::conditionalPanel("input.aoi_method == 'draw'",
            shiny::helpText("Geometry: polygon or rectangle.")),
          shiny::textOutput("aoi_status"),
          shiny::actionButton("reset_aoi", "Clear AOI", class = "als-reset-btn")),
        shiny::div(class = "als-section",
          shiny::dateRangeInput("dates", "Acquisition period", start = "2000-01-01", end = Sys.Date()),
          shiny::actionButton("search", "Find ALS data", class = "als-action-btn")),
        shiny::h3(class = "als-sidebar-heading", "Outputs"),
            shiny::conditionalPanel("output.report_ready === 'yes'", class="als-section als-report-section",
              shiny::downloadButton("download_report_pdf", "Download report (PDF)", class="als-report-btn"),
              shiny::textOutput("report_selection_summary"),
            shiny::tags$details(shiny::tags$summary("Report options"),
              shiny::checkboxInput("report_rgb", "OpenStreetMap basemap", TRUE),
              shiny::downloadButton("download_report_map", "Download aoi map (PNG)"),
              shiny::tags$div(id = "report_map_status", role = "status", `aria-live` = "polite"),
              shiny::checkboxInput("report_details", "Include technical appendix (file sample and download-time scenarios)", FALSE),
              shiny::fileInput("report_figures", "Optional exported PNG figures (up to 6, 10 MiB each)", multiple = TRUE, accept = ".png"),
              shiny::helpText("The report uses selected tiles, or all search results if none are selected."))),
        shiny::div(class = "als-section",
          shiny::h4(class = "als-options-heading", "Download options"),
          if (mode == "local") shiny::tagList(
            shiny::textInput("destination", "Output folder", value = tempdir()),
            shiny::numericInput("workers", "Download workers", min(4L, policy$maximum), min = 1, max = policy$maximum),
            shiny::helpText(paste("Recommended:", policy$recommended, "| maximum:", policy$maximum, "| provider ceiling:", provider_limit)))
          else shiny::helpText("Hosted downloads use one worker. Select up to 10 tiles per batch. Files are delivered through the browser."),
          shiny::actionButton("download", "Download selected tiles", class = "als-action-btn"),
          shiny::textOutput("selection_summary"),
          shiny::helpText("Downloads preserve complete original tiles.")),
        shiny::conditionalPanel("output.download_visible === 'yes'", class = "als-section als-transfer-section",
          shiny::h4("Download status"),
          shiny::textOutput("job_status"),
          shiny::conditionalPanel("output.download_running === 'yes'", shiny::actionButton("cancel", "Cancel download")),
          shiny::uiOutput("bundle_control")),
        shiny::actionButton("reset_all", "Reset workspace", class = "als-reset-btn"))),
      shiny::div(class = "als-main",
        shiny::tabsetPanel(id = "view",
          shiny::tabPanel("Explore",
            shiny::conditionalPanel("input.enter_map == 0",
              shiny::div(class = "als-globe-intro",
                shiny::div(class = "als-globe-wrap",
                  shiny::tags$canvas(id = "als-globe", tabindex = "0", role = "img", `aria-label` = "Representative world globe. Red countries have in-app access; yellow countries have external access. Country colours are not survey footprints. Explorer shows actual coverage.",
                    `data-adapters` = jsonlite::toJSON(unique(catalog$country_code[catalog$implemented]), auto_unbox = FALSE), `data-external` = jsonlite::toJSON(unique(catalog$country_code[!catalog$implemented]), auto_unbox = FALSE)),
                  shiny::tags$div(class = "als-globe-legend",
                    shiny::tags$div(class = "als-globe-legend-row", shiny::tags$span(class = "als-globe-swatch als-globe-swatch-red"), "In-App Access"),
                    shiny::tags$div(class = "als-globe-legend-row", shiny::tags$span(class = "als-globe-swatch als-globe-swatch-yellow"), "External Access"))),
                shiny::p(id = "globe_status", class = "als-globe-caption", "Country colours represent access routes. Open Explorer to see survey coverage."),
                shiny::p(class = "als-globe-mission",
                  "Find airborne LiDAR by area, view point clouds and download original files from their providers."),
                shiny::p(class = "als-globe-author", "Developed by Cesar Alvites"),
                shiny::div(class = "als-welcome-buttons", role = "group", `aria-label` = "Explore ALS Downloader",
                  shiny::actionButton("enter_map", "Open map", icon = shiny::icon("map"), class = "als-welcome-button als-welcome-map"),
                  shiny::actionButton("welcome_about", "About the project", icon = shiny::icon("book-open"), class = "als-welcome-button als-welcome-guide"),
                  shiny::actionButton("welcome_catalogue", "Source catalogue", icon = shiny::icon("layer-group"), class = "als-welcome-button als-welcome-sources")),
                shiny::div(class = "als-globe-actions",
                  shiny::tags$button(id = "globe_reset", type = "button", class = "btn", "Restart rotation")),
                shiny::p(class = "als-globe-credits", "Globe outlines: ",
                  shiny::tags$a(href = "https://www.naturalearthdata.com/about/terms-of-use/", target = "_blank", rel = "noopener noreferrer", "Natural Earth"), " | ",
                  shiny::tags$a(href = "https://github.com/Cesarito2021/als_downloader", target = "_blank", rel = "noopener noreferrer", "GitHub")),
                shiny::div(class = "als-author-profile",
                  shiny::tags$img(src = "als-assets/cesar.jpg", alt = "Cesar Alvites", loading = "lazy", width = "104", height = "104"),
                  shiny::div(shiny::tags$a(href = "https://cesarito2021.github.io/", target = "_blank", rel = "noopener noreferrer", "Cesar Alvites"),
                    shiny::p("School of Forest, Fisheries, and Geomatics Sciences", shiny::tags$br(), "University of Florida"))))),
            shiny::uiOutput("welcome_information"),
            shiny::conditionalPanel("input.enter_map > 0", leaflet::leafletOutput("map", height = "60vh"),
            figure_button("export_map_png", "Download map (PNG)"),
            shiny::tags$div(style="display:none", shiny::textOutput("map_source_credits")),
            shiny::checkboxInput("map_export_basemap", "OpenStreetMap basemap in image", TRUE),
            shiny::tags$span(id="map_export_status", role="status"),
            shiny::h3("Search results"),
            shiny::textOutput("search_status"),
            shiny::conditionalPanel("output.report_ready === 'yes'",
              shiny::div(class="als-campaign-selection",
                shiny::h4("Select by acquisition year and campaign"),
                shiny::selectInput("campaign_year", "Acquisition year", choices=c("All years"="all")),
                shiny::selectInput("campaign_projects", "Campaigns", choices=c("All campaigns"="all"), selected="all"),
                shiny::textOutput("campaign_selection_summary"),
                shiny::actionButton("select_campaign_tiles", "Select these tiles", icon=shiny::icon("check-double")),
                shiny::helpText("Replaces the current selection across all result pages. Then use Download selected tiles. Multi-year surveys appear in each reported year; missing dates remain Unknown."))),
            shiny::div(class = "als-result-actions",
              shiny::tags$button(id="toggle_tile_filters", type="button", class="btn", `aria-expanded`="false", `aria-controls`="tiles", shiny::icon("filter"), " Filter"),
              shiny::actionButton("select_all_tiles", "Select filtered", icon=shiny::icon("check-double")),
              shiny::actionButton("clear_tiles", "Clear selection", icon=shiny::icon("xmark")),
              shiny::actionButton("tile_licenses", "License", icon=shiny::icon("scale-balanced")),
              shiny::actionButton("tile_information", "Product info", icon=shiny::icon("circle-info"))),
            shiny::div(class="als-results-table", DT::DTOutput("tiles")),
            shiny::div(class = "als-result-actions",
              shiny::downloadButton("export_manifest", "Export all tile metadata"),
              shiny::downloadButton("export_selection", "Export selected tile metadata"),
              shiny::downloadButton("export_script", "Export download R script")),
            shiny::div(class = "als-tile-preview",
              shiny::actionButton("plot_tile", "View selected tile in 3D", class = "als-primary"),
              shiny::textOutput("tile_selection"),
              shiny::helpText(if(mode == "hosted") "Select one LAS/LAZ tile or ZIP. Temporary download: up to 250 MiB; ZIP contents: up to 500 MiB." else "Select one LAS/LAZ tile or ZIP. Opens in 3D view (download up to 1 GiB; ZIP contents up to 2 GiB).")))),
          shiny::tabPanel("3D view",
            shiny::p("One viewer for the tile selected in Explore or a local LAS/LAZ file."),
            shiny::actionButton("replot_tile", "View selected map tile"),
            shiny::uiOutput("zip_member_control"),
            shiny::fileInput("point_file", "Upload a local LAS/LAZ tile (up to 1 GB)", accept = c(".las", ".laz")),
            forest_preview_controls("local_"),
            shiny::actionButton("preview", "View point cloud"),
            shiny::actionButton("cancel_preview", "Cancel loading"),
            shiny::helpText("Display sampling preserves the source file. Large local tiles can also be read with read_preview() in R."),
            shiny::textOutput("preview_status"),
            shiny::tags$canvas(id = "als-cloud", role = "img", tabindex = "0", `aria-label` = "Interactive point-cloud preview. Arrow keys rotate; plus and minus zoom; zero resets."),
            figure_button("export_preview_png", "Download point cloud view (PNG)", TRUE),
            shiny::tags$span(id="preview_export_status", role="status"),
            shiny::selectInput("colour_by", "Colour by", c("Automatic"="auto", "Source classification"="classification", "Intensity"="intensity", "Elevation"="elevation"), selected="auto"),
            shiny::selectInput("palette", "Palette for intensity / elevation", preview_palettes(), selected="Greyscale"),
            shiny::helpText("Automatic uses source classes when any labelled classes are present, then non-zero intensity, then elevation. Classification has fixed categorical colours. Intensity is raw sensor return strength, not calibrated reflectance."),
            shiny::sliderInput("exaggeration", "Vertical scale factor", min = 1, max = 12, value = 1, step = 1),
            shiny::helpText("Vertical scale factor changes display only: 1x is true proportions; 2x doubles vertical differences. Source elevations remain unchanged."),
            shiny::div(class = "map-caption", "Drag or arrow keys to rotate | scroll or +/- to zoom | 0 to reset. Classification colours use the labels supplied in the file; no automatic classification. Elevation colours show source Z, not canopy height.")),
          comparison_ui(),
          NULL)))
  )
  server <- function(input, output, session) {
    output$ot_access_audit <- shiny::downloadHandler(
      filename=function()"opentopography-access-audit.csv",
      content=function(file)ot_export_audit(file))
    if (!is.null(submission_dir)) overview <- discovery_coverage(
      tile_index_dir, file.path(submission_dir, "approved"))
    state <- shiny::reactiveValues(aoi = NULL, tiles = NULL, search = "No search results yet.",
      job = NULL, jobdir = NULL, destination = NULL, jobtext = "No active download.", finished = FALSE,
      preview_job = NULL, preview_started = NULL, previewtext = "Upload one tile to inspect its structure.", lock_owned = FALSE,
      preview_target = "als-cloud", tiletext = "Select exactly one tile to preview.", preview_label = "", preview_attribution = NULL, preview_path = NULL, preview_locked = FALSE,
      tile_groups = character(0), zip_members = character(), zip_url = NULL)
    notify <- function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = 12)
    welcome_information <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$welcome_about, { welcome_information("about") })
    shiny::observeEvent(c(input$welcome_catalogue, input$open_catalogue), ignoreInit = TRUE, {
      shiny::updateTabsetPanel(session, "view", selected = "Explore")
      welcome_information("catalogue")
    })
    shiny::observeEvent(input$close_information, { welcome_information(NULL) })
    shiny::observeEvent(input$enter_map, { welcome_information(NULL) })
    output$welcome_information <- shiny::renderUI({
      section <- welcome_information()
      if (is.null(section)) return(NULL)
      shiny::tags$section(class = "als-inline-information", `aria-labelledby` = "welcome_information_title",
        shiny::div(class = "als-information-heading",
          shiny::h2(id = "welcome_information_title", tabindex = "-1", if(section == "about") "About the project" else "Source catalogue"),
          shiny::actionButton("close_information", "Close")),
        if(section == "about") shiny::tagList(
          shiny::p("ALS Downloader connects existing airborne LiDAR catalogues: define an AOI, find surveys, view point clouds and download original files from their providers."),
        shiny::p("Coverage, acquisition dates and classifications depend on source metadata. Visual comparisons support inspection; they do not measure change."),
          shiny::p("Developed by Cesar Alvites. Software: GPL-3. Data and basemaps retain their own licences and credits."),
          shiny::p("Interactive maps use Leaflet and the leaflet R package. Geographic outlines: Natural Earth / World Atlas. We acknowledge the data producers and access services identified in the source catalogue and exported metadata."),
          shiny::p("This work is based on API services provided by the OpenTopography Facility with support from the National Science Foundation under NSF Award Numbers 2410799, 2410800 & 2410801."),
        shiny::p("OpenForest4D is funded by NSF awards 2409885, 2409886 & 2409887."),
        shiny::tags$a(href = "https://github.com/Cesarito2021/als_downloader#readme", target = "_blank", rel = "noopener noreferrer", "Read the project guide")
        ) else shiny::tagList(
          shiny::p("Discovery covers aircraft, helicopter and UAV laser scanning. Research deposits require reviewed coverage information; author-declared approximate extents are labelled and may include areas without points. Official national portals also provide external access; find a country in the table below for its official source link. Terrestrial, spaceborne and photogrammetric acquisitions are outside the curated selection. Only providers marked Implemented have an in-app search adapter. Verify dataset terms and citations before downloading."),
            shiny::tags$a(href = "https://github.com/Cesarito2021/als_downloader/issues/new?template=suggest-dataset.yml", target = "_blank", rel = "noopener noreferrer", "Open the GitHub source suggestion form"),
            shiny::p("Software: GPL-3. The screenshot library html2canvas is MIT-licensed; its notice is included. Dataset and basemap licences remain separate. Keep source credits and licence links with figures and downloads; scientific use does not waive provider terms. Local-file and uploaded boundary rights must be checked with their source."),
            shiny::downloadButton("licensing_notes", "Download licence guidance and software notices"),
            shiny::p("OpenTopography: verified hosted airborne-LiDAR tile indexes are searched automatically. Other hosted collections and Community Dataspace records retain external source links in the access audit."),
            shiny::downloadLink("ot_access_audit","Download OpenTopography access audit (CSV)"),
            shiny::textInput("catalogue_search", "Find a source or country", placeholder = "e.g. France, USGS, OpenTopography"),
            shiny::uiOutput("source_cards"),
            shiny::tags$details(class = "als-source-table", shiny::tags$summary("View detailed source table"), DT::DTOutput("sources"))
        ))
    })
    shiny::observeEvent(input$enter_map, ignoreNULL = FALSE, {
      session$sendCustomMessage("als-toggle-class",
        list(selector = ".als-layout", class = "als-map-open", on = isTRUE(input$enter_map > 0)))
    })
    source_check_summary <- source_preflight_server(input, output, session, state, mode, hosted_lock)
    source_submission_server(input, output, session, source_check_summary)
    zenodo_submission_server(input, output, session, submission_dir, reviewer, review_access)
    if (!is.null(submission_dir)) {
      approved_dir <- file.path(submission_dir, "approved")
      last_coverage <- approved_coverage_signature(approved_dir)
      shiny::observe({
        shiny::invalidateLater(3000, session)
        signature <- approved_coverage_signature(approved_dir)
        if (identical(signature, last_coverage)) return()
        updated <- discovery_coverage(tile_index_dir, approved_dir)
        proxy <- leaflet::leafletProxy("map", session=session) |>
          leaflet::clearGroup("In-App Access")
        add_in_app_coverage(proxy, updated)
        last_coverage <<- signature
      })
    }
    comparison_server(input, output, session, state, mode, hosted_lock)
    output$map <- leaflet::renderLeaflet({
      map <- leaflet::leaflet(world, options=leaflet::leafletOptions(preferCanvas=TRUE)) |>
        leaflet::addMapPane("aoi-outline", zIndex = 450) |>
        leaflet::addTiles("https://tile.openstreetmap.org/{z}/{x}/{y}.png",
          group = "OpenStreetMap", attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors',
          options = leaflet::tileOptions(maxZoom = 19, crossOrigin = "anonymous", keepBuffer = 0)) |>
        leaflet::addPolygons(layerId = ~id, group = "Countries", color = "#60717c", weight = .5,
          fill = FALSE, label = ~name,
          options = leaflet::pathOptions(interactive = FALSE)) |>
        leaflet.extras::addDrawToolbar(targetGroup = "AOI", polygonOptions = leaflet.extras::drawPolygonOptions(showArea = TRUE),
          rectangleOptions = leaflet.extras::drawRectangleOptions(), polylineOptions = FALSE,
          markerOptions = FALSE, circleOptions = FALSE, circleMarkerOptions = FALSE,
          editOptions = leaflet.extras::editToolbarOptions()) |>
        leaflet::addLayersControl(baseGroups = "OpenStreetMap", overlayGroups = discovery_groups()) |>
        leaflet::addScaleBar(position="bottomleft", options=leaflet::scaleBarOptions(imperial=FALSE)) |>
        leaflet::setView(0, 20, 2, options = list(animate = FALSE))
      add_discovery_layers(map, world, catalog, overview)
    })
    ot_view <- shiny::debounce(shiny::reactive(list(bounds=input$map_bounds,zoom=input$map_zoom)),500)
    shiny::observeEvent(ot_view(), {
      view <- ot_view()
      shiny::req(view$bounds,view$zoom)
      tryCatch(add_ot_map_coverage(leaflet::leafletProxy("map",session=session),view$zoom,view$bounds),
        error=function(e) {
          add_ot_map_coverage(leaflet::leafletProxy("map",session=session))
          shiny::showNotification(paste("Detailed coverage unavailable; survey locations remain visible.",conditionMessage(e)),
            type="warning",id="ot-detail-unavailable",duration=10)
        })
    },ignoreInit=TRUE)
    # Tiles are split into one Leaflet group per acquisition year (see the
    # search handler) so the existing layers control can toggle a single
    # year on/off; clearing them all back to just "Countries"/"AOI"
    # has to walk whatever group names the last search created.
    clear_tile_layers <- function() {
      p <- leaflet::leafletProxy("map")
      for (g in state$tile_groups) p <- p |> leaflet::clearGroup(g)
      p |> leaflet::removeControl("tile_year_legend") |>
        leaflet::addLayersControl(baseGroups = "OpenStreetMap",
          overlayGroups = discovery_groups())
      state$tile_groups <- character(0)
    }
    set_aoi <- function(x) {
      state$aoi <- read_aoi(x); state$tiles <- NULL
      state$search <- "AOI updated. Search to verify tile coverage."
      bb <- sf::st_bbox(state$aoi)
      clear_tile_layers()
      leaflet::leafletProxy("map") |> leaflet::clearGroup("AOI") |>
        leaflet::addPolygons(data = state$aoi, group = "AOI", color = "#ffe4a3", weight = 3, dashArray = "8,5", fillOpacity = 0,
          options = leaflet::pathOptions(pane = "aoi-outline")) |>
        leaflet::fitBounds(bb[[1]], bb[[2]], bb[[3]], bb[[4]], options = list(animate = FALSE))
    }
    clear_aoi <- function() {
      state$aoi <- NULL; state$tiles <- NULL
      state$search <- "No search results yet."
      shiny::updateRadioButtons(session, "aoi_method", selected = "draw")
      clear_tile_layers()
      leaflet::leafletProxy("map") |>
        leaflet.extras::removeDrawToolbar(clearFeatures = TRUE) |>
        leaflet::clearGroup("AOI") |>
        leaflet.extras::addDrawToolbar(targetGroup = "AOI",
          polygonOptions = leaflet.extras::drawPolygonOptions(showArea = TRUE),
          rectangleOptions = leaflet.extras::drawRectangleOptions(), polylineOptions = FALSE,
          markerOptions = FALSE, circleOptions = FALSE, circleMarkerOptions = FALSE,
          editOptions = leaflet.extras::editToolbarOptions()) |>
        leaflet::setView(0, 20, 2, options = list(animate = FALSE))
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
      tryCatch(set_aoi(leaflet_draw_feature_to_sf(input$map_draw_new_feature)), error = notify)
    })
    shiny::observeEvent(input$map_draw_edited_features, {
      tryCatch(set_aoi(leaflet_draw_collection_to_sf(input$map_draw_edited_features)), error = notify)
    })
    shiny::observeEvent(input$map_draw_deleted_features, {
      state$aoi <- NULL; state$tiles <- NULL; state$search <- "AOI removed."
      clear_tile_layers()
      leaflet::leafletProxy("map") |> leaflet::clearGroup("AOI")
    })
    navigate <- function(id) {
      if (!nzchar(id)) {leaflet::leafletProxy("map") |> leaflet::setView(0, 20, 2, options = list(animate = FALSE)); return()}
      row <- world[which(world$code == suppressWarnings(as.numeric(id))), ]
      if (nrow(row)) {bb <- sf::st_bbox(row); leaflet::leafletProxy("map") |> leaflet::fitBounds(bb[[1]], bb[[2]], bb[[3]], bb[[4]], options = list(animate = FALSE))}
    }
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
    output$aoi_status <- shiny::renderText(if (is.null(state$aoi)) "No AOI selected." else {
      area <- aoi_area(state$aoi)
      paste0(sprintf("AOI: %.4f km^2", area), if (area > 1000)
        ". Large-area search: results are limited to 10,000 tiles per source. Split state-wide areas into smaller regions if a source reaches its limit. 3D shows one tile; comparison shows only a 100-1000 m window, not the whole state." else "")
    })
    shiny::observeEvent(input$search, {
      shiny::req(state$aoi)
      state$tiles <- NULL
      clear_tile_layers()
      state$search <- "Searching all configured sources..."
      local_index_dir <- tile_index_dir
      has_local_index <- !is.null(local_index_dir) && nzchar(trimws(local_index_dir))
      providers <- c("usgs3dep", "ahn6", "swisstopo", "ignfr", "canelevation", "opentopography")
      if (has_local_index) {
        files <- list.files(local_index_dir, ignore.case=TRUE)
        if(any(grepl("\\.tiles\\.geojson$",files,ignore.case=TRUE))) providers <- c(providers,"contributed")
      }
      approved_dir <- if (is.null(submission_dir)) NULL else file.path(submission_dir, "approved")
      if (!is.null(approved_dir) && length(list.files(approved_dir, "\\.tiles\\.geojson$")))
        providers <- c(providers, "zenodo-approved")
      tryCatch({
        outcome <- shiny::withProgress(message = "Searching sources", value = .2, {
          out <- lapply(providers, function(p) tryCatch(
            find_tiles(state$aoi, if (p == "zenodo-approved") "contributed" else p,
              as.character(input$dates[1]), as.character(input$dates[2]),
              if (p == "zenodo-approved") approved_dir else if(p %in% c("canelevation","opentopography")) NULL else local_index_dir),
            error = function(e) e))
          names(out) <- providers
          out
        })
        ok <- vapply(outcome, inherits, logical(1), "sf")
        result <- if (any(ok)) do.call(rbind, outcome[ok]) else empty_tiles()
        if (nrow(result)) result <- result[!duplicated(paste(result$provider,redact_url(result$url))),,drop=FALSE]
        state$tiles <- result
        state$search <- if (nrow(result))
            paste0(nrow(result), " intersecting tiles from ", length(unique(result$provider)), " source(s); ", length(providers), " sources searched. Select rows below; acquisition dates may be unknown.")
          else "No matching records in the searched sources and interval. This does not establish that no LiDAR data exist here."
        if (any(!ok)) state$search <- paste0(state$search, " Incomplete search. ", paste(vapply(which(!ok), function(i)
          paste0(names(outcome)[i], ": ", redact_urls_in_text(conditionMessage(outcome[[i]]))), character(1)), collapse = " | "))
        if (nrow(result)) {
          # Colour and group footprints by acquisition year (final date; start
          # when the end date is unknown) instead of one flat colour, so
          # overlapping surveys from different years are visually
          # distinguishable -- and, since each year is its own Leaflet group,
          # a user can hide/show one year at a time from the existing layers
          # control instead of reading a blended legend. A discrete palette
          # (one swatch per real year present, plus "Unknown") is used
          # instead of a continuous one, which produced a blank legend when
          # a search returned only one distinct year or only undated tiles.
          reported <- ifelse(is.na(result$acquired_end), result$acquired_start, result$acquired_end)
          year_num <- suppressWarnings(as.integer(substr(reported, 1, 4)))
          year_label <- ifelse(is.na(year_num), "Unknown", as.character(year_num))
          levels_all <- c(sort(unique(year_label[year_label != "Unknown"])),
            if ("Unknown" %in% year_label) "Unknown")
          pal <- leaflet::colorFactor("viridis", domain = levels_all)
          proxy <- leaflet::leafletProxy("map")
          groups <- character(0)
          for (lv in levels_all) {
            idx <- which(year_label == lv)
            grp <- paste0("Tiles: ", lv)
            groups <- c(groups, grp)
            proxy <- proxy |> leaflet::addPolygons(data = result[idx, , drop = FALSE], group = grp,
              layerId = paste0("tile:", idx), color = "#1c2b35", weight = 1,
              fillColor = pal(lv), fillOpacity = .55, label = ~filename)
          }
          state$tile_groups <- groups
          proxy |>
            leaflet::addLegend("bottomright", layerId = "tile_year_legend",
              pal = pal, values = levels_all, title = "Acquisition year") |>
            leaflet::addLayersControl(baseGroups = "OpenStreetMap",
              overlayGroups = c(discovery_groups(), groups))
        }
      }, error = function(e) {state$search <- conditionMessage(e); notify(e)})
    })
    output$search_status <- shiny::renderText(state$search)
    shiny::observeEvent(state$tiles, {
      years <- sort(unique(unlist(tile_year_membership(state$tiles))))
      known <- years[years != "unknown"]
      choices <- c("All years"="all", stats::setNames(known, known),
        if ("unknown" %in% years) c("Unknown dates"="unknown"))
      shiny::updateSelectInput(session, "campaign_year", choices=choices, selected="all")
    }, ignoreNULL=FALSE)
    shiny::observeEvent(list(state$tiles, input$campaign_year), {
      groups <- selection_campaign_groups(state$tiles, if(is.null(input$campaign_year)) "all" else input$campaign_year)
      choices <- c("All campaigns"="all", if(length(groups)) stats::setNames(names(groups),
        paste0(names(groups), " | ", lengths(groups), " tiles")))
      shiny::updateSelectInput(session, "campaign_projects", choices=choices, selected="all")
    }, ignoreNULL=FALSE)
    campaign_rows <- shiny::reactive({
      campaign_tile_rows(state$tiles, if(is.null(input$campaign_year)) "all" else input$campaign_year,
        input$campaign_projects)
    })
    output$campaign_selection_summary <- shiny::renderText({
      paste(length(campaign_rows()), "tiles match this year and campaign choice.")
    })
    shiny::observeEvent(input$select_campaign_tiles, {
      shiny::req(state$tiles)
      DT::selectRows(DT::dataTableProxy("tiles"), campaign_rows())
    })
    output$tiles <- DT::renderDT({
      if (is.null(state$tiles)) return(DT::datatable(data.frame(Status = "No search results yet."), rownames = FALSE))
      table <- sf::st_drop_geometry(state$tiles)
      table$campaign_id <- if ("campaign_id" %in% names(table)) ifelse(is.na(table$campaign_id), "Not supplied", table$campaign_id) else rep("Not supplied", nrow(table))
      DT::datatable(table[c("filename", "campaign_id", "dataset", "provider", "acquired_end", "acquired_start", "size_bytes", "license_url", "citation")],
        colnames = c("File", "Campaign", "Dataset", "Source adapter", "Collection date (end)", "Collection start", "Size (bytes)", "License", "Producer / citation"),
        rownames = FALSE, selection = "multiple", filter="top", class="stripe hover compact",
        options = list(scrollX = TRUE, pageLength = 8,
          columnDefs=list(list(targets=c(2, 3, 5, 7, 8), visible=FALSE))))
    })
    output$sources <- DT::renderDT(DT::datatable(catalog, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 15)))
    output$source_cards <- shiny::renderUI({
      query <- if (is.null(input$catalogue_search)) "" else tolower(trimws(input$catalogue_search))
      rows <- catalog[grepl(query, tolower(paste(catalog$name, catalog$country)), fixed = TRUE), , drop = FALSE]
      if (!nrow(rows)) return(shiny::p(role = "status", "No matching sources. Try another name or country."))
      rows <- rows[order(rows$id != "usgs3dep", !rows$implemented, rows$country, rows$name), , drop = FALSE]
      shiny::tagList(shiny::p(role = "status", paste(nrow(rows), "sources listed. Check each source's access conditions.")),
        shiny::div(class = "als-source-grid", lapply(seq_len(nrow(rows)), function(i) {
          source <- rows[i, ]
          shiny::tags$article(class = "als-source-card",
            shiny::div(class = "als-source-card-top", shiny::span(class = "als-source-icon", `aria-hidden` = "true", shiny::icon("plane")),
              shiny::span(class = if (source$implemented) "als-source-status available" else "als-source-status", if (source$implemented) "In-app adapter" else "External portal")),
            shiny::h3(source$name), shiny::p(class = "als-source-country", source$country),
            shiny::tags$details(shiny::tags$summary("Access and availability"), shiny::p(source$access)),
            shiny::tags$a(href = source$info_url, target = "_blank", rel = "noopener noreferrer", paste("View source:", source$name)))
        })))
    })
    selected_tiles <- shiny::reactive({
      shiny::req(state$tiles)
      ids <- input$tiles_rows_selected
      ids <- ids[ids %in% seq_len(nrow(state$tiles))]
      state$tiles[ids, , drop = FALSE]
    })
    report_tiles <- shiny::reactive({x<-selected_tiles();if(nrow(x))x else state$tiles})
    output$report_selection_summary<-shiny::renderText({
      if(is.null(state$tiles)||!nrow(state$tiles))return("Search for ALS data to prepare a report. No download is required.")
      x<-report_tiles();known<-is.finite(x$size_bytes)
      paste(nrow(x),if(nrow(x)==1L)"tile |" else "tiles |",round(sum(x$size_bytes[known])/1024^3,2),"GiB known |",sum(!known),"unknown file sizes. Select tiles to narrow the report.")
    })
    output$report_ready <- shiny::renderText(if (!is.null(state$tiles) && nrow(state$tiles) > 0L) "yes" else "no")
    output$download_visible <- shiny::renderText(if (!is.null(state$job)) "yes" else "no")
    output$download_running <- shiny::renderText(if (!is.null(state$job) && !isTRUE(state$finished)) "yes" else "no")
    for (id in c("report_ready", "download_visible", "download_running"))
      shiny::outputOptions(output, id, suspendWhenHidden = FALSE)
    report_map <- report_map_server(input, output, session, shiny::reactive(state$aoi), report_tiles)
    shiny::observeEvent(input$select_all_tiles, {shiny::req(state$tiles); DT::selectRows(DT::dataTableProxy("tiles"), input$tiles_rows_all)})
    shiny::observeEvent(input$clear_tiles, DT::selectRows(DT::dataTableProxy("tiles"), integer()))
    show_tile_details <- function(licenses=FALSE) {
      if (is.null(state$tiles) || !nrow(state$tiles)) {
        shiny::showNotification("Find ALS data first.", type="message"); return()
      }
      x <- selected_tiles()
      if (!nrow(x)) {
        shiny::showNotification("Select one or more tiles first.", type="message"); return()
      }
      details <- unique(sf::st_drop_geometry(x)[c("dataset", "provider", "citation", "license_url")])
      shiny::showModal(shiny::modalDialog(title=if(licenses) "License and attribution" else "Product information",
        shiny::p(paste(nrow(x), "selected tiles")),
        lapply(seq_len(nrow(details)), function(i) shiny::tags$section(
          shiny::h4(details$dataset[i]),
          if (!licenses) shiny::p(paste("Source:", details$provider[i])),
          shiny::p(details$citation[i]),
          if (!is.na(details$license_url[i]) && grepl("^https://", details$license_url[i]))
            shiny::tags$a(href=details$license_url[i], target="_blank", rel="noopener noreferrer", "Read source license")
          else shiny::p("License not supplied; check the source."))),
        easyClose=TRUE, footer=shiny::modalButton("Close")))
    }
    shiny::observeEvent(input$tile_licenses, show_tile_details(TRUE))
    shiny::observeEvent(input$tile_information, show_tile_details(FALSE))
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
    write_report <- function(file, format) {
      x <- report_tiles(); shiny::req(nrow(x))
      if (format == "pdf" && !(requireNamespace("tinytex", quietly = TRUE) && isTRUE(tinytex::is_tinytex()))) {
        shiny::showNotification("PDF reports require a working TinyTeX installation on the app server.", type = "error", duration = 15)
        stop("PDF reports require a working TinyTeX installation.", call. = FALSE)
      }
      area <- if (is.null(state$aoi)) NA_real_ else aoi_area(state$aoi)
      dir <- tempfile("als-report-"); dir.create(dir)
      on.exit(unlink(dir, recursive = TRUE), add = TRUE)
      figures <- if (is.null(input$report_figures)) character() else input$report_figures$datapath
      rgb <- if (isTRUE(input$report_rgb)) report_map() else list(path = character(), credits = "")
      path <- tryCatch(als_report(x, dir, format = format, aoi_area_km2 = area, aoi = state$aoi, figures = figures,
        details = isTRUE(input$report_details), map_image = rgb$path, map_credits = rgb$credits),
        error = function(e) {shiny::showNotification(conditionMessage(e), type = "error", duration = 15); stop(e)})
      file.copy(path, file, overwrite = TRUE)
    }
    output$download_report_pdf <- shiny::downloadHandler(filename = "als-session-report.pdf", contentType = "application/pdf",
      content = function(file) write_report(file, "pdf"))
    output$tile_selection <- shiny::renderText({
      selected <- input$tiles_rows_selected
      if (is.null(state$tiles) || length(selected) != 1L || !selected %in% seq_len(nrow(state$tiles)))
        "Select exactly one result for the 3D view."
      else paste("Selected:", state$tiles$filename[selected])
    })
    output$map_source_credits <- shiny::renderText(paste(c(figure_attribution(state$tiles),
      if("citation" %in% names(overview)) unique(overview$citation),
      unique(ot_registry()$citation),
      paste("OpenTopography dataset terms:",paste(unique(ot_registry()$license_url),collapse="; ")),
      if("license_url" %in% names(overview)) paste("Coverage licence:",unique(overview$license_url[nzchar(overview$license_url)]))), collapse = "\n"))
    shiny::outputOptions(output, "map_source_credits", suspendWhenHidden = FALSE)
    output$licensing_notes <- shiny::downloadHandler(filename = "ALS-Downloader-licensing.txt", content = function(file) {
      paths <- c(system.file("sources", "LICENSING.md", package = "alsdownloader"),
        system.file("sources", "USE_REVIEW.md", package = "alsdownloader"),
        system.file("sources", "DISCOVERY_COVERAGE.md", package = "alsdownloader"),
        system.file("NOTICE", package = "alsdownloader"),
        system.file("app", "www", "html2canvas-LICENSE.txt", package = "alsdownloader"))
      writeLines(unlist(lapply(paths, readLines, warn = FALSE)), file, useBytes = TRUE)
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
    shiny::observeEvent(input$reset_aoi, clear_aoi())
    shiny::observeEvent(input$reset_all, {
      if (!is.null(state$job) && state$job$is_alive()) state$job$kill_tree()
      release_lock(); release_output_lock()
      if (!is.null(state$jobdir)) unlink(state$jobdir, recursive = TRUE)
      state$job <- NULL; state$jobdir <- NULL
      state$destination <- NULL; state$finished <- FALSE
      state$jobtext <- "No active download."
      if (mode == "local") {
        shiny::updateTextInput(session, "destination", value = tempdir())
        shiny::updateNumericInput(session, "workers", value = min(4L, policy$maximum))
      }
      clear_aoi()
    })
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
        destination <- if (mode == "hosted") file.path(jobdir, "files") else input$destination
        if (is.null(destination) || !nzchar(trimws(destination))) stop("Choose a local output directory.")
        state$jobdir <- jobdir; state$destination <- destination; state$finished <- FALSE
        state$job <- background_job("download_tiles", list(tiles = rows, output_dir = destination,
          workers = if (mode == "hosted") 1L else input$workers,
          mode = mode, provider_limit = provider_limit, progress_dir = file.path(jobdir, "progress")))
        state$job_total <- nrow(rows)
        state$jobtext <- paste("Starting download:", nrow(rows), "tiles.")
      }, error = function(e) {release_lock(); notify(e)})
    })
    shiny::observe({
      shiny::invalidateLater(700, session)
      job <- state$job
      if (is.null(job) || isTRUE(state$finished)) return()
      completed <- length(list.files(file.path(state$jobdir, "progress"), "\\.rds$"))
      state$jobtext <- paste(completed, "of", state$job_total, "tiles processed.")
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
      if ((isTRUE(state$comparison_busy) || isTRUE(state$source_test_busy))) {shiny::showNotification("Wait for the campaign comparison or cancel it first."); return()}
      shiny::req(input$point_file)
      if (!is.null(state$preview_job) && state$preview_job$is_alive()) return()
      state$preview_target <- "als-cloud"
      state$preview_label <- input$point_file$name
      state$preview_attribution <- figure_attribution()
      state$previewtext <- "Reading a bounded point sample..."
      state$preview_started <- Sys.time()
      session$sendCustomMessage("als-points", list(target = "als-cloud", points = list(), origin = c(0, 0, 0)))
      preview_path <- tempfile(fileext = paste0(".", tools::file_ext(input$point_file$name)))
      state$preview_path <- preview_path
      file.copy(input$point_file$datapath, preview_path)
      tryCatch({
        state$preview_job <- background_job("local_preview_job", list(preview_path,
          input$local_percent, as.numeric(input$local_window), input$local_center_x,
          input$local_center_y, as.numeric(input$local_voxel), input$local_xy_units, input$local_z_units))
      }, error = function(e) {
        unlink(preview_path)
        state$previewtext <- paste("Could not start preview:", redact_urls_in_text(conditionMessage(e)))
      })
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
      member <- if(identical(state$zip_url,tile$url[[1]])) input$zip_member else NULL
      if(!identical(state$zip_url,tile$url[[1]]))state$zip_members<-character()
      state$zip_url<-tile$url[[1]]
      if (mode == "hosted") {
        if (!dir.create(hosted_lock, showWarnings = FALSE)) {
          shiny::showNotification("Another hosted transfer is running. Try again later."); return()
        }
        state$lock_owned <- TRUE; state$preview_locked <- TRUE
      }
      state$preview_target <- "als-cloud"
      shiny::updateTabsetPanel(session, "view", selected = "3D view")
      state$preview_label <- tile$filename[[1]]
      state$preview_attribution <- figure_attribution(tile)
      state$previewtext <- paste("Downloading and sampling:", state$preview_label)
      state$preview_started <- Sys.time()
      session$sendCustomMessage("als-points", list(target = "als-cloud", points = list(), origin = c(0, 0, 0)))
      state$preview_path <- tempfile(fileext = ".laz")
      tryCatch({state$preview_job <- background_job("remote_preview_job",
        list(tile, state$preview_path, input$local_percent, as.numeric(input$local_window),
          input$local_center_x, input$local_center_y, as.numeric(input$local_voxel), input$local_xy_units, input$local_z_units, member,
          if(mode == "hosted") 250 * 1024^2 else 1024^3,
          if(mode == "hosted") 500 * 1024^2 else 2 * 1024^3))}, error = function(e) {
          if (isTRUE(state$preview_locked)) {release_lock(); state$preview_locked <- FALSE}
          state$previewtext <- paste("Could not start preview:", redact_urls_in_text(conditionMessage(e)))
        })
    })
    shiny::observe({
      shiny::invalidateLater(500, session)
      job <- state$preview_job
      if (is.null(job)) return()
      if (job$is_alive()) {
        elapsed <- as.numeric(difftime(Sys.time(), state$preview_started, units = "secs"))
        status_file <- paste0(state$preview_path, ".status")
        stage <- if (file.exists(status_file)) tryCatch(readLines(status_file, warn = FALSE), error = function(e) character()) else character()
        received <- if (file.exists(state$preview_path)) file.size(state$preview_path) / 1024^2 else 0
        state$previewtext <- paste(state$preview_label, "-", if (length(stage)) stage[1] else "Starting/reading preview...",
          sprintf("Elapsed: %.0f s. Temporary file: %.1f MiB. You can cancel this preview.", elapsed, received))
        if (is.finite(elapsed) && elapsed > 900) {
          job$kill_tree(); state$preview_job <- NULL
          cleanup_preview_files(state$preview_path)
          if (isTRUE(state$preview_locked)) {release_lock(); state$preview_locked <- FALSE}
          state$previewtext <- "Preview stopped after 15 minutes. Try a smaller tile or download it for local inspection."
        }
        return()
      }
      state$preview_job <- NULL
      if (isTRUE(state$preview_locked)) {release_lock(); state$preview_locked <- FALSE}
      if (!is.null(state$preview_path)) cleanup_preview_files(state$preview_path)
      tryCatch({p <- job$get_result()
        if(is.list(p)&&!is.data.frame(p)&&length(p$zip_members)) {
          state$zip_members<-p$zip_members
          state$previewtext<-"ZIP contains multiple point clouds. Select a file, then use View selected map tile. The archive will be downloaded again."
          return()
        }
        if(!is.null(attr(p,"archive_member")))state$preview_label<-paste(state$preview_label,"/",attr(p,"archive_member"))
        caption <- paste(nrow(p), "preview points.", attr(p, "units_note"))
        if (!is.null(attr(p, "display_note"))) caption <- paste(caption, attr(p, "display_note"))
        if (state$preview_target == "als-cloud") state$previewtext <- paste(state$preview_label, "-", caption)
        else state$tiletext <- paste(state$preview_label, "-", caption)
        session$sendCustomMessage("als-points", list(target = state$preview_target, points = unname(as.matrix(p)), classification = unname(attr(p, "classification")), intensity = unname(attr(p, "intensity")), units = attr(p, "units"), units_note = attr(p, "units_note"), label = state$preview_label, attribution = as.list(state$preview_attribution), origin = unname(attr(p, "origin"))))},
        error = function(e) {
          message <- paste("Preview failed:", redact_urls_in_text(conditionMessage(e)))
          if (state$preview_target == "als-cloud") state$previewtext <- message else state$tiletext <- message
        })
    })
    output$preview_status <- shiny::renderText(state$previewtext)
    output$zip_member_control <- shiny::renderUI({
      if(length(state$zip_members))shiny::selectInput("zip_member","Point cloud within ZIP",
        choices=c("Select a file"="",stats::setNames(state$zip_members,state$zip_members)))
    })
    shiny::observeEvent(input$cancel_preview, {
      if (is.null(state$preview_job)) return()
      if (state$preview_job$is_alive()) state$preview_job$kill_tree()
      state$preview_job <- NULL
      if (!is.null(state$preview_path)) cleanup_preview_files(state$preview_path)
      if (isTRUE(state$preview_locked)) {release_lock(); state$preview_locked <- FALSE}
      state$previewtext <- "Preview cancelled. Select a tile and rebuild when ready."
    })
    output$tile_preview_status <- shiny::renderText(state$tiletext)
    shiny::observeEvent(input$local_pose, session$sendCustomMessage("als-view", list(target = "als-cloud", pose = input$local_pose)))
    shiny::observeEvent(input$local_point_size, session$sendCustomMessage("als-view", list(target = "als-cloud", pointSize = input$local_point_size)))
    shiny::observeEvent(input$exaggeration, session$sendCustomMessage("als-view", list(target = "als-cloud", exaggeration = input$exaggeration)))
    shiny::observeEvent(input$colour_by, session$sendCustomMessage("als-view", list(target = "als-cloud", colourBy = input$colour_by)))
    shiny::observeEvent(input$palette, session$sendCustomMessage("als-view", list(target = "als-cloud", palette = input$palette)))
    session$onSessionEnded(function() shiny::isolate({
      if (!is.null(state$job) && state$job$is_alive()) state$job$kill_tree()
      if (!is.null(state$preview_job) && state$preview_job$is_alive()) state$preview_job$kill_tree()
      if (!is.null(state$preview_path)) cleanup_preview_files(state$preview_path)
      release_lock()
      release_output_lock()
      if (!is.null(state$jobdir)) unlink(state$jobdir, recursive = TRUE)
    }))
  }
  shiny::shinyApp(ui, server, options = list(shiny.maxRequestSize = 1024 * 1024^2))
}

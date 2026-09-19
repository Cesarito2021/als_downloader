comparison_ui <- function() {
  shiny::tabPanel("Compare campaigns",
    shiny::h3("Two point clouds, overlapping area only"),
    shiny::textOutput("compare_time_message"),
    shiny::conditionalPanel("output.compare_temporal_ready === 'yes'",
    shiny::checkboxInput("compare_opt_in", "I want to compare two point clouds in this study area", FALSE),
    shiny::p("Search an AOI in Explore, then choose two campaigns. Collection dates are supplied by the provider; the final acquisition date represents a multi-date survey. Publication dates and filename dates are not substituted. The overlay is for visualization only. Downloads remain separate."),
    shiny::fluidRow(shiny::column(6, shiny::selectInput("epoch_a", "A | first / earlier cloud", choices = character()),
      shiny::actionButton("download_epoch_a", "Select A tiles for download")),
      shiny::column(6, shiny::selectInput("epoch_b", "B | latest cloud", choices = character()),
      shiny::actionButton("download_epoch_b", "Select B tiles for download"))),
    shiny::selectInput("compare_side_m", "Preview square side", choices = c("100 m (default)" = 100, "250 m" = 250, "500 m" = 500, "1 km" = 1000), selected = 100),
    shiny::tags$details(shiny::tags$summary("Source elevation units"),
      shiny::helpText("Display axes use metres. Horizontal units come from the CRS; elevation units are checked separately. If Z units are absent, confirm them from provider documentation before loading. Unit conversion does not align vertical datums."),
      shiny::selectInput("compare_z_units_a", "A: source elevation units", source_unit_choices()),
      shiny::selectInput("compare_z_units_b", "B: source elevation units", source_unit_choices())),
    shiny::helpText("A small window is placed in the largest shared footprint and clipped to your AOI. Draw a smaller AOI in Explore to choose its location. The automatic location is not guaranteed to represent the forest."),
    shiny::helpText("Visualization only: square side 100 m to 1 km (100 m = 0.01 km2; 1 km = 1 km2). At most four tiles and 600 MB per campaign, 300 MB per tile. Both clouds require the same embedded projected CRS and known coordinate units; feet are scaled to metres for display."),
    shiny::textOutput("compare_availability"), shiny::uiOutput("compare_load_control"),
    shiny::actionButton("compare_cancel", "Cancel comparison"), shiny::textOutput("compare_status"),
    shiny::tags$div(class = "als-compare-split",
      shiny::tags$canvas(id = "als-compare-cloud-a", class = "als-point-cloud als-compare-panel", role = "img", tabindex = "0", `aria-label` = "Campaign A point cloud. Drag to rotate (synced with panel B); plus/minus to zoom; zero to fit."),
      shiny::tags$canvas(id = "als-compare-cloud-b", class = "als-point-cloud als-compare-panel", role = "img", tabindex = "0", `aria-label` = "Campaign B point cloud. Drag to rotate (synced with panel A); plus/minus to zoom; zero to fit.")),
    shiny::helpText("Panels A and B share one camera: drag or zoom either one and both rotate together, so the same view compares both clouds side by side."),
    shiny::tags$div(class="als-profile-tools",
      shiny::tags$button(id="profile_draw",type="button",class="btn btn-default",disabled=TRUE,"Draw profile line"),
      shiny::tags$button(id="profile_3d",type="button",class="btn btn-default",disabled=TRUE,"Return to 3D"),
      shiny::tags$button(id="profile_clear",type="button",class="btn btn-default",disabled=TRUE,"Clear profile"),
      shiny::tags$label(`for`="profile_width","Profile strip width (m)"),
      shiny::tags$input(id="profile_width",type="number",min=0.2,max=100,step=0.2,value=2),
      figure_button("export_cloud", "Download cloud figure", TRUE)),
    shiny::tags$p(id="profile_hint",role="status",`aria-live`="polite","Load two clouds to draw a profile. Drawing switches both panels to a plan view: click the start and end in either one, or drag a line in any direction; the same segment appears in both. Escape cancels drawing."),
    shiny::tags$div(id="profile_panel",hidden=NA,
      shiny::h4("Profile along the selected line (both campaigns)"),
      shiny::tags$canvas(id="als-compare-profile",class="als-profile-canvas",role="img",`aria-label`="Distance and elevation profile of sampled points from both campaigns"),
      shiny::tags$div(class="als-profile-tools",
        figure_button("export_profile", "Download profile figure", TRUE),
        figure_button("export_combined", "Download both figures", TRUE))),
    shiny::tags$div(class="als-density-panel",
      shiny::h4("Elevation distribution (both campaigns)"),
      shiny::tags$canvas(id="als-compare-density", class="als-density-canvas", role="img",
        `aria-label`="Elevation density histograms for campaigns A and B, with each campaign's sample count and mean elevation."),
      figure_button("export_density_png", "Save distribution PNG", TRUE),
      shiny::helpText("Density of all currently loaded points by elevation, not a modeled distribution or a calculated difference between campaigns.")),
    shiny::helpText("A (earlier) is red and B (later) is blue on black, for a clearer contrast between the two clouds. Colours identify campaigns, not measured change. Profiles show sampled points within the chosen strip, without fitted curves or calculated differences."),
    shiny::fluidRow(shiny::column(4, shiny::selectInput("compare_palette_a", "A color / palette", preview_palettes(), selected = "Red")),
      shiny::column(4, shiny::selectInput("compare_palette_b", "B color / palette", preview_palettes(), selected = "Blue")),
      shiny::column(4, shiny::sliderInput("compare_exaggeration", "Vertical exaggeration", 1, 12, 1, step = 1))),
    shiny::checkboxInput("compare_focus", "Focus camera on central 98% (display only; turn off to fit all points)", TRUE),
    shiny::checkboxInput("compare_show_a", "Show A", TRUE), shiny::checkboxInput("compare_show_b", "Show B", TRUE),
    shiny::helpText("One shared origin, camera and elevation scale. Up to 50,000 display points per cloud. PNG exports contain figures only, not analytical results. Provider footprints may contain gaps in actual point coverage."),
    shiny::tags$details(shiny::tags$summary("Source and display information"), shiny::verbatimTextOutput("compare_metadata"))))
}

comparison_server <- function(input, output, session, state, mode, hosted_lock) {
  cmp <- shiny::reactiveValues(job = NULL, directory = NULL, locked = FALSE, result = NULL,
    status = "Search an AOI to discover campaigns.", labels = NULL, attribution = NULL, dates = NULL, files = NULL)
  state$comparison_busy <- FALSE
  cleanup <- function() {
    if (isTRUE(cmp$locked)) {unlink(hosted_lock, recursive = TRUE); cmp$locked <- FALSE}
    if (!is.null(cmp$directory) && dir.exists(cmp$directory)) {
      root <- paste0(normalizePath(tempdir(), winslash = "/"), "/")
      if (startsWith(normalizePath(cmp$directory, winslash = "/"), root)) unlink(cmp$directory, recursive = TRUE)
    }
    cmp$directory <- NULL
    state$comparison_busy <- FALSE
  }
  stop_job <- function() {if (!is.null(cmp$job) && cmp$job$is_alive()) cmp$job$kill_tree(); cmp$job <- NULL; cleanup()}
  groups <- shiny::reactive(campaign_groups(state$tiles))
  output$compare_temporal_ready <- shiny::renderText(if (is.null(comparison_time_message(state$tiles))) "yes" else "no")
  shiny::outputOptions(output, "compare_temporal_ready", suspendWhenHidden = FALSE)
  output$compare_time_message <- shiny::renderText({
    message <- comparison_time_message(state$tiles)
    if (is.null(message)) "Two acquisition periods are available. Select a pair to check shared coverage. Two separate flights in the same year can qualify." else message
  })
  availability <- shiny::reactive(comparison_availability(state$tiles, state$aoi, input$epoch_a, input$epoch_b, input$compare_opt_in, if (is.null(input$compare_side_m)) 100 else input$compare_side_m))
  output$compare_availability <- shiny::renderText(availability()$message)
  output$compare_load_control <- shiny::renderUI({
    available <- availability()
    if (isTRUE(available$ready)) shiny::actionButton("compare_load", "View overlapping clouds", class = "als-primary")
    else shiny::actionButton("compare_load", "View overlapping clouds", class = "als-primary", disabled = TRUE)
  })
  shiny::observeEvent(state$tiles, {
    g <- groups(); choices <- if (length(g)) stats::setNames(names(g), paste0(names(g), " [", lengths(g), " tiles]")) else character()
    choices <- c("Choose a point-cloud campaign" = "", choices)
    shiny::updateSelectInput(session, "epoch_a", choices = choices, selected = "")
    shiny::updateSelectInput(session, "epoch_b", choices = choices, selected = "")
  }, ignoreNULL = FALSE)
  shiny::observeEvent(list(state$aoi, state$tiles, input$epoch_a, input$epoch_b, input$compare_opt_in, input$compare_side_m, input$compare_z_units_a, input$compare_z_units_b), {
    stop_job(); cmp$result <- NULL; cmp$status <- "Choose two campaigns, then load the AOI comparison."
    session$sendCustomMessage("als-points", list(target = "als-compare-cloud", points = list(), origin = c(0, 0, 0)))
  }, ignoreNULL = FALSE)
  choose_download <- function(key) {
    if (is.null(key) || !nzchar(key) || !key %in% names(groups())) {
      shiny::showNotification("Search an AOI and choose a campaign first."); return()
    }
    indexes <- groups()[[key]]
    if (!length(indexes)) return()
    DT::selectRows(DT::dataTableProxy("tiles"), indexes)
    shiny::updateTabsetPanel(session, "view", selected = "Explore")
    shiny::showNotification("Only this campaign's tiles are selected. Review them and use Download selected tiles.")
  }
  shiny::observeEvent(input$download_epoch_a, choose_download(input$epoch_a))
  shiny::observeEvent(input$download_epoch_b, choose_download(input$epoch_b))
  shiny::observeEvent(input$compare_load, {
    if (!isTRUE(availability()$ready)) {shiny::showNotification(availability()$message); return()}
    if (isTRUE(state$source_test_busy) || isTRUE(state$comparison_busy) || (!is.null(state$job) && state$job$is_alive()) ||
        (!is.null(state$preview_job) && state$preview_job$is_alive())) {
      shiny::showNotification("Wait for the current download or preview."); return()
    }
    cmp$result <- NULL
    tryCatch({
      g <- groups()
      if (is.null(input$epoch_a) || is.null(input$epoch_b) || !input$epoch_a %in% names(g) || !input$epoch_b %in% names(g)) stop("Search an AOI and choose two campaigns first.")
      ia <- g[[input$epoch_a]]; ib <- g[[input$epoch_b]]
      if (!length(ia) || !length(ib) || identical(input$epoch_a, input$epoch_b)) stop("Choose two distinct campaigns.")
      if (is.null(state$aoi)) stop("Draw/upload an AOI first.")
      region <- comparison_region(state$tiles[ia, ], state$tiles[ib, ], state$aoi, input$compare_side_m)
      overlap <- region$overlap
      if (mode == "hosted") {
        if (!dir.create(hosted_lock, showWarnings = FALSE)) stop("Another hosted transfer is running. Try again later.")
        cmp$locked <- TRUE
      }
      cmp$directory <- tempfile("als-comparison-"); dir.create(cmp$directory)
      a <- region$a; b <- region$b
      cmp$labels <- c(input$epoch_a, input$epoch_b)
      cmp$attribution <- figure_attribution(rbind(a, b))
      cmp$dates <- NULL
      cmp$files <- NULL
      state$comparison_busy <- TRUE; cmp$status <- "Loading both clouds inside the overlapping area..."
      cmp$job <- background_job("compare_campaigns", list(a, b, overlap, cmp$directory, input$compare_z_units_a, input$compare_z_units_b))
    }, error = function(e) {cmp$status <- conditionMessage(e); cleanup()})
  })
  shiny::observe({
    shiny::invalidateLater(500, session)
    job <- cmp$job
    if (is.null(job)) return()
    if (job$is_alive()) {
      progress <- file.path(cmp$directory, "progress.txt")
      if (file.exists(progress)) {
        text <- tryCatch(readLines(progress, warn = FALSE), error = function(e) character())
        if (length(text)) cmp$status <- text[1]
      }
      return()
    }
    cmp$job <- NULL
    tryCatch({
      cmp$result <- job$get_result()
      r <- cmp$result
      reference <- sf::st_crs(r$crs)
      crs_label <- if (!is.na(reference$epsg)) paste0("EPSG:", reference$epsg) else reference$Name
      crs_label <- paste("Source", crs_label, "| display coordinates scaled to metres")
      cmp$status <- sprintf("A: %s overlap points | B: %s overlap points | %.4f km2. Overlay ready for visualization only.", r$counts[1], r$counts[2], r$overlap_km2)
      session$sendCustomMessage("als-points", list(target = "als-compare-cloud", points = rbind(r$a, r$b),
        groups = c(rep(0L, nrow(r$a)), rep(1L, nrow(r$b))), origin = r$origin, labels = cmp$labels,
        crs = crs_label, attribution = as.list(c(cmp$attribution, r$unit_notes))))
    }, error = function(e) {cmp$status <- paste("Comparison could not be completed:", conditionMessage(e))})
    cleanup()
  })
  shiny::observeEvent(input$compare_cancel, {stop_job(); cmp$status <- "Comparison cancelled."})
  output$compare_status <- shiny::renderText(cmp$status)
  output$compare_metadata <- shiny::renderText({shiny::req(cmp$result); paste(paste(c("A", "B"), cmp$labels, collapse = "\n"), paste(cmp$result$unit_notes, collapse = "\n"), cmp$result$method, cmp$result$crs, sep = "\n\n")})
  shiny::observeEvent(list(input$compare_palette_a, input$compare_palette_b, input$compare_exaggeration, input$compare_show_a, input$compare_show_b, input$compare_focus),
    session$sendCustomMessage("als-view", list(target = "als-compare-cloud", palette = input$compare_palette_a,
      paletteB = input$compare_palette_b, exaggeration = input$compare_exaggeration, showA = input$compare_show_a, showB = input$compare_show_b, focusCentral = input$compare_focus)))
  session$onSessionEnded(function() shiny::isolate(stop_job()))
  invisible(cmp)
}

comparison_ui <- function() {
  shiny::tabPanel("Compare campaigns",
    shiny::h3("Two point clouds, overlapping area only"),
    shiny::checkboxInput("compare_opt_in", "I want to compare two point clouds in this study area", FALSE),
    shiny::p("Search an AOI in Explore, then choose two campaigns. Dates are displayed as supplied by the provider, without independent year verification or correction. The overlay is for visualization only. Downloads remain separate."),
    shiny::fluidRow(shiny::column(6, shiny::selectInput("epoch_a", "A | reference campaign", choices = character()),
      shiny::actionButton("download_epoch_a", "Select A tiles for download")),
      shiny::column(6, shiny::selectInput("epoch_b", "B | comparison campaign", choices = character()),
      shiny::actionButton("download_epoch_b", "Select B tiles for download"))),
    shiny::helpText("Visualization only: overlapping area up to 1 km2, limited to the intersection of both provider footprints and your AOI. At most four tiles and 200 MB per campaign, 100 MB per tile. Both clouds require the same embedded projected CRS in metres."),
    shiny::textOutput("compare_availability"), shiny::uiOutput("compare_load_control"),
    shiny::actionButton("compare_cancel", "Cancel comparison"), shiny::textOutput("compare_status"),
    shiny::tags$canvas(id = "als-compare-cloud", class = "als-point-cloud", role = "img", tabindex = "0", `aria-label` = "Two georeferenced campaign point clouds. Drag to rotate; plus/minus to zoom; zero to fit."),
    shiny::fluidRow(shiny::column(4, shiny::selectInput("compare_palette_a", "A color / palette", c("Cyan", "Orange", "Viridis", "Magma", "Plasma", "Cividis"))),
      shiny::column(4, shiny::selectInput("compare_palette_b", "B color / palette", c("Orange", "Cyan", "Magma", "Viridis", "Plasma", "Cividis"))),
      shiny::column(4, shiny::sliderInput("compare_exaggeration", "Vertical exaggeration", 1, 12, 1, step = 1))),
    shiny::checkboxInput("compare_show_a", "Show A", TRUE), shiny::checkboxInput("compare_show_b", "Show B", TRUE),
    shiny::helpText("One shared origin, camera and elevation scale. Up to 50,000 display points per cloud. No calculated differences, analysis or exports. Provider footprints may contain gaps in actual point coverage."),
    shiny::tags$details(shiny::tags$summary("Source and display information"), shiny::verbatimTextOutput("compare_metadata")))
}

comparison_server <- function(input, output, session, state, mode, hosted_lock) {
  cmp <- shiny::reactiveValues(job = NULL, directory = NULL, locked = FALSE, result = NULL,
    status = "Search an AOI to discover campaigns.", labels = NULL, dates = NULL, files = NULL)
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
  availability <- shiny::reactive(comparison_availability(state$tiles, state$aoi, input$epoch_a, input$epoch_b, input$compare_opt_in))
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
  shiny::observeEvent(list(state$aoi, state$tiles, input$epoch_a, input$epoch_b, input$compare_opt_in), {
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
    if (isTRUE(state$comparison_busy) || (!is.null(state$job) && state$job$is_alive()) ||
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
      overlap <- comparison_overlap(state$tiles[ia, ], state$tiles[ib, ], state$aoi)
      if (length(ia) > 4 || length(ib) > 4) stop("Reduce the AOI to at most four intersecting tiles per campaign.")
      if (mode == "hosted") {
        if (!dir.create(hosted_lock, showWarnings = FALSE)) stop("Another hosted transfer is running. Try again later.")
        cmp$locked <- TRUE
      }
      cmp$directory <- tempfile("als-comparison-"); dir.create(cmp$directory)
      a <- state$tiles[ia, , drop = FALSE]; b <- state$tiles[ib, , drop = FALSE]
      cmp$labels <- c(input$epoch_a, input$epoch_b)
      cmp$dates <- NULL
      cmp$files <- NULL
      state$comparison_busy <- TRUE; cmp$status <- "Loading both clouds inside the overlapping area..."
      cmp$job <- callr::r_bg(function(a, b, aoi, directory)
        alsdownloader:::compare_campaigns(a, b, aoi, directory),
        args = list(a, b, overlap, cmp$directory), supervise = TRUE)
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
      cmp$status <- sprintf("A: %s overlap points | B: %s overlap points | %.4f km2. Overlay ready for visualization only.", r$counts[1], r$counts[2], r$overlap_km2)
      session$sendCustomMessage("als-points", list(target = "als-compare-cloud", points = rbind(r$a, r$b),
        groups = c(rep(0L, nrow(r$a)), rep(1L, nrow(r$b))), origin = r$origin))
    }, error = function(e) {cmp$status <- paste("Comparison could not be completed:", conditionMessage(e))})
    cleanup()
  })
  shiny::observeEvent(input$compare_cancel, {stop_job(); cmp$status <- "Comparison cancelled."})
  output$compare_status <- shiny::renderText(cmp$status)
  output$compare_metadata <- shiny::renderText({shiny::req(cmp$result); paste(paste(c("A", "B"), cmp$labels, collapse = "\n"), cmp$result$method, cmp$result$crs, sep = "\n\n")})
  shiny::observeEvent(list(input$compare_palette_a, input$compare_palette_b, input$compare_exaggeration, input$compare_show_a, input$compare_show_b),
    session$sendCustomMessage("als-view", list(target = "als-compare-cloud", palette = input$compare_palette_a,
      paletteB = input$compare_palette_b, exaggeration = input$compare_exaggeration, showA = input$compare_show_a, showB = input$compare_show_b)))
  session$onSessionEnded(function() shiny::isolate(stop_job()))
  invisible(cmp)
}

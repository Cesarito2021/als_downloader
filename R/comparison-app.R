comparison_ui <- function() {
  shiny::tabPanel("Compare campaigns",
    shiny::h3("Two campaigns, one study area"),
    shiny::p("Search an AOI in Explore, then choose two acquisition campaigns. Campaign dates are survey intervals, not publication years. Downloads remain separate."),
    shiny::fluidRow(shiny::column(6, shiny::selectInput("epoch_a", "A | reference campaign", choices = character()),
      shiny::actionButton("download_epoch_a", "Select A tiles for download")),
      shiny::column(6, shiny::selectInput("epoch_b", "B | comparison campaign", choices = character()),
      shiny::actionButton("download_epoch_b", "Select B tiles for download"))),
    shiny::helpText("Exploratory comparison: AOI up to 0.25 km2; at most four tiles and 200 MB per campaign, 100 MB per tile. Full source tiles are downloaded temporarily; the exact AOI is used for analysis. Both clouds must have the same embedded projected CRS in metres."),
    shiny::fluidRow(shiny::column(6, shiny::numericInput("compare_resolution", "Grid cell size (metres)", 10, min = 1, max = 100)),
      shiny::column(6, shiny::numericInput("compare_min_points", "Minimum points per cell in EACH campaign", 5, min = 3, step = 1))),
    shiny::actionButton("compare_load", "Load and compare AOI", class = "als-primary"),
    shiny::actionButton("compare_cancel", "Cancel comparison"), shiny::textOutput("compare_status"),
    shiny::tags$canvas(id = "als-compare-cloud", class = "als-point-cloud", role = "img", tabindex = "0", `aria-label` = "Two georeferenced campaign point clouds. Drag to rotate; plus/minus to zoom; zero to fit."),
    shiny::fluidRow(shiny::column(4, shiny::selectInput("compare_palette_a", "A color / palette", c("Cyan", "Orange", "Viridis", "Magma", "Plasma", "Cividis"))),
      shiny::column(4, shiny::selectInput("compare_palette_b", "B color / palette", c("Orange", "Cyan", "Magma", "Viridis", "Plasma", "Cividis"))),
      shiny::column(4, shiny::sliderInput("compare_exaggeration", "Vertical exaggeration", 1, 12, 1, step = 1))),
    shiny::checkboxInput("compare_show_a", "Show A", TRUE), shiny::checkboxInput("compare_show_b", "Show B", TRUE),
    shiny::helpText("One shared origin, camera and elevation scale. Up to 50,000 display points per campaign; calculations use all retained AOI points, not the display sample."),
    shiny::tags$details(shiny::tags$summary("Coordinate system and analysis metadata"), shiny::verbatimTextOutput("compare_metadata")),
    shiny::h4("Difference: P95 elevation B minus A"),
    shiny::p("This is a difference in the 95th percentile of source elevation on a shared grid, not a canopy-height change or a statistically significant change estimate. Registration, acquisition season, density and classifications can affect the result."),
    shiny::textInput("vertical_a", "Verified vertical reference for A (datum + geoid/model)", placeholder = "Copy from the survey metadata"),
    shiny::textInput("vertical_b", "Verified vertical reference for B (datum + geoid/model)", placeholder = "Must match A; no vertical conversion is performed"),
    shiny::checkboxInput("compare_verified", "I verified the same vertical reference, metre Z units, absolute elevations (not normalized heights) and adequate alignment in the survey metadata.", FALSE),
    shiny::textOutput("compare_gate"),
    shiny::uiOutput("difference_panel"),
    shiny::uiOutput("difference_exports"))
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
  shiny::observeEvent(state$tiles, {
    g <- groups(); choices <- if (length(g)) stats::setNames(names(g), paste0(names(g), " [", lengths(g), " tiles]")) else character()
    shiny::updateSelectInput(session, "epoch_a", choices = choices, selected = if (length(g)) names(g)[1] else "")
    shiny::updateSelectInput(session, "epoch_b", choices = choices, selected = if (length(g) > 1) names(g)[2] else "")
  }, ignoreNULL = FALSE)
  shiny::observeEvent(list(state$aoi, state$tiles, input$epoch_a, input$epoch_b, input$compare_resolution, input$compare_min_points), {
    stop_job(); cmp$result <- NULL; cmp$status <- "Choose two campaigns, then load the AOI comparison."
    shiny::updateCheckboxInput(session, "compare_verified", value = FALSE)
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
      if (is.null(state$aoi) || aoi_area(state$aoi) > .25) stop("Draw/upload an AOI no larger than 0.25 km2.")
      if (length(ia) > 4 || length(ib) > 4) stop("Reduce the AOI to at most four intersecting tiles per campaign.")
      if (!is.finite(input$compare_resolution) || input$compare_resolution < 1 || input$compare_resolution > 100 ||
          !is.finite(input$compare_min_points) || input$compare_min_points < 3 || input$compare_min_points != floor(input$compare_min_points)) stop("Check grid resolution and minimum point count.")
      if (mode == "hosted") {
        if (!dir.create(hosted_lock, showWarnings = FALSE)) stop("Another hosted transfer is running. Try again later.")
        cmp$locked <- TRUE
      }
      cmp$directory <- tempfile("als-comparison-"); dir.create(cmp$directory)
      a <- sf::st_drop_geometry(state$tiles[ia, , drop = FALSE]); b <- sf::st_drop_geometry(state$tiles[ib, , drop = FALSE])
      cmp$labels <- c(input$epoch_a, input$epoch_b)
      cmp$dates <- list(a = a[c("acquired_start", "acquired_end")], b = b[c("acquired_start", "acquired_end")])
      cmp$files <- rbind(transform(a, epoch = "A"), transform(b, epoch = "B")); cmp$files$url <- redact_url(cmp$files$url)
      state$comparison_busy <- TRUE; cmp$status <- "Downloading campaigns sequentially, clipping the AOI and building the shared grid..."
      cmp$job <- callr::r_bg(function(a, b, aoi, resolution, minimum, directory)
        alsdownloader:::compare_campaigns(a, b, aoi, resolution, minimum, directory),
        args = list(a, b, state$aoi, input$compare_resolution, input$compare_min_points, cmp$directory), supervise = TRUE)
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
      cmp$status <- sprintf("A: %s AOI points | B: %s AOI points. Overlay ready; difference eligibility is shown below.", r$counts[1], r$counts[2])
      session$sendCustomMessage("als-points", list(target = "als-compare-cloud", points = rbind(r$a, r$b),
        groups = c(rep(0L, nrow(r$a)), rep(1L, nrow(r$b))), origin = r$origin))
    }, error = function(e) {cmp$status <- paste("Comparison could not be completed:", conditionMessage(e))})
    cleanup()
  })
  shiny::observeEvent(input$compare_cancel, {stop_job(); cmp$status <- "Comparison cancelled."})
  output$compare_status <- shiny::renderText(cmp$status)
  output$compare_metadata <- shiny::renderText({shiny::req(cmp$result); paste(paste(c("A", "B"), cmp$labels, collapse = "\n"), cmp$result$method, cmp$result$crs, sep = "\n\n")})
  gate <- shiny::reactive({
    comparison_gate(cmp$result, cmp$dates, input$compare_verified, input$vertical_a, input$vertical_b)
  })
  output$compare_gate <- shiny::renderText(gate())
  difference <- shiny::reactive({shiny::req(startsWith(gate(), "Exploratory difference enabled")); cmp$result$grid})
  output$difference_panel <- shiny::renderUI({difference(); shiny::tagList(shiny::plotOutput("difference_plot", height = "420px"), shiny::textOutput("difference_summary"))})
  output$difference_plot <- shiny::renderPlot({
    g <- difference(); delta <- g$delta_b_minus_a; lim <- max(abs(delta), na.rm = TRUE); if (lim == 0) lim <- 1
    colors <- grDevices::colorRampPalette(c("#2166ac", "#f7f7f7", "#b2182b"))(101)
    col <- rep("#606b75", nrow(g)); col[g$eligible] <- colors[1L + round(100 * (delta[g$eligible] + lim) / (2 * lim))]
    r <- cmp$result$resolution / 2
    graphics::par(bg = "#101b23", fg = "#e0eaf0", col.axis = "#e0eaf0", col.lab = "#e0eaf0", mar = c(4, 4, 3, 1))
    graphics::plot(g$x, g$y, type = "n", asp = 1, xlim = range(g$x) + c(-r, r), ylim = range(g$y) + c(-r, r),
      xlab = "Easting (m)", ylab = "Northing (m)", main = "P95 elevation difference: B - A (m)", col.main = "#e0eaf0")
    graphics::rect(g$x-r, g$y-r, g$x+r, g$y+r, col = col, border = NA)
    graphics::legend("topright", legend = c(sprintf("%.2f m", -lim), "0 m", sprintf("+%.2f m", lim), "Insufficient / missing"), fill = c(colors[c(1,51,101)], "#606b75"), bg = "#101b23", text.col = "#e0eaf0", cex = .8)
  })
  output$difference_summary <- shiny::renderText({g <- difference(); sprintf("%s of %s occupied grid cells comparable; median B - A = %.3f m. Cell P95 uses full retained AOI points; no significance test or automatic change classification.", sum(g$eligible), nrow(g), stats::median(g$delta_b_minus_a[g$eligible]))})
  output$difference_exports <- shiny::renderUI({difference(); shiny::tagList(shiny::downloadButton("difference_csv", "Export difference grid CSV"), shiny::downloadButton("comparison_metadata", "Export comparison provenance JSON"))})
  output$difference_csv <- shiny::downloadHandler(filename = "p95-difference-b-minus-a.csv", content = function(file) utils::write.csv(difference(), file, row.names = FALSE, na = ""))
  output$comparison_metadata <- shiny::downloadHandler(filename = "comparison-provenance.json", content = function(file) {
    difference(); r <- cmp$result
    jsonlite::write_json(list(created_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE), software_version = as.character(utils::packageVersion("alsdownloader")), campaigns = cmp$labels, sources = cmp$files, crs = r$crs,
      vertical_reference_user_verified = input$vertical_a, z_units_user_verified = "metres", method = r$method,
      resolution_m = r$resolution, minimum_points = input$compare_min_points, counts = r$counts,
      aoi_wkt_epsg4326 = sf::st_as_text(sf::st_geometry(state$aoi))), file, pretty = TRUE, auto_unbox = TRUE)
  })
  shiny::observeEvent(list(input$compare_palette_a, input$compare_palette_b, input$compare_exaggeration, input$compare_show_a, input$compare_show_b),
    session$sendCustomMessage("als-view", list(target = "als-compare-cloud", palette = input$compare_palette_a,
      paletteB = input$compare_palette_b, exaggeration = input$compare_exaggeration, showA = input$compare_show_a, showB = input$compare_show_b)))
  session$onSessionEnded(function() shiny::isolate(stop_job()))
  invisible(cmp)
}

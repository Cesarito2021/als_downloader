comparison_ui <- function() {
  shiny::tabPanel("3D comparison",
    shiny::h3("Two point clouds, overlapping area only"),
    shiny::radioButtons("compare_source", "Comparison sources", c("Two source point clouds"="remote", "Source point cloud + local LAS/LAZ"="local"),inline=TRUE),
    shiny::conditionalPanel("input.compare_source === 'local'",
      shiny::fileInput("compare_local_file","B | Local point cloud (LAS/LAZ, up to 1 GB)",accept=c(".las",".laz")),
      shiny::textOutput("compare_local_status"),
      shiny::checkboxInput("compare_local_refs","Confirm compatible horizontal and vertical references. Matching EPSG alone may not establish the elevation datum.",FALSE),
      shiny::helpText("The uploaded file stays in this app session and is not submitted to a provider or catalogue. On a hosted app it is uploaded to that server. Dates are not inferred; this mode is for visual inspection, including same-date surveys.")),
    shiny::textOutput("compare_time_message"),
    shiny::conditionalPanel("output.compare_temporal_ready === 'yes'",
    shiny::checkboxInput("compare_opt_in", "Enable point-cloud comparison", FALSE),
    shiny::p("Search an AOI in Explore, then choose source point cloud A and either another source point cloud or local point cloud B. Source-to-source comparison requires separate acquisition periods. Local-cloud dates are unverified. This is visualization only; downloads remain separate."),
    shiny::fluidRow(shiny::column(6, shiny::selectInput("epoch_a", "A | first / earlier cloud", choices = character()),
      shiny::actionButton("download_epoch_a", "Select A tiles for download")),
      shiny::column(6, shiny::conditionalPanel("input.compare_source !== 'local'",shiny::selectInput("epoch_b", "B | latest cloud", choices = character()),
      shiny::actionButton("download_epoch_b", "Select B tiles for download")))),
    shiny::selectInput("compare_side_m", "Preview square side", choices = c("100 m (default)" = 100, "250 m" = 250, "500 m" = 500, "1 km" = 1000), selected = 100),
    shiny::tags$details(shiny::tags$summary("Source elevation units"),
      shiny::helpText("Display axes use metres. Horizontal units come from the CRS; elevation units are checked separately. If Z units are absent, confirm them from provider documentation before loading. Unit conversion does not align vertical datums."),
      shiny::selectInput("compare_z_units_a", "A: source elevation units", source_unit_choices()),
      shiny::selectInput("compare_z_units_b", "B: source elevation units", source_unit_choices())),
    shiny::helpText("A small window is placed in the largest shared footprint and clipped to the AOI. Draw a smaller AOI in Explore to choose its location. The automatic location is not guaranteed to represent the forest."),
    shiny::helpText("Visualization only: square side 100 m to 1 km. Remote limits: four tiles and 600 MB per source, 300 MB per tile. One local file up to 1 GB is supported. Both clouds require the same embedded projected CRS and known coordinate units; feet are scaled to metres for display. No vertical-datum transformation is performed."),
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
      figure_button("export_cloud", "Download cloud views (PNG)", TRUE)),
    shiny::tags$p(id="profile_hint",role="status",`aria-live`="polite","Load two clouds to draw a profile. Drawing switches both panels to a plan view: click the start and end in either one, or drag a line in any direction; the same segment appears in both. Escape cancels drawing."),
    shiny::tags$div(id="profile_panel",hidden=NA,
      shiny::h4("Profile along the selected line (both point clouds)"),
      shiny::tags$canvas(id="als-compare-profile",class="als-profile-canvas",role="img",`aria-label`="Distance and elevation profile of sampled points from both point clouds"),
      shiny::tags$div(class="als-profile-tools",
        figure_button("export_profile", "Download profile (PNG)", TRUE))),
    shiny::tags$div(class="als-density-panel",
      shiny::h4("Elevation distribution (both point clouds)"),
      shiny::tags$canvas(id="als-compare-density", class="als-density-canvas", role="img",
        `aria-label`="Elevation density histograms for point clouds A and B, with each cloud's sample count and mean elevation."),
      figure_button("export_density_png", "Download distribution (PNG)", TRUE),
      shiny::helpText("Sampled point counts by elevation. This is not a probability density or a calculated difference between point clouds.")),
    shiny::helpText("Cloud views use one shared elevation scale. Profiles and distributions distinguish A and B in red and blue by default. Similar appearances do not establish that the surveys are identical."),
    shiny::fluidRow(shiny::column(6,shiny::selectInput("compare_cloud_mode","Cloud view colours",c("Shared elevation scale"="shared","Source colours"="campaign"))),
      shiny::column(6,shiny::selectInput("compare_shared_palette","Shared elevation palette",c("Greens","Viridis","Magma","Plasma","Cividis","Greyscale"),selected="Greens"))),
    shiny::helpText("Both clouds share the same minimum and maximum source elevation in metres. This is not height above ground. Vertical exaggeration affects display only: 1x is true proportions, 2x doubles vertical differences."),
    shiny::fluidRow(shiny::column(4, shiny::selectInput("compare_palette_a", "A profile / source colour", preview_palettes(), selected = "Red")),
      shiny::column(4, shiny::selectInput("compare_palette_b", "B profile / source colour", preview_palettes(), selected = "Blue")),
      shiny::column(4, shiny::sliderInput("compare_exaggeration", "Vertical scale factor", 1, 12, 1, step = 1))),
    shiny::checkboxInput("compare_focus", "Focus camera on central 98% (display only; turn off to fit all points)", TRUE),
    shiny::checkboxInput("compare_show_a", "Show A", TRUE), shiny::checkboxInput("compare_show_b", "Show B", TRUE),
    shiny::helpText("One shared origin, camera and elevation scale. Up to 50,000 display points per cloud. PNG exports contain figures only, not analytical results. Provider footprints may contain gaps in actual point coverage."),
    shiny::tags$details(shiny::tags$summary("Source and display information"), shiny::verbatimTextOutput("compare_metadata"))))
}

comparison_server <- function(input, output, session, state, mode, hosted_lock) {
  cmp <- shiny::reactiveValues(job = NULL, directory = NULL, locked = FALSE, result = NULL,
    status = "Search an AOI to discover point clouds.", labels = NULL, attribution = NULL, dates = NULL, files = NULL)
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
  is_local <- shiny::reactive(identical(input$compare_source,"local"))
  local_tile <- shiny::reactive({if(!is_local() || is.null(input$compare_local_file))return(NULL)
    tryCatch(local_comparison_tile(input$compare_local_file),error=function(e)e)})
  output$compare_local_status <- shiny::renderText({x<-local_tile();if(inherits(x,"error"))conditionMessage(x) else if(is.null(x))"Upload a local cloud to begin." else "Header and projected CRS read. Actual point overlap and matching CRS will be checked when loading."})
  output$compare_temporal_ready <- shiny::renderText(if (is_local() || is.null(comparison_time_message(state$tiles))) "yes" else "no")
  shiny::outputOptions(output, "compare_temporal_ready", suspendWhenHidden = FALSE)
  output$compare_time_message <- shiny::renderText({
    if(is_local())return("Compare one available source point cloud with a local point cloud. Two dated source point clouds are not required in this mode.")
    message <- comparison_time_message(state$tiles)
    if (is.null(message)) "Two acquisition periods are available. Select a pair to check shared coverage. Two separate flights in the same year can qualify." else message
  })
  availability <- shiny::reactive({side<-if(is.null(input$compare_side_m))100 else input$compare_side_m
    if(is_local()) {x<-local_tile();if(inherits(x,"error"))return(list(ready=FALSE,message=conditionMessage(x)))
      local_comparison_availability(state$tiles,state$aoi,input$epoch_a,x,input$compare_opt_in,input$compare_local_refs,side)
    } else comparison_availability(state$tiles,state$aoi,input$epoch_a,input$epoch_b,input$compare_opt_in,side)})
  output$compare_availability <- shiny::renderText(availability()$message)
  output$compare_load_control <- shiny::renderUI({
    available <- availability()
    if (isTRUE(available$ready)) shiny::actionButton("compare_load", "View overlapping clouds", class = "als-primary")
    else shiny::actionButton("compare_load", "View overlapping clouds", class = "als-primary", disabled = TRUE)
  })
  shiny::observeEvent(state$tiles, {
    g <- groups(); choices <- if (length(g)) stats::setNames(names(g), paste0(names(g), " [", lengths(g), " tiles]")) else character()
    choices <- c("Choose a point-cloud source" = "", choices)
    shiny::updateSelectInput(session, "epoch_a", choices = choices, selected = "")
    shiny::updateSelectInput(session, "epoch_b", choices = choices, selected = "")
  }, ignoreNULL = FALSE)
  shiny::observeEvent(list(state$aoi, state$tiles, input$epoch_a, input$epoch_b, input$compare_opt_in, input$compare_side_m, input$compare_z_units_a, input$compare_z_units_b,input$compare_source,input$compare_local_file,input$compare_local_refs), {
    stop_job(); cmp$result <- NULL; cmp$status <- "Choose two point-cloud sources, then load the AOI comparison."
    session$sendCustomMessage("als-points", list(target = "als-compare-cloud", points = list(), origin = c(0, 0, 0)))
  }, ignoreNULL = FALSE)
  choose_download <- function(key) {
    if (is.null(key) || !nzchar(key) || !key %in% names(groups())) {
      shiny::showNotification("Search an AOI and choose a source first."); return()
    }
    indexes <- groups()[[key]]
    if (!length(indexes)) return()
    DT::selectRows(DT::dataTableProxy("tiles"), indexes)
    shiny::updateTabsetPanel(session, "view", selected = "Explore")
    shiny::showNotification("Only this source's tiles are selected. Review them and use Download selected tiles.")
  }
  shiny::observeEvent(input$download_epoch_a, choose_download(input$epoch_a))
  shiny::observeEvent(input$download_epoch_b, {if(!is_local())choose_download(input$epoch_b)})
  shiny::observeEvent(input$compare_load, {
    if (!isTRUE(availability()$ready)) {shiny::showNotification(availability()$message); return()}
    if (isTRUE(state$source_test_busy) || isTRUE(state$comparison_busy) || (!is.null(state$job) && state$job$is_alive()) ||
        (!is.null(state$preview_job) && state$preview_job$is_alive())) {
      shiny::showNotification("Wait for the current download or preview."); return()
    }
    cmp$result <- NULL
    tryCatch({
      g <- groups()
      if (is.null(input$epoch_a) || !input$epoch_a %in% names(g)) stop("Choose source point cloud A.")
      ia <- g[[input$epoch_a]]
      if(is_local()) b<-local_tile() else {
        if(is.null(input$epoch_b) || !input$epoch_b %in% names(g) || identical(input$epoch_a,input$epoch_b))stop("Choose two distinct acquisition periods.")
        b<-state$tiles[g[[input$epoch_b]],]
      }
      if (is.null(state$aoi)) stop("Draw/upload an AOI first.")
      region <- comparison_region(state$tiles[ia, ], b, state$aoi, input$compare_side_m)
      overlap <- region$overlap
      if (mode == "hosted") {
        if (!dir.create(hosted_lock, showWarnings = FALSE)) stop("Another hosted transfer is running. Try again later.")
        cmp$locked <- TRUE
      }
      cmp$directory <- tempfile("als-comparison-"); dir.create(cmp$directory)
      a <- region$a; b <- region$b
      local_path<-NULL
      if(is_local()) {
        local_path<-file.path(cmp$directory,"user-cloud.laz")
        if(!file.copy(input$compare_local_file$datapath,local_path))stop("Could not stage the local cloud.")
      }
      cmp$labels <- c(input$epoch_a, if(is_local())paste("User cloud:",b$filename[1],"| date unverified") else input$epoch_b)
      cmp$attribution <- if(is_local())c(figure_attribution(a),paste("B: user-uploaded",b$filename[1],"- source credits and rights must be supplied by the user; date unverified.")) else figure_attribution(rbind(a, b))
      cmp$dates <- NULL
      cmp$files <- NULL
      state$comparison_busy <- TRUE; cmp$status <- "Loading both clouds inside the overlapping area..."
      cmp$job <- background_job("compare_campaigns", list(a, b, overlap, cmp$directory, input$compare_z_units_a, input$compare_z_units_b,local_path))
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
  shiny::observeEvent(list(input$compare_palette_a, input$compare_palette_b, input$compare_exaggeration, input$compare_show_a, input$compare_show_b, input$compare_focus,input$compare_cloud_mode,input$compare_shared_palette),
    session$sendCustomMessage("als-view", list(target = "als-compare-cloud", palette = input$compare_palette_a,
      paletteB = input$compare_palette_b, cloudMode=input$compare_cloud_mode,sharedPalette=input$compare_shared_palette,exaggeration = input$compare_exaggeration, showA = input$compare_show_a, showB = input$compare_show_b, focusCentral = input$compare_focus)))
  session$onSessionEnded(function() shiny::isolate(stop_job()))
  invisible(cmp)
}

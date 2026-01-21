# ============================================================
# app.R
# LiDAR Downloader (GitHub-ready)
# Tabs:
#  1) OpenTopography (LOCAL TileIndex zips): AOI overlap -> plot -> select -> download
#  2) Planetary Computer (3dep-lidar-copc): STAC -> plot year -> select -> download
# ============================================================

# Safety: if someone accidentally defined fromJSON() in the global env, remove it
if (exists("fromJSON", envir = .GlobalEnv, inherits = FALSE)) {
  rm(fromJSON, envir = .GlobalEnv)
}
app_dir <- getwd()


suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(sf)
  library(dplyr)
  library(data.table)
  library(httr)
  library(jsonlite)
  library(purrr)
  library(future)
  library(future.apply)
  library(ggplot2)
  library(DT)
})

# Remove a global fromJSON() override (this is the common real problem)
if (exists("fromJSON", envir = .GlobalEnv, inherits = FALSE)) {
  rm(fromJSON, envir = .GlobalEnv)
}

options(shiny.fullstacktrace = TRUE)
options(shiny.maxRequestSize = 200 * 1024^2)  # 200 MB upload

app_dir <- getwd()
source(file.path(app_dir, "base.R"))

# Internal (hidden) TileIndex directory
tileindex_dir <- file.path(app_dir, "data", "TileIndex_all")

# -----------------------------
# UI
# -----------------------------
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "ALS Downloader"),
  dashboardSidebar(
    sidebarMenu(
      # Global key (used by OT Local only)
      tags$div(
        style = "padding: 10px;",
        passwordInput(
          "ot_api_key_global",
          "API key:",
          value = Sys.getenv("OPENTOPO_API_KEY")
        ),
        helpText("")
      ),
      menuItem("OpenTopography", tabName = "ot_local", icon = icon("cloud-download-alt")),
      menuItem("USGS 3DEP", tabName = "pc_copc", icon = icon("cloud-download-alt"))
    )
  ),
  dashboardBody(
    tags$head(
      # your main css file (create it in: www/css/main.css)
      if (file.exists("www/css/main.css")) includeCSS("www/css/main.css")
    ),
    
    tabItems(
      # =========================================================
      # TAB 1: OT local TileIndex zips
      # =========================================================
      tabItem(
        tabName = "ot_local",
        fluidPage(
          titlePanel("OpenTopography dataset"),
          
          sidebarLayout(
            sidebarPanel(
              width = 4,
              
              fileInput(
                "ot_aoi_file",
                "Upload AOI (.zip shapefile or .gpkg/.geojson/.shp):",
                multiple = FALSE,
                accept = c(".zip", ".gpkg", ".geojson", ".json", ".shp", ".fgb")
              ),
              
              helpText("TileIndex zips are expected inside: ./data/TileIndex_all (hidden)."),
              verbatimTextOutput("ot_tileindex_status"),
              
              hr(),
              
              numericInput(
                "ot_cores",
                "Download cores:",
                value = max(1, parallel::detectCores() - 1),
                min = 1,
                max = max(1, parallel::detectCores()),
                step = 1
              ),
              
              textInput(
                "ot_base_dir",
                "Output folder directory:",
                value = get_downloads_dir()
              ),
              
              textInput("ot_aoi_label", "Output folder name:", value = "OT_LOCAL_OUTPUT"),
              
              actionButton(
                "ot_run_search",
                "Search tiles (AOI overlap)",
                icon = icon("search"),
                class = "btn-als-search",
                style = "width:100%;"
              ),
              
              hr(),
              
              uiOutput("ot_year_ui"),
              
              actionButton(
                "ot_run_download",
                "Download selected tiles (parallel)",
                icon = icon("download"),
                class = "btn-als-download",
                style = "width:100%;"
              ),
              
              hr(),
              verbatimTextOutput("ot_status")
            ),
            
            mainPanel(
              width = 8,
              
              # -------- LOGO (ON SCREEN) --------
              div(
                class = "logo-wrap",
                tags$img(
                  src = "logo/logo_300dpi.png",
                  class = "logo-screen"
                )
              ),
              br(),
              
              h4("ALS availability"),
              plotOutput("ot_year_plot", height = 260),
              
              br(),
              h4("Summary"),
              DTOutput("ot_summary_table"),
              
              br(),
              h4("Full tiles table"),
              DTOutput("ot_full_table")
            )
          )
        )
      ),
      
      # =========================================================
      # TAB 2: Planetary Computer COPC
      # =========================================================
      tabItem(
        tabName = "pc_copc",
        fluidPage(
          titlePanel("Planetary Computer: 3DEP COPC (STAC)"),
          
          sidebarLayout(
            sidebarPanel(
              width = 4,
              
              fileInput(
                "pc_aoi_file",
                "Upload AOI (.zip shapefile or .gpkg/.geojson/.shp):",
                multiple = FALSE,
                accept = c(".zip", ".gpkg", ".geojson", ".json", ".shp", ".fgb")
              ),
              
              textInput("pc_date_min", "Datetime min (YYYY-MM-DD):", value = "2010-01-01"),
              textInput(
                "pc_date_max",
                "Datetime max (YYYY-MM-DD):",
                value = paste0(format(Sys.Date(), "%Y"), "-12-31")
              ),
              
              hr(),
              
              numericInput(
                "pc_cores",
                "Download cores:",
                value = max(1, parallel::detectCores() - 1),
                min = 1,
                max = max(1, parallel::detectCores()),
                step = 1
              ),
              
              textInput("pc_base_dir", "Output folder directory:", value = get_downloads_dir()),
              textInput("pc_aoi_label", "Output folder name:", value = "PC_COPC_OUTPUT"),
              
              actionButton(
                "pc_run_search",
                "Search COPC items",
                icon = icon("search"),
                class = "btn-als-search",
                style = "width:100%;"
              ),
              
              hr(),
              uiOutput("pc_year_ui"),
              
              actionButton(
                "pc_run_download",
                "Download selected COPC (parallel)",
                icon = icon("download"),
                class = "btn-als-download",
                style = "width:100%;"
              ),
              
              hr(),
              verbatimTextOutput("pc_status")
            ),
            
            mainPanel(
              width = 8,
              
              # -------- LOGO (ON SCREEN) --------
              div(
                class = "logo-wrap",
                tags$img(
                  src = "logo/logo_300dpi.png",
                  class = "logo-screen"
                )
              ),
              br(),
              
              h4("ALS availability"),
              plotOutput("pc_year_plot", height = 260),
              
              br(),
              h4("Summary"),
              DTOutput("pc_summary_table"),
              
              br(),
              h4("Full items table"),
              DTOutput("pc_full_table")
            )
          )
        )
      )
    )
  )
)

# -----------------------------
# SERVER
# -----------------------------
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    ot_tiles = NULL,
    pc_items = NULL
  )
  
  # helper: sync key into environment (so your tested functions work)
  sync_ot_key <- function() {
    k <- trimws(as.character(input$ot_api_key_global %||% ""))
    if (nzchar(k)) Sys.setenv(OPENTOPO_API_KEY = k)
    k
  }
  
  output$ot_tileindex_status <- renderText({
    if (!dir.exists(tileindex_dir)) {
      return(paste0("TileIndex folder status: NOT FOUND\n", tileindex_dir))
    }
    z <- list.files(tileindex_dir, pattern = "_TileIndex\\.zip$", full.names = TRUE)
    paste0(
      "TileIndex folder status: OK\n",
      "Zip files found: ", length(z)
    )
  })
  
  # =========================================================
  # TAB 1: OT local TileIndex
  # =========================================================
  observeEvent(input$ot_run_search, {
    req(input$ot_aoi_file)
    
    output$ot_status <- renderText("Running OT local search...")
    
    tryCatch({
      
      if (!dir.exists(tileindex_dir)) {
        stop(sprintf("TileIndex folder not found: %s", tileindex_dir))
      }
      
      api_key <- sync_ot_key()
      if (!nzchar(api_key)) {
        stop("OpenTopography API key is required (for EPSG/catalog lookup).")
      }
      
      aoi_file <- input$ot_aoi_file$datapath
      tiles <- NULL
      
      withProgress(message = "Overlapping AOI with local TileIndex zips...", value = 0, {
        
        output$ot_status <- renderText("Running TileIndex overlap...")
        
        tiles <- suppressMessages(
          tileindex_filter_by_aoi_to_links(
            tilezip_dir   = tileindex_dir,
            aoi_path      = aoi_file,
            show_progress = FALSE,
            api_key       = api_key
          )
        )
        
        incProgress(1, detail = "Done")
      })
      
      rv$ot_tiles <- tiles
      
      if (is.null(tiles) || nrow(tiles) == 0) {
        output$ot_status <- renderText("❌ No tiles overlapping AOI (or zips unreadable / EPSG lookup failed).")
        output$ot_year_ui <- renderUI(NULL)
        return()
      }
      
      yrs <- sort(unique(tiles$year))
      yrs <- yrs[is.finite(yrs)]
      
      output$ot_status <- renderText(paste0(
        "✅ Tiles overlapping AOI: ", nrow(tiles), "\n",
        "AOI area (km^2): ", sprintf("%.2f", aoi_area_km2(aoi_file)), "\n",
        "Years detected: ", ifelse(length(yrs) == 0, "None (NA)", paste(yrs, collapse = ", "))
      ))
      
      output$ot_year_ui <- renderUI({
        if (length(yrs) == 0) return(helpText("Year not detected → year filter disabled."))
        selectInput(
          "ot_year_sel",
          "Select year(s) (or ALL):",
          choices = c("ALL" = "ALL", setNames(yrs, yrs)),
          selected = "ALL",
          multiple = TRUE
        )
      })
      
    }, error = function(e) {
      rv$ot_tiles <- NULL
      output$ot_year_ui <- renderUI(NULL)
      output$ot_status <- renderText(paste0("❌ OT local search failed:\n", conditionMessage(e)))
    })
  })
  
  ot_filtered <- reactive({
    df <- rv$ot_tiles
    if (is.null(df) || nrow(df) == 0) return(df)
    
    if (!is.null(input$ot_year_sel) && length(input$ot_year_sel) > 0 && !("ALL" %in% input$ot_year_sel)) {
      df <- df[df$year %in% as.integer(input$ot_year_sel), , drop = FALSE]
    }
    df
  })
  
  output$ot_year_plot <- renderPlot({
    df <- ot_filtered()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    df2 <- df %>% dplyr::filter(is.finite(year)) %>% dplyr::count(year)
    if (nrow(df2) == 0) return(NULL)
    
    ggplot(df2, aes(x = factor(year), y = n)) +
      geom_col(fill = "#fbbf24", color = "#fbbf24", width = 0.7) +
      labs(x = "Year", y = "Number of tiles") +
      theme_minimal(base_size = 13) +
      theme(
        plot.background   = element_rect(fill = "#050816", colour = NA),
        panel.background  = element_rect(fill = "#020617", colour = "#020617"),
        panel.grid.major  = element_line(color = "#111827"),
        panel.grid.minor  = element_blank(),
        axis.text.x       = element_text(angle = 45, hjust = 1, colour = "#e5e7eb"),
        axis.text.y       = element_text(colour = "#e5e7eb"),
        axis.title        = element_text(colour = "#e5e7eb"),
        panel.border      = element_rect(colour = "#111827", fill = NA),
        legend.position   = "none"
      )
  })
  output$ot_summary_table <- renderDT({
    df <- ot_filtered()
    if (is.null(df) || nrow(df) == 0) return(datatable(data.frame()))
    
    df_sum <- df %>%
      mutate(year = ifelse(is.finite(year), year, NA_integer_)) %>%
      group_by(year) %>%
      summarise(n_tiles = n(), example_file = file_name[1], .groups = "drop") %>%
      arrange(year)
    
    datatable(df_sum, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })
  
  output$ot_full_table <- renderDT({
    df <- ot_filtered()
    if (is.null(df) || nrow(df) == 0) return(datatable(data.frame()))
    
    datatable(
      df %>% select(year, file_name, laz_link),
      options = list(pageLength = 12, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  observeEvent(input$ot_run_download, {
    df <- ot_filtered()
    if (is.null(df) || nrow(df) == 0) {
      showNotification("No tiles to download (run Search first, and check filters).", type = "error")
      output$ot_status <- renderText("❌ No tiles to download.")
      return()
    }
    if (!("laz_link" %in% names(df))) {
      showNotification("Internal error: 'laz_link' column not present.", type = "error")
      output$ot_status <- renderText("❌ Internal error: laz_link missing.")
      return()
    }
    
    base_dir_input <- trimws(input$ot_base_dir)
    if (!nzchar(base_dir_input)) {
      showNotification("Please type an output folder path in 'Output folder directory'.", type = "error")
      output$ot_status <- renderText("❌ No output folder directory specified.")
      return()
    }
    
    base_dir <- normalizePath(base_dir_input, winslash = "/", mustWork = FALSE)
    dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
    
    ncores <- suppressWarnings(as.integer(input$ot_cores))
    if (is.na(ncores) || ncores < 1L) ncores <- 1L
    
    urls <- unique(trimws(as.character(df$laz_link)))
    urls <- urls[nzchar(urls) & !is.na(urls)]
    if (length(urls) == 0) {
      showNotification("No valid URLs to download (after cleaning).", type = "error")
      output$ot_status <- renderText("❌ No valid URLs to download.")
      return()
    }
    
    aoi_label <- sanitize_folder_name(input$ot_aoi_label)
    output_dir <- file.path(base_dir, aoi_label)
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    
    future::plan(future::multisession, workers = ncores)
    on.exit(future::plan(future::sequential), add = TRUE)
    
    output$ot_status <- renderText(paste0(
      "⬇ Starting download...\n",
      "Tiles selected: ", nrow(df), "\n",
      "Unique URLs: ", length(urls), "\n",
      "Output: ", normalizePath(output_dir, winslash = "/", mustWork = FALSE), "\n",
      "Cores: ", ncores
    ))
    
    withProgress(message = "Downloading LAZ tiles...", value = 0, {
      log_status <- tryCatch(
        download_laz_parallel_no_progress(
          urls       = urls,
          output_dir = output_dir,
          cores      = ncores,
          timeout    = 1200
        ),
        error = function(e) {
          message("Download error: ", conditionMessage(e))
          NULL
        }
      )
      
      if (is.null(log_status)) {
        output$ot_status <- renderText(paste0(
          "❌ Error during download. Check console/log.\n",
          "Tried to use folder: ", normalizePath(output_dir, winslash = "/", mustWork = FALSE)
        ))
      } else {
        output$ot_status <- renderText(paste0(
          "✅ Download completed using ", ncores, " core(s).\n",
          "URLs requested: ", length(urls), "\n",
          "LAZ saved under: ", normalizePath(file.path(output_dir, "LAZ"), winslash = "/", mustWork = FALSE), "\n",
          "Log: ", normalizePath(file.path(output_dir, "LAZ", "FILE_PROCESSING.txt"), winslash = "/", mustWork = FALSE)
        ))
      }
    })
  })
  
  # =========================
  # TAB 2: Planetary Computer COPC
  # =========================
  rv$pc_status_txt <- reactiveVal("")
  output$pc_status <- renderText(as.character(rv$pc_status_txt()))
  
  observeEvent(input$pc_run_search, {
    req(input$pc_aoi_file)
    
    rv$pc_status_txt("Searching Planetary Computer STAC...")
    
    tryCatch({
      aoi <- read_aoi_any(input$pc_aoi_file$datapath)
      aoi <- sf::st_make_valid(sf::st_transform(aoi, 4326))
      
      dt_min <- trimws(input$pc_date_min)
      dt_max <- trimws(input$pc_date_max)
      
      year_max <- suppressWarnings(as.integer(substr(dt_max, 1, 4)))
      if (!is.finite(year_max)) year_max <- as.integer(format(Sys.Date(), "%Y"))
      
      withProgress(message = "Searching Planetary Computer STAC...", value = 0, {
        items <- pc_copc_search_aoi(
          aoi_sf = aoi,
          datetime_min = dt_min,
          datetime_max = dt_max,
          year_max = year_max
        )
        rv$pc_items <- as.data.frame(items)
        incProgress(1)
      })
      
      items <- rv$pc_items
      if (is.null(items) || nrow(items) == 0) {
        rv$pc_status_txt("❌ No COPC items found.")
        output$pc_year_ui <- renderUI(NULL)
        return()
      }
      
      yrs <- sort(unique(items$year))
      yrs <- yrs[is.finite(yrs)]
      
      rv$pc_status_txt(paste0(
        "✅ COPC items found: ", nrow(items), "\n",
        "Years: ", paste(yrs, collapse = ", ")
      ))
      
      output$pc_year_ui <- renderUI({
        selectInput(
          "pc_year_sel",
          "Select year(s) (or ALL):",
          choices = c("ALL" = "ALL", setNames(yrs, yrs)),
          selected = "ALL",
          multiple = TRUE
        )
      })
      
    }, error = function(e) {
      rv$pc_items <- NULL
      output$pc_year_ui <- renderUI(NULL)
      rv$pc_status_txt(paste0("❌ PC search failed:\n", conditionMessage(e)))
    })
  })
  
  pc_filtered <- reactive({
    df <- rv$pc_items
    if (is.null(df) || nrow(df) == 0) return(df)
    
    if (!is.null(input$pc_year_sel) && length(input$pc_year_sel) > 0 && !("ALL" %in% input$pc_year_sel)) {
      df <- df[df$year %in% as.integer(input$pc_year_sel), , drop = FALSE]
    }
    df
  })
  
  output$pc_year_plot <- renderPlot({
    df <- pc_filtered()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    df2 <- df %>% dplyr::count(year) %>% dplyr::arrange(year)
    
    ggplot(df2, aes(x = factor(year), y = n)) +
      geom_col(fill = "#fbbf24", color = "#fbbf24", width = 0.7) +
      labs(x = "Year", y = "Number of items") +
      theme_minimal(base_size = 13) +
      theme(
        plot.background   = element_rect(fill = "#050816", colour = NA),
        panel.background  = element_rect(fill = "#020617", colour = "#020617"),
        panel.grid.major  = element_line(color = "#111827"),
        panel.grid.minor  = element_blank(),
        axis.text.x       = element_text(angle = 45, hjust = 1, colour = "#e5e7eb"),
        axis.text.y       = element_text(colour = "#e5e7eb"),
        axis.title        = element_text(colour = "#e5e7eb"),
        panel.border      = element_rect(colour = "#111827", fill = NA),
        legend.position   = "none"
      )
  })
  
  output$pc_summary_table <- renderDT({
    df <- pc_filtered()
    if (is.null(df) || nrow(df) == 0) return(datatable(data.frame()))
    
    df_sum <- df %>%
      mutate(year = ifelse(is.finite(year), year, NA_integer_)) %>%
      group_by(year) %>%
      summarise(n_items = n(), example_file = file_name[1], .groups = "drop") %>%
      arrange(year)
    
    datatable(df_sum, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })
  
  output$pc_full_table <- renderDT({
    df <- pc_filtered()
    if (is.null(df) || nrow(df) == 0) return(datatable(data.frame()))
    
    datatable(
      df %>% select(year, file_name, laz_link),
      options = list(pageLength = 12, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  observeEvent(input$pc_run_download, {
    
    tryCatch({
      
      df <- pc_filtered()
      req(df)
      
      base_dir_input <- trimws(input$pc_base_dir)
      if (!nzchar(base_dir_input)) {
        showNotification("Please type an output folder path in 'Output folder directory'.", type = "error")
        rv$pc_status_txt("❌ No output folder directory specified.")
        return()
      }
      
      base_dir <- normalizePath(base_dir_input, winslash = "/", mustWork = FALSE)
      dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
      
      ncores <- suppressWarnings(as.integer(input$pc_cores))
      if (is.na(ncores) || ncores < 1L) ncores <- 1L
      
      if (is.null(df) || nrow(df) == 0) {
        showNotification("No items to download (check filters).", type = "error")
        rv$pc_status_txt("❌ No items to download (check filters).")
        return()
      }
      if (!("laz_link" %in% names(df))) {
        showNotification("Internal error: 'laz_link' column not present.", type = "error")
        rv$pc_status_txt("❌ Internal error: 'laz_link' column not present.")
        return()
      }
      
      urls <- unique(trimws(as.character(df$laz_link)))
      urls <- urls[nzchar(urls) & !is.na(urls)]
      
      if (length(urls) == 0) {
        showNotification("No valid URLs to download (after cleaning).", type = "error")
        rv$pc_status_txt("❌ No valid URLs to download.")
        return()
      }
      
      aoi_label <- sanitize_folder_name(input$pc_aoi_label)
      output_dir <- file.path(base_dir, aoi_label)
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
      
      future::plan(future::multisession, workers = ncores)
      on.exit(future::plan(future::sequential), add = TRUE)
      
      rv$pc_status_txt(paste0(
        "⬇ Starting download...\n",
        "Items selected: ", nrow(df), "\n",
        "Unique URLs: ", length(urls), "\n",
        "Output: ", normalizePath(output_dir, winslash = "/", mustWork = FALSE), "\n",
        "Cores: ", ncores
      ))
      
      withProgress(message = "Downloading COPC tiles...", value = 0, {
        
        log_status <- tryCatch(
          download_laz_parallel_no_progress_copc(
            urls       = urls,
            output_dir = output_dir,
            cores      = ncores,
            timeout    = 1200
          ),
          error = function(e) {
            message("Download error: ", conditionMessage(e))
            NULL
          }
        )
        
        if (is.null(log_status)) {
          rv$pc_status_txt(paste0(
            "❌ Error during download. Check console/log.\n",
            "Tried folder: ", normalizePath(output_dir, winslash = "/", mustWork = FALSE)
          ))
        } else {
          rv$pc_status_txt(paste0(
            "✅ Download completed using ", ncores, " core(s).\n",
            "URLs requested: ", length(urls), "\n",
            "Saved under: ", normalizePath(file.path(output_dir, "LAZ"), winslash = "/", mustWork = FALSE), "\n",
            "Log: ", normalizePath(file.path(output_dir, "LAZ", "FILE_PROCESSING.txt"), winslash = "/", mustWork = FALSE)
          ))
        }
        
        incProgress(1)
      })
      
    }, error = function(e) {
      rv$pc_status_txt(paste0("❌ Download failed:\n", conditionMessage(e)))
    })
    
  })
  
}

shinyApp(ui, server)

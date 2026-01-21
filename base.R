# ============================================================
# base.R  (MINIMAL - NO DEM)
# Helpers for the Shiny app
# Tabs:
#  1) OpenTopography (LOCAL TileIndex zips) -> AOI overlap -> select -> download
#  2) Planetary Computer (3DEP COPC)        -> STAC search  -> select -> download
# ============================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(data.table)
  library(httr)
  library(jsonlite)
  library(purrr)
  library(future)
  library(future.apply)
  library(rstac)
  library(geojsonsf)
})

sf::sf_use_s2(FALSE)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# ---------------------------
# Paths / naming helpers
# ---------------------------
get_downloads_dir <- function() {
  up <- Sys.getenv("USERPROFILE")
  if (nzchar(up)) {
    d <- file.path(up, "Downloads")
    if (dir.exists(d)) return(normalizePath(d, winslash = "/", mustWork = FALSE))
  }
  h <- normalizePath("~", winslash = "/", mustWork = FALSE)
  d2 <- file.path(h, "Downloads")
  if (dir.exists(d2)) return(normalizePath(d2, winslash = "/", mustWork = FALSE))
  h
}

sanitize_folder_name <- function(x) {
  x <- trimws(as.character(x %||% ""))
  if (!nzchar(x)) return("OUTPUT")
  gsub("[^A-Za-z0-9._-]", "_", x)
}

aoi_area_km2 <- function(aoi_sf) {
  # If a path, read it
  if (is.character(aoi_sf) && length(aoi_sf) == 1 && file.exists(aoi_sf)) {
    aoi_sf <- read_aoi_any(aoi_sf)
  }
  if (inherits(aoi_sf, "sfc")) aoi_sf <- sf::st_sf(geometry = aoi_sf)
  if (!inherits(aoi_sf, "sf")) return(NA_real_)
  
  aoi_m <- suppressWarnings(sf::st_transform(aoi_sf, 5070))
  as.numeric(sf::st_area(sf::st_union(aoi_m))) / 1e6
}

# ---------------------------
# Read AOI (zip shapefile / gpkg / geojson / shp / fgb)
# Accepts: shiny fileInput list OR a character path
# Returns: sf in EPSG:4326 (single unioned geometry)
# ---------------------------
read_aoi_any <- function(x) {
  path <- NULL
  if (is.list(x) && "datapath" %in% names(x)) path <- x$datapath[1]
  if (is.null(path) && is.character(x) && length(x) == 1) path <- x
  
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    stop("AOI file not found or invalid input.")
  }
  
  ext <- tolower(tools::file_ext(path))
  
  if (ext == "zip") {
    tmp <- tempfile("aoi_zip_")
    dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
    utils::unzip(path, exdir = tmp)
    
    shp <- list.files(tmp, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
    if (length(shp) == 0) stop("ZIP does not contain a .shp file.")
    aoi <- sf::st_read(shp[1], quiet = TRUE)
    
  } else if (ext == "gpkg") {
    layers <- sf::st_layers(path)$name
    if (length(layers) == 0) stop("No layers found in GPKG.")
    aoi <- sf::st_read(path, layer = layers[1], quiet = TRUE)
    
  } else if (ext %in% c("geojson","json","shp","fgb")) {
    aoi <- sf::st_read(path, quiet = TRUE)
    
  } else {
    stop("Unsupported AOI format: ", ext)
  }
  
  aoi <- suppressWarnings(sf::st_make_valid(aoi))
  aoi <- aoi[!sf::st_is_empty(aoi), , drop = FALSE]
  if (nrow(aoi) == 0) stop("AOI geometry is empty after reading.")
  
  # Union to single geometry (more robust)
  if (nrow(aoi) > 1) {
    aoi <- sf::st_union(aoi)
    aoi <- sf::st_as_sf(aoi)
  }
  
  suppressWarnings(sf::st_transform(aoi, 4326))
}

# ============================================================
# OpenTopography - API key + catalog EPSG lookup
# ============================================================
get_opentopo_key <- function(env_var = "OPENTOPO_API_KEY", api_key = NULL) {
  if (!is.null(api_key) && nzchar(trimws(api_key))) return(trimws(api_key))
  k <- Sys.getenv(env_var, unset = "")
  if (!nzchar(k)) stop("Missing OpenTopography API key. Set env var ", env_var, " or pass api_key=...")
  k
}

fetch_epsg_lookup_from_catalog <- function(api_key,
                                           bbox = c(minX = -120, minY = 35, maxX = -119, maxY = 36),
                                           catalog_url_base = "https://portal.opentopography.org/API/otCatalog") {
  api_key <- get_opentopo_key(api_key = api_key)
  
  url <- paste0(
    catalog_url_base, "?",
    "minX=", bbox["minX"], "&minY=", bbox["minY"],
    "&maxX=", bbox["maxX"], "&maxY=", bbox["maxY"],
    "&outputFormat=JSON",
    "&apiKey=", api_key
  )
  
  resp <- httr::GET(url, httr::timeout(60))
  httr::stop_for_status(resp)
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  
  # hardening: sometimes returns HTML if key is wrong
  if (!nzchar(txt) || !grepl("^\\s*\\{", txt)) {
    snippet <- substr(gsub("[\r\n\t]", " ", txt), 1, 200)
    stop("OT catalog did not return JSON. Check API key / network. Response starts with: ", snippet)
  }
  
  lst <- jsonlite::fromJSON(txt, flatten = TRUE, simplifyDataFrame = TRUE)
  datasets_df <- lst$Datasets
  if (is.null(datasets_df) || nrow(datasets_df) == 0) stop("Catalog returned no datasets for this bbox.")
  
  lidar_datasets <- datasets_df %>% filter(Dataset.fileFormat == "Point Cloud Data")
  
  get_epsg_horizontal <- function(ap) {
    if (is.null(ap) || !is.data.frame(ap)) return(NA_integer_)
    ix <- which(ap$name == "EPSG (Horizontal)")
    if (length(ix) == 0) return(NA_integer_)
    suppressWarnings(as.integer(ap$value[ix[1]]))
  }
  
  epsg_lookup <- lidar_datasets %>%
    transmute(
      dataset_alt  = Dataset.alternateName,
      dataset_name = dplyr::coalesce(
        if ("Dataset.name"  %in% names(lidar_datasets)) lidar_datasets$Dataset.name  else NA_character_,
        if ("Dataset.title" %in% names(lidar_datasets)) lidar_datasets$Dataset.title else NA_character_,
        Dataset.alternateName
      ),
      epsg_h = purrr::map_int(Dataset.spatialCoverage.additionalProperty, get_epsg_horizontal)
    ) %>%
    filter(!is.na(dataset_alt), nzchar(dataset_alt)) %>%
    group_by(dataset_alt) %>%
    summarise(
      epsg_h = dplyr::first(na.omit(epsg_h)),
      dataset_name = dplyr::first(na.omit(dataset_name)),
      .groups = "drop"
    )
  
  epsg_lookup
}

# ---- year extractor ----
extract_year_2014_2026 <- function(x, year_min = 2014, year_max = 2026) {
  x <- ifelse(is.na(x), "", as.character(x))
  m <- regmatches(x, gregexpr("(?<!\\d)(19\\d{2}|20\\d{2})(?!\\d)", x, perl = TRUE))
  
  vapply(m, function(v) {
    if (length(v) == 0) return(NA_integer_)
    v_int <- suppressWarnings(as.integer(v))
    v_ok <- v_int[v_int >= year_min & v_int <= year_max]
    if (length(v_ok) == 0) return(NA_integer_)
    v_ok[1]
  }, integer(1))
}

progress_line <- function(i, n, prefix = "Processing") {
  if (n < 1) return(invisible(NULL))
  pct <- floor((i / n) * 100)
  width <- 30
  filled <- floor((pct / 100) * width)
  bar <- paste0("[", paste0(rep("#", filled), collapse = ""),
                paste0(rep("-", width - filled), collapse = ""), "]")
  msg <- sprintf("\r%s %s %3d%% (%d/%d)", prefix, bar, pct, i, n)
  cat(msg)
  if (i == n) cat("\n")
  invisible(NULL)
}

# ============================================================
# OT local TileIndex zips -> overlap AOI -> LAZ links
# Returns data.table with columns: file_name, year, laz_link
# ============================================================
tileindex_filter_by_aoi_to_links <- function(tilezip_dir,
                                             aoi_path,
                                             api_key = NULL,
                                             env_var = "OPENTOPO_API_KEY",
                                             catalog_bbox = c(minX=-120, minY=35, maxX=-119, maxY=36),
                                             show_progress = TRUE) {
  sf::sf_use_s2(FALSE)
  
  api_key <- get_opentopo_key(env_var = env_var, api_key = api_key)
  
  epsg_lookup <- suppressWarnings(
    suppressMessages(
      fetch_epsg_lookup_from_catalog(api_key = api_key, bbox = catalog_bbox)
    )
  )
  
  clean_names_lower <- function(x) { names(x) <- tolower(names(x)); x }
  
  find_vector_file <- function(files) {
    exts <- c("gpkg","geojson","json","shp","fgb","csv","tsv","txt")
    for (e in exts) {
      hit <- files[tolower(tools::file_ext(files)) == e]
      if (length(hit) > 0) return(hit[1])
    }
    NA_character_
  }
  
  read_tileindex_any <- function(path) {
    ext <- tolower(tools::file_ext(path))
    if (ext %in% c("gpkg","shp","geojson","json","fgb")) {
      if (ext == "gpkg") {
        layers <- sf::st_layers(path)$name
        if (length(layers) == 0) stop("No layers in gpkg: ", path)
        return(suppressMessages(sf::st_read(path, layer = layers[1], quiet = TRUE)))
      } else {
        return(suppressMessages(sf::st_read(path, quiet = TRUE)))
      }
    }
    if (ext %in% c("csv","tsv","txt")) {
      sep <- ifelse(ext == "csv", ",", "\t")
      df <- data.table::fread(path, sep = sep, data.table = FALSE, showProgress = FALSE)
      return(df)
    }
    stop("Unsupported file type: ", path)
  }
  
  standardize_cols_min <- function(x) {
    x <- clean_names_lower(x)
    nms <- names(x)
    
    if ("filename" %in% nms && !"file_name" %in% nms) names(x)[names(x) == "filename"] <- "file_name"
    if ("file"     %in% nms && !"file_name" %in% nms) names(x)[names(x) == "file"]     <- "file_name"
    
    if ("URL" %in% names(x) && !"url" %in% nms) names(x)[names(x) == "URL"] <- "url"
    if ("Url" %in% names(x) && !"url" %in% nms) names(x)[names(x) == "Url"] <- "url"
    x
  }
  
  make_polys_from_bbox_cols <- function(df, crs_epsg) {
    needed <- c("min_x","max_x","min_y","max_y")
    if (!all(needed %in% names(df))) stop("Missing bbox columns: ", paste(setdiff(needed, names(df)), collapse = ", "))
    
    df$min_x <- as.numeric(df$min_x); df$max_x <- as.numeric(df$max_x)
    df$min_y <- as.numeric(df$min_y); df$max_y <- as.numeric(df$max_y)
    
    geom <- purrr::pmap(
      list(df$min_x, df$min_y, df$max_x, df$max_y),
      function(xmin, ymin, xmax, ymax) {
        sf::st_polygon(list(matrix(
          c(xmin,ymin, xmax,ymin, xmax,ymax, xmin,ymax, xmin,ymin),
          ncol = 2, byrow = TRUE
        )))
      }
    )
    sf::st_sf(df, geometry = sf::st_sfc(geom, crs = crs_epsg))
  }
  
  try_transform_4326_safe <- function(sfobj) {
    sfobj <- suppressWarnings(sf::st_zm(sfobj, drop = TRUE, what = "ZM"))
    sfobj <- suppressWarnings(sf::st_make_valid(sfobj))
    out <- suppressWarnings(sf::st_transform(sfobj, 4326))
    
    bb <- sf::st_bbox(out)
    ok <- is.finite(bb["xmin"]) && is.finite(bb["xmax"]) &&
      is.finite(bb["ymin"]) && is.finite(bb["ymax"]) &&
      bb["xmin"] >= -180 && bb["xmax"] <= 180 &&
      bb["ymin"] >=  -90 && bb["ymax"] <=  90 &&
      bb["xmin"] <  bb["xmax"] &&
      bb["ymin"] <  bb["ymax"]
    if (!ok) return(NULL)
    
    out <- out[!sf::st_is_empty(out), ]
    if (nrow(out) == 0) return(NULL)
    out
  }
  
  tiles_overlap_aoi <- function(tiles_sf, aoi_sf) {
    tiles_sf <- sf::st_make_valid(tiles_sf)
    
    bb_aoi <- sf::st_bbox(aoi_sf)
    bb_ok  <- suppressWarnings(sf::st_intersects(tiles_sf, sf::st_as_sfc(bb_aoi), sparse = FALSE)[,1])
    tiles_sf <- tiles_sf[bb_ok, , drop = FALSE]
    if (nrow(tiles_sf) == 0) return(NULL)
    
    hit  <- suppressWarnings(sf::st_intersects(tiles_sf, aoi_sf, sparse = FALSE))
    keep <- rowSums(hit) > 0
    tiles_sf <- tiles_sf[keep, , drop = FALSE]
    if (nrow(tiles_sf) == 0) return(NULL)
    
    tiles_sf
  }
  
  # ---- read AOI ----
  aoi <- read_aoi_any(aoi_path)
  
  # ---- process zips ----
  zip_files <- list.files(tilezip_dir, pattern = "_TileIndex\\.zip$", full.names = TRUE)
  if (length(zip_files) == 0) stop("No *_TileIndex.zip found in: ", tilezip_dir)
  
  nzip <- length(zip_files)
  out_list <- vector("list", nzip)
  
  for (i in seq_along(zip_files)) {
    if (isTRUE(show_progress)) progress_line(i, nzip, prefix = "TileIndex")
    
    z   <- zip_files[i]
    alt <- sub("_TileIndex\\.zip$", "", basename(z))
    
    epsg_h <- epsg_lookup$epsg_h[match(alt, epsg_lookup$dataset_alt)]
    if (is.na(epsg_h)) next
    
    ds_name <- epsg_lookup$dataset_name[match(alt, epsg_lookup$dataset_alt)]
    if (is.na(ds_name) || !nzchar(ds_name)) ds_name <- alt
    
    tmp <- tempfile(pattern = paste0("tileindex_", alt, "_"))
    dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
    utils::unzip(z, exdir = tmp)
    
    all_files <- list.files(tmp, recursive = TRUE, full.names = TRUE)
    f <- find_vector_file(all_files)
    if (is.na(f)) { unlink(tmp, recursive = TRUE, force = TRUE); next }
    
    obj <- read_tileindex_any(f)
    obj <- standardize_cols_min(obj)
    
    if (!inherits(obj, "sf")) {
      nm <- names(obj)
      ren <- c("minx"="min_x","maxx"="max_x","miny"="min_y","maxy"="max_y")
      for (k in names(ren)) if (k %in% nm && !(ren[[k]] %in% nm)) names(obj)[names(obj) == k] <- ren[[k]]
      obj_sf <- make_polys_from_bbox_cols(obj, crs_epsg = epsg_h)
    } else {
      obj_sf <- obj
      if (is.na(sf::st_crs(obj_sf))) sf::st_crs(obj_sf) <- epsg_h
    }
    
    obj_sf$dataset_alt <- alt
    
    obj_4326 <- try_transform_4326_safe(obj_sf)
    if (is.null(obj_4326)) { unlink(tmp, recursive = TRUE, force = TRUE); next }
    
    obj_4326 <- standardize_cols_min(obj_4326)
    if (!("url" %in% names(obj_4326))) { unlink(tmp, recursive = TRUE, force = TRUE); next }
    
    if (!("file_name" %in% names(obj_4326))) obj_4326$file_name <- basename(obj_4326$url)
    
    hit_sf <- tiles_overlap_aoi(obj_4326, aoi)
    if (is.null(hit_sf)) { unlink(tmp, recursive = TRUE, force = TRUE); next }
    
    hit_df <- sf::st_drop_geometry(hit_sf)
    hit_df$laz_link  <- hit_df$url
    hit_df$file_name <- basename(hit_df$file_name)
    
    y1 <- extract_year_2014_2026(hit_df$file_name)
    y2 <- extract_year_2014_2026(hit_df$laz_link)
    y3 <- rep(extract_year_2014_2026(ds_name), length(y1))
    hit_df$year <- dplyr::coalesce(y1, y2, y3)
    
    out_list[[i]] <- hit_df[, c("file_name", "year", "laz_link")]
    unlink(tmp, recursive = TRUE, force = TRUE)
  }
  
  out_list <- out_list[!vapply(out_list, is.null, logical(1))]
  hits_df <- if (length(out_list) == 0) data.table::data.table()
  else data.table::rbindlist(out_list, fill = TRUE)
  
  if (nrow(hits_df) > 0) hits_df <- unique(hits_df)
  hits_df
}

# ============================================================
# Planetary Computer (3DEP COPC) via STAC
# Returns tibble(file_name, year, laz_link)
# ============================================================
aoi_to_geojson_list <- function(aoi_sf) {
  aoi_wgs <- st_transform(aoi_sf, 4326) |>
    st_make_valid() |>
    st_union() |>
    st_as_sf()
  gj <- geojsonsf::sf_geojson(aoi_wgs)
  jsonlite::fromJSON(gj, simplifyVector = TRUE)
}

stac_post_search_all <- function(s, ...) {
  items <- s %>% stac_search(...) %>% post_request()
  
  repeat {
    if (is.null(items$links) || length(items$links) == 0) break
    rels <- vapply(items$links, function(x) x$rel %||% NA_character_, character(1))
    if (!any(rels == "next", na.rm = TRUE)) break
    
    nxt <- tryCatch(items %>% items_next() %>% get_request(), error = function(e) NULL)
    if (is.null(nxt) || is.null(nxt$features) || length(nxt$features) == 0) break
    
    items$features <- c(items$features, nxt$features)
    items$links <- nxt$links
  }
  
  items
}

extract_year_from_feature <- function(f) {
  dt <- f$properties$datetime
  if (is.null(dt) || is.na(dt) || !nzchar(dt)) dt <- f$properties$start_datetime
  if (is.null(dt) || is.na(dt) || !nzchar(dt)) dt <- f$properties$end_datetime
  if (is.null(dt) || is.na(dt) || !nzchar(dt)) return(NA_integer_)
  y <- suppressWarnings(as.integer(substr(dt, 1, 4)))
  if (is.na(y)) NA_integer_ else y
}

inventory_3dep_copc_by_aoi <- function(aoi_sf,
                                       year_max = 2025,
                                       datetime_min = "2010-01-01",
                                       datetime_max = NULL,
                                       keep_only_with_year = TRUE,
                                       .log_env = NULL) {
  if (is.null(datetime_max)) datetime_max <- paste0(year_max, "-12-31")
  
  aoi_gj <- suppressWarnings(aoi_to_geojson_list(aoi_sf))
  s <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")
  
  items <- stac_post_search_all(
    s,
    collections = "3dep-lidar-copc",
    intersects  = aoi_gj,
    datetime    = paste0(datetime_min, "/", datetime_max),
    limit       = 500
  )
  
  if (length(items$features) == 0) {
    return(dplyr::tibble(file_name = character(), year = integer(), laz_link = character()))
  }
  
  items_signed <- rstac::items_sign_planetary_computer(items)
  feats <- items_signed$features
  
  laz_link <- vapply(feats, function(f) {
    href <- f$assets$data$href
    if (is.null(href) || is.na(href) || !nzchar(href)) NA_character_ else href
  }, character(1))
  
  file_name <- vapply(laz_link, function(u) {
    if (is.na(u)) return(NA_character_)
    basename(strsplit(u, "\\?", fixed = FALSE)[[1]][1])
  }, character(1))
  
  year <- vapply(feats, extract_year_from_feature, integer(1))
  
  out <- dplyr::tibble(
    file_name = file_name,
    year      = year,
    laz_link  = laz_link
  ) |>
    dplyr::filter(!is.na(laz_link), !is.na(file_name)) |>
    dplyr::distinct(laz_link, .keep_all = TRUE) |>
    dplyr::arrange(year, file_name)
  
  out <- out |> dplyr::filter(is.na(year) | year <= year_max)
  if (keep_only_with_year) out <- out |> dplyr::filter(!is.na(year))
  
  if (!is.null(.log_env)) {
    .log_env$n_items <- length(feats)
    .log_env$n_out   <- nrow(out)
    .log_env$years   <- sort(unique(out$year))
  }
  
  out
}

# ============================================================
# ADAPTERS (used by app.R)
# ============================================================
ot_tileindex_search_aoi <- function(tilezip_dir, aoi_sf,
                                    api_key = NULL,
                                    show_progress = FALSE,
                                    catalog_bbox = NULL) {
  tmp_aoi <- tempfile(fileext = ".geojson")
  sf::st_write(aoi_sf, tmp_aoi, quiet = TRUE, delete_dsn = TRUE)
  
  if (is.null(catalog_bbox)) {
    bb <- sf::st_bbox(sf::st_transform(aoi_sf, 4326))
    catalog_bbox <- c(minX = bb["xmin"], minY = bb["ymin"], maxX = bb["xmax"], maxY = bb["ymax"])
  }
  
  out <- tileindex_filter_by_aoi_to_links(
    tilezip_dir   = tilezip_dir,
    aoi_path      = tmp_aoi,
    api_key       = api_key,
    catalog_bbox  = catalog_bbox,
    show_progress = show_progress
  )
  
  as.data.frame(out)
}

pc_copc_search_aoi <- function(aoi_sf,
                               datetime_min = "2010-01-01",
                               datetime_max = NULL,
                               year_max = NULL) {
  if (is.null(year_max)) year_max <- as.integer(format(Sys.Date(), "%Y"))
  inventory_3dep_copc_by_aoi(
    aoi_sf = aoi_sf,
    year_max = year_max,
    datetime_min = datetime_min,
    datetime_max = datetime_max,
    keep_only_with_year = TRUE
  )
}

# ============================================================
# Downloaders used by app.R
# ============================================================
download_laz_parallel_no_progress <- function(urls,
                                              output_dir,
                                              cores = NULL,
                                              reserve_cores = 6L,
                                              timeout = 600) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  laz_dir <- file.path(output_dir, "LAZ")
  if (!dir.exists(laz_dir)) dir.create(laz_dir, recursive = TRUE, showWarnings = FALSE)
  
  urls <- trimws(gsub(",+$", "", urls))
  urls <- unique(urls[nzchar(urls)])
  if (length(urls) == 0) stop("No valid URLs provided.")
  
  total_cores <- tryCatch(parallel::detectCores(logical = TRUE), error = function(e) 1L)
  if (is.null(cores)) {
    workers <- max(1L, total_cores - reserve_cores)
  } else {
    workers <- min(as.integer(cores), total_cores)
  }
  if (workers < 1L) workers <- 1L
  
  old_timeout <- getOption("timeout")
  on.exit({ options(timeout = old_timeout) }, add = TRUE)
  options(timeout = timeout)
  
  old_plan <- future::plan()
  on.exit({ future::plan(old_plan) }, add = TRUE)
  future::plan(future::multisession, workers = workers)
  
  existing_files <- list.files(laz_dir, pattern = "\\.laz$", full.names = FALSE, ignore.case = TRUE)
  urls_to_download <- urls[!(basename(urls) %in% existing_files)]
  
  n_existing <- sum(basename(urls) %in% existing_files)
  n_new <- length(urls_to_download)
  
  download_tile <- function(url, laz_dir) {
    file_name <- basename(url)
    destfile  <- file.path(laz_dir, file_name)
    
    out <- tryCatch({
      resp_check <- httr::HEAD(url)
      code <- httr::status_code(resp_check)
      
      if (code == 200) {
        httr::GET(url, httr::write_disk(destfile, overwrite = TRUE), httr::timeout(timeout))
        c(file_name, "Downloaded", url)
      } else {
        c(file_name, paste0("Failed (", code, ")"), url)
      }
    }, error = function(e) {
      c(file_name, paste0("Error: ", e$message), url)
    })
    out
  }
  
  results <- list()
  if (n_new > 0) {
    results <- future.apply::future_lapply(urls_to_download, download_tile, laz_dir = laz_dir)
  }
  
  log_status <- data.frame(
    filename = c(
      basename(urls[basename(urls) %in% existing_files]),
      if (length(results) > 0) vapply(results, `[[`, character(1), 1) else character(0)
    ),
    status = c(
      rep("Already exists", n_existing),
      if (length(results) > 0) vapply(results, `[[`, character(1), 2) else character(0)
    ),
    source = c(
      rep(NA_character_, n_existing),
      if (length(results) > 0) vapply(results, `[[`, character(1), 3) else character(0)
    ),
    stringsAsFactors = FALSE
  )
  
  log_path <- file.path(laz_dir, "FILE_PROCESSING.txt")
  write.table(log_status, file = log_path, row.names = FALSE, sep = "\t", quote = FALSE)
  
  invisible(log_status)
}

download_laz_parallel_no_progress_copc <- function(urls,
                                                   output_dir,
                                                   cores = NULL,
                                                   reserve_cores = 6L,
                                                   timeout = 600) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  laz_dir <- file.path(output_dir, "LAZ")
  if (!dir.exists(laz_dir)) dir.create(laz_dir, recursive = TRUE, showWarnings = FALSE)
  
  urls <- as.character(urls)
  urls <- trimws(gsub(",+$", "", urls))
  urls <- unique(urls[nzchar(urls) & !is.na(urls)])
  if (length(urls) == 0) stop("No valid URLs provided.")
  
  total_cores <- tryCatch(parallel::detectCores(logical = TRUE), error = function(e) 1L)
  if (is.null(cores)) {
    workers <- max(1L, total_cores - as.integer(reserve_cores))
  } else {
    workers <- min(as.integer(cores), total_cores)
  }
  if (!is.finite(workers) || workers < 1L) workers <- 1L
  
  old_timeout <- getOption("timeout")
  on.exit({ options(timeout = old_timeout) }, add = TRUE)
  options(timeout = timeout)
  
  old_plan <- future::plan()
  on.exit({ future::plan(old_plan) }, add = TRUE)
  future::plan(future::multisession, workers = workers)
  
  clean_name <- function(u) basename(sub("\\?.*$", "", u))
  existing_files <- list.files(laz_dir, pattern = "\\.(laz|copc\\.laz)$", full.names = FALSE, ignore.case = TRUE)
  
  urls_to_download <- urls[!(clean_name(urls) %in% existing_files)]
  
  n_existing <- sum(clean_name(urls) %in% existing_files)
  n_new <- length(urls_to_download)
  
  download_tile <- function(url, laz_dir) {
    file_name <- basename(sub("\\?.*$", "", url))
    destfile  <- file.path(laz_dir, file_name)
    
    out <- tryCatch({
      resp_check <- httr::HEAD(url)
      code <- httr::status_code(resp_check)
      
      if (!is.na(code) && code == 200) {
        httr::GET(url, httr::write_disk(destfile, overwrite = TRUE), httr::timeout(timeout))
        c(file_name, "Downloaded", url)
      } else {
        c(file_name, paste0("Failed (", code, ")"), url)
      }
    }, error = function(e) {
      c(file_name, paste0("Error: ", e$message), url)
    })
    out
  }
  
  results <- list()
  if (n_new > 0) {
    results <- future.apply::future_lapply(urls_to_download, download_tile, laz_dir = laz_dir)
  }
  
  log_status <- data.frame(
    filename = c(
      clean_name(urls[clean_name(urls) %in% existing_files]),
      if (length(results) > 0) vapply(results, `[[`, character(1), 1) else character(0)
    ),
    status = c(
      rep("Already exists", n_existing),
      if (length(results) > 0) vapply(results, `[[`, character(1), 2) else character(0)
    ),
    source = c(
      rep(NA_character_, n_existing),
      if (length(results) > 0) vapply(results, `[[`, character(1), 3) else character(0)
    ),
    stringsAsFactors = FALSE
  )
  
  log_path <- file.path(laz_dir, "FILE_PROCESSING.txt")
  write.table(log_status, file = log_path, row.names = FALSE, sep = "\t", quote = FALSE)
  
  invisible(log_status)
}

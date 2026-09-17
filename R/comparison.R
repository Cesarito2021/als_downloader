# Campaign identity includes the project and acquisition interval, never the upload year.
campaign_groups <- function(tiles) {
  if (is.null(tiles) || !nrow(tiles)) return(list())
  project <- tiles$dataset
  usgs <- tiles$provider == "usgs3dep"
  project[usgs] <- basename(dirname(dirname(redact_url(tiles$url[usgs]))))
  date <- function(x) ifelse(is.na(x) | !nzchar(x), "unknown", x)
  interval <- paste0(date(tiles$acquired_start), " to ", date(tiles$acquired_end))
  invalid <- !is.na(tiles$acquired_start) & !is.na(tiles$acquired_end) & tiles$acquired_start > tiles$acquired_end
  interval[invalid] <- paste(interval[invalid], "[invalid source interval]")
  label <- paste(project, interval, sep = " | ")
  split(seq_len(nrow(tiles)), label)
}

comparison_grid <- function(a, b, resolution = 10, min_points = 5L) {
  if (length(resolution) != 1L || !is.finite(resolution) || resolution < 1 || resolution > 100)
    stop("Choose a grid resolution between 1 and 100 metres.")
  if (length(min_points) != 1L || !is.finite(min_points) || min_points < 3 || min_points != floor(min_points))
    stop("At least three points per cell and epoch are required.")
  aggregate_epoch <- function(p, suffix) {
    p <- p[is.finite(p$X) & is.finite(p$Y) & is.finite(p$Z), c("X", "Y", "Z")]
    if (!nrow(p)) stop("No finite AOI points in one campaign.")
    ix <- floor(p$X / resolution); iy <- floor(p$Y / resolution)
    cells <- split(seq_len(nrow(p)), paste(ix, iy, sep = ":"))
    first <- vapply(cells, `[`, integer(1), 1L)
    out <- data.frame(ix = ix[first], iy = iy[first], n = lengths(cells),
      p95 = vapply(cells, function(i) unname(stats::quantile(p$Z[i], .95, type = 7)), numeric(1)))
    names(out)[3:4] <- paste0(c("n_", "p95_"), suffix)
    out
  }
  cells <- merge(aggregate_epoch(a, "a"), aggregate_epoch(b, "b"), by = c("ix", "iy"), all = TRUE)
  cells$x <- (cells$ix + .5) * resolution; cells$y <- (cells$iy + .5) * resolution
  cells$eligible <- !is.na(cells$n_a) & !is.na(cells$n_b) & cells$n_a >= min_points & cells$n_b >= min_points
  cells$delta_b_minus_a <- ifelse(cells$eligible, cells$p95_b - cells$p95_a, NA_real_)
  cells
}

read_comparison_cloud <- function(path, aoi) {
  header <- lidR::readLASheader(path)
  crs <- sf::st_crs(header)
  if (is.na(crs) || isTRUE(sf::st_is_longlat(crs)) || !isTRUE(crs$units_gdal %in% c("metre", "meter", "metres", "meters", "m")))
    stop("Comparison requires an embedded projected CRS in metres. Reproject unsupported files externally first.")
  n <- header@PHB[["Extended Number of point records"]]
  if (is.null(n) || n == 0) n <- header@PHB[["Number of point records"]]
  if (is.null(n) || !is.finite(n) || n > 50000000) stop("A comparison tile exceeds the 50-million source-point limit.")
  roi <- sf::st_transform(aoi, crs)
  bb <- sf::st_bbox(roi)
  las <- lidR::readLAS(path, select = "xyzc", filter = paste("-drop_withheld -drop_class 7 18 -inside", paste(format(as.numeric(bb), scientific = FALSE, trim = TRUE), collapse = " ")))
  if (is.null(las) || !nrow(las@data)) return(list(points = data.frame(X = numeric(), Y = numeric(), Z = numeric()), crs = crs))
  if (nrow(las@data) > 2000000) stop("More than two million AOI bounding-box points; use a smaller AOI.")
  points <- as.data.frame(las@data)
  if ("Classification" %in% names(points)) points <- points[!points$Classification %in% c(7L, 18L), ]
  points <- points[is.finite(points$X) & is.finite(points$Y) & is.finite(points$Z), c("X", "Y", "Z")]
  if (nrow(points)) {
    inside <- lengths(sf::st_intersects(sf::st_as_sf(points, coords = c("X", "Y"), crs = crs), roi)) > 0
    points <- points[inside, , drop = FALSE]
  }
  list(points = points, crs = crs)
}

compare_campaigns <- function(a, b, aoi, resolution, min_points, directory) {
  if (aoi_area(aoi) > .25) stop("Comparison is limited to a 0.25 square kilometre AOI; draw a smaller area.")
  if (aoi_area(aoi) * 1e6 / resolution^2 > 50000) stop("Increase cell size: comparison is limited to approximately 50,000 AOI grid cells.")
  if (!nrow(a) || !nrow(b) || nrow(a) > 4L || nrow(b) > 4L)
    stop("Use an AOI intersecting at most four tiles per campaign for this preview.")
  reference <- NULL
  read_epoch <- function(rows, prefix) {
    remaining <- 200 * 1024^2
    point_budget <- 2000000L
    clouds <- lapply(seq_len(nrow(rows)), function(i) {
      writeLines(sprintf("Campaign %s: downloading/reading tile %s of %s", toupper(prefix), i, nrow(rows)), file.path(directory, "progress.txt"))
      cloud <- preview_remote_tile(rows[i, , drop = FALSE], max_bytes = min(100 * 1024^2, remaining),
        path = file.path(directory, paste0(prefix, i, ".laz")),
        reader = function(path) {
          remaining <<- remaining - file.size(path)
          read_comparison_cloud(path, aoi)
        })
      if (is.null(reference)) reference <<- cloud$crs
      else if (!isTRUE(reference == cloud$crs)) stop("Campaign CRS definitions differ. Align horizontal and vertical references externally before comparison.")
      point_budget <<- point_budget - nrow(cloud$points)
      if (point_budget < 0) stop("More than two million retained AOI points in a campaign; reduce the AOI.")
      cloud$points
    })
    points <- unique(do.call(rbind, clouds))
    if (!nrow(points)) stop("One campaign has no laser points inside the AOI.")
    if (nrow(points) > 2000000) stop("More than two million AOI points in a campaign; reduce the AOI.")
    points
  }
  pa <- read_epoch(a, "a"); pb <- read_epoch(b, "b")
  writeLines("Building the shared AOI grid and display samples...", file.path(directory, "progress.txt"))
  grid <- comparison_grid(pa, pb, resolution, min_points)
  origin <- vapply(rbind(pa, pb), min, numeric(1))
  sample_cloud <- function(p) {
    p <- p[unique(round(seq(1, nrow(p), length.out = min(50000, nrow(p))))), ]
    p[] <- Map(function(x, offset) x - offset, p, origin)
    unname(as.matrix(p))
  }
  list(a = sample_cloud(pa), b = sample_cloud(pb), origin = unname(origin), grid = grid,
    counts = c(nrow(pa), nrow(pb)), crs = reference$wkt, resolution = resolution,
    method = "Difference of cell P95 source elevation (B minus A); withheld and classes 7/18 excluded; exact duplicate XYZ removed; no interpolation or registration")
}

comparison_gate <- function(result, dates, verified, vertical_a, vertical_b) {
  if (is.null(result)) return("Load two campaigns to begin.")
  if (anyNA(unlist(dates)) || any(!nzchar(unlist(dates)))) return("Acquisition dates are unknown; temporal differences remain unavailable.")
  values <- unlist(dates, use.names = FALSE)
  if (any(!grepl("^\\d{4}-\\d{2}-\\d{2}$", values)) || anyNA(as.Date(values, format = "%Y-%m-%d")))
    return("Invalid acquisition interval in source metadata; temporal differences remain unavailable until the source dates are corrected.")
  if (any(dates$a$acquired_start > dates$a$acquired_end) || any(dates$b$acquired_start > dates$b$acquired_end))
    return("Invalid acquisition interval in source metadata; temporal differences remain unavailable until the source dates are corrected.")
  if (max(dates$a$acquired_end) >= min(dates$b$acquired_start)) return("For temporal differences, choose A entirely before B with non-overlapping acquisition intervals.")
  if (!isTRUE(verified) || is.null(vertical_a) || is.null(vertical_b) || !nzchar(trimws(vertical_a)) ||
      tolower(trimws(vertical_a)) != tolower(trimws(vertical_b)))
    return("Verify matching vertical references and metre Z units before interpreting a difference.")
  if (!any(result$grid$eligible)) return("No shared grid cells meet the minimum point count in both campaigns. No difference can be inferred.")
  "Exploratory difference enabled: positive = higher P95 in B; negative = lower P95 in B. Missing/undersampled cells are not zero change."
}

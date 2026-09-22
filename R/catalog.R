#' Inspect provider access and implementation status
#' @return A data frame with provider identifiers, access notes, source links,
#'   and whether a tile-search adapter is implemented. Implemented does not
#'   mean that every dataset or endpoint is currently available.
#' @export
#' @examples
#' provider_catalog()
provider_catalog <- function() {
  utils::read.csv(system.file("extdata", "providers.csv", package = "ALSdownloadeR"),
                  stringsAsFactors = FALSE)
}

empty_tiles <- function() {
  sf::st_sf(tile_id = character(), provider = character(), dataset = character(),
    filename = character(), url = character(), acquired_start = character(),
    acquired_end = character(), size_bytes = numeric(), license_url = character(),
    citation = character(), geometry = sf::st_sfc(crs = 4326))
}

request_json <- function(url, body = NULL, query = NULL) {
  tryCatch({
    response <- if (is.null(body) && is.null(query)) httr::GET(url, httr::timeout(60))
      else if (is.null(body)) httr::GET(url, query = query, httr::timeout(60))
      else httr::POST(url, body = body, encode = "json", httr::timeout(60))
    if (httr::status_code(response) >= 400)
      stop(sprintf("Provider returned HTTP %s.", httr::status_code(response)))
    jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  }, error = function(e) stop("Provider request failed. Check connectivity, access and service status.", call. = FALSE))
}

#' Find point-cloud tiles intersecting an AOI
#' @param aoi Polygon input accepted by [read_aoi()].
#' @param provider Either `"usgs3dep"` (USGS The National Map) or `"planetary"` (optional COPC mirror) or
#'   `"opentopography"` (verified hosted TileIndex catalogue), or `"contributed"`
#'   (maintainer-approved local `*.tiles.geojson` indexes), `"ahn6"`
#'   (native AHN6 index), `"swisstopo"` (swissSURFACE3D STAC), `"ignfr"`
#'   (IGN LiDAR HD, indexed through a public STAC catalogue maintained by
#'   UMR TETIS / INRAE; not IGN's own WFS), or `"canelevation"` (Canada;
#'   official NRCan spatial tile service, or a configured local `.gpkg`/`.shp`
#'   index).
#' @param start,end Optional inclusive acquisition dates in `YYYY-MM-DD` format.
#'   Unknown acquisition dates remain in the results.
#' @param tile_index_dir Directory of OpenTopography `*_TileIndex.zip` files,
#'   approved contributed `*.tiles.geojson` files, or CanElevation `.gpkg`/
#'   `.shp` indexes. Required for contributed sources; optional for OpenTopography and Canada. Indexes require an embedded
#'   CRS and a `url` field pointing to the original file.
#' @param max_items Maximum number of tiles to return. An incomplete result
#'   raises an error instead of silently reporting partial coverage.
#' @return An `sf` table of assets with stable identifiers, canonical URLs,
#'   acquisition dates, citation information and tile geometry in EPSG:4326.
#'   `campaign_id` contains the reported 3DEP project or IGN mission identifier.
#'   `campaign_group` and `campaign_basis` also describe fallback groups from
#'   recognized project directories, delivery blocks or datasets. Fallback
#'   groups do not establish distinct flight dates.
#' @details Network access occurs only when this function is explicitly called
#'   for a remote source. OpenTopography searches the packaged, audited airborne-LiDAR
#'   registry by default and fetches matching provider indexes with SHA-256 verification.
#'   An explicit local index directory overrides that route in the R API.
#'   Public tile downloads do not require an API key. Asset licenses vary per dataset.
#' @export
#' @examples
#' if (interactive()) {
#'   # tiles <- find_tiles("aoi.gpkg", provider = "usgs3dep")
#' }
find_tiles <- function(aoi, provider = c("usgs3dep", "planetary", "opentopography", "contributed", "ahn6", "swisstopo", "ignfr", "canelevation"),
                       start = NULL, end = NULL, tile_index_dir = NULL,
                       max_items = 10000L) {
  provider <- match.arg(provider)
  aoi <- read_aoi(aoi)
  for (value in list(start, end)) if (!is.null(value) &&
      (length(value) != 1L || is.na(as.Date(value)) || !grepl("^\\d{4}-\\d{2}-\\d{2}$", value)))
    stop("Dates must use YYYY-MM-DD.", call. = FALSE)
  if (!is.null(start) && !is.null(end) && as.Date(start) > as.Date(end))
    stop("Start date must not follow end date.", call. = FALSE)
  if (length(max_items) != 1L || !is.finite(max_items) || max_items < 1)
    stop("max_items must be positive.", call. = FALSE)
  tiles <- switch(provider, usgs3dep = find_als_usgs(aoi, max_items), planetary = search_planetary(aoi, max_items),
    opentopography = find_als_opentopography(aoi, tile_index_dir, max_items),
    contributed = search_contributed(aoi, tile_index_dir, max_items),
    ahn6 = find_als_ahn(aoi,max_items),
    swisstopo = find_als_swisstopo(aoi,max_items),
    ignfr = find_als_ign_france(aoi,max_items),
    canelevation = find_als_canelevation(aoi, tile_index_dir, max_items))
  tiles <- extract_als_dates(tiles)
  if (!"campaign_id" %in% names(tiles)) tiles$campaign_id <- rep(NA_character_, nrow(tiles))
  if (nrow(tiles)) {
    # Search without a date filter so undated surveys are not silently lost.
    keep <- rep(TRUE, nrow(tiles))
    if (!is.null(start)) keep <- keep & (is.na(tiles$acquired_end) | tiles$acquired_end >= start)
    if (!is.null(end)) keep <- keep & (is.na(tiles$acquired_start) | tiles$acquired_start <= end)
    tiles <- tiles[keep, , drop = FALSE]
  }
  get_als_file_sizes(tile_campaign_metadata(tiles))
}

# Keep the provider's acquisition interval; generic catalog timestamps may be nominal.
stac_acquisition_period <- function(properties) {
  date <- function(x) {
    if (!is.character(x) || length(x) != 1L || is.na(x)) return(NA_character_)
    value <- substr(x, 1, 10)
    if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", value)) return(NA_character_)
    parsed <- suppressWarnings(as.Date(value, format = "%Y-%m-%d"))
    if (is.na(parsed) || format(parsed, "%Y-%m-%d") != value) NA_character_ else value
  }
  period <- c(start = date(properties$start_datetime), end = date(properties$end_datetime))
  # Some provider records have inverted intervals. Do not invent a correction
  # or use these intervals to exclude a potentially relevant survey.
  if (!anyNA(period) && period['start'] > period['end']) period[] <- NA_character_
  period
}

country_source_links <- function(catalog, country_code) {
  rows <- catalog[as.character(catalog$country_code) == as.character(country_code), , drop = FALSE]
  shiny::tagList(lapply(seq_len(nrow(rows)), function(i) shiny::div(
    shiny::tags$strong(rows$name[i]),
    shiny::p(rows$access[i]),
    shiny::p(if (isTRUE(rows$implemented[i])) "AOI adapter available in this app; see source requirements."
      else "Download through the official portal. In-app AOI search is not yet available for this source."),
    shiny::tags$a(href=rows$info_url[i], target="_blank", rel="noopener noreferrer",
      "Open official source"))))
}

search_planetary <- function(aoi, max_items) {
  url <- "https://planetarycomputer.microsoft.com/api/stac/v1/search"
  body <- list(collections = list("3dep-lidar-copc"), intersects = aoi_geometry(aoi), limit = 500L)
  features <- list(); visited <- character()
  repeat {
    fingerprint <- digest::digest(list(url, body))
    if (fingerprint %in% visited) stop("Provider pagination repeated; search incomplete.", call. = FALSE)
    visited <- c(visited, fingerprint)
    page <- request_json(url, body)
    features <- c(features, page$features)
    if (length(features) > max_items) stop("Search exceeds max_items. Use a smaller area or raise the explicit limit.", call. = FALSE)
    links <- Filter(function(x) identical(x$rel, "next"), page$links)
    if (!length(links)) break
    nxt <- links[[1]]
    if (!startsWith(nxt$href, "https://planetarycomputer.microsoft.com/"))
      stop("Unexpected pagination host; search incomplete.", call. = FALSE)
    url <- nxt$href
    body <- if (identical(nxt$method, "POST")) {
      if (isTRUE(nxt$merge)) utils::modifyList(body, nxt$body) else nxt$body
    } else NULL
  }
  if (!length(features)) return(empty_tiles())
  rows <- lapply(features, function(f) {
    asset <- f$assets$data
    if (is.null(asset$href) || is.null(f$geometry)) return(NULL)
    p <- f$properties
    period <- stac_acquisition_period(p)
    g <- sf::st_read(jsonlite::toJSON(list(type = "Feature", properties = list(), geometry = f$geometry), auto_unbox = TRUE), quiet = TRUE)
    sf::st_sf(tile_id = f$id, provider = "usgs3dep", dataset = f$collection,
      campaign_id = if (is.character(p[["3dep:usgs_id"]]) && length(p[["3dep:usgs_id"]]) == 1L &&
        !is.na(p[["3dep:usgs_id"]]) && nzchar(trimws(p[["3dep:usgs_id"]]))) p[["3dep:usgs_id"]] else NA_character_,
      filename = basename(sub("\\?.*$", "", asset$href)), url = sub("\\?.*$", "", asset$href),
      acquired_start = unname(period['start']), acquired_end = unname(period['end']),
      size_bytes = if (is.null(asset[["file:size"]])) NA_real_ else as.numeric(asset[["file:size"]]),
      license_url = "https://www.usgs.gov/information-policies-and-instructions/copyrights-and-credits",
      citation = paste("USGS 3DEP;", f$id, "; distributed through Microsoft Planetary Computer. Consult survey metadata for acquisition and producer credits."),
      geometry = sf::st_geometry(g))
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(empty_tiles())
  ans <- do.call(rbind, rows)
  ans[!duplicated(ans$tile_id) & lengths(sf::st_intersects(ans, aoi)) > 0, , drop = FALSE]
}

search_ot <- function(aoi, folder, max_items) {
  if (is.null(folder)) return(search_ot_catalog(aoi, max_items))
  if (is.null(folder) || !dir.exists(folder)) stop("Configure a local OpenTopography TileIndex directory.", call. = FALSE)
  files <- list.files(folder, "_TileIndex\\.zip$", full.names = TRUE, ignore.case = TRUE)
  if (!length(files)) stop("No OpenTopography TileIndex archives found.", call. = FALSE)
  results <- list()
  for (path in files) {
    members <- utils::unzip(path, list = TRUE)
    if (any(grepl("(^/|^[A-Za-z]:|(^|/)\\.\\.(/|$))", gsub("\\\\", "/", members$Name))))
      stop("Unsafe tile-index archive.", call. = FALSE)
    if (sum(members$Length) > 500 * 1024^2) stop("Tile index exceeds 500 MB extraction limit.", call. = FALSE)
    tmp <- tempfile("als-index-"); dir.create(tmp)
    obj <- tryCatch({
      utils::unzip(path, exdir = tmp)
      shp <- list.files(tmp, "\\.(shp|gpkg|geojson)$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
      if (length(shp) != 1L) stop("Each tile archive must contain one spatial index.")
      sf::st_read(shp, quiet = TRUE)
    }, finally = unlink(tmp, recursive = TRUE))
    if (is.na(sf::st_crs(obj))) stop("Tile index has no embedded CRS; provide a corrected index.", call. = FALSE)
    names(obj) <- tolower(names(obj))
    if (!"url" %in% names(obj)) stop("Tile index lacks a URL field.", call. = FALSE)
    obj <- sf::st_transform(sf::st_make_valid(sf::st_zm(obj, drop = TRUE, what = "ZM")), 4326)
    obj <- obj[lengths(sf::st_intersects(obj, aoi)) > 0, , drop = FALSE]
    if (!nrow(obj)) next
    dataset <- sub("_TileIndex\\.zip$", "", basename(path), ignore.case = TRUE)
    href <- as.character(obj$url)
    name <- basename(sub("\\?.*$", "", href))
    rows <- sf::st_sf(tile_id = paste(dataset, name, sep = "/"), provider = "opentopography",
      dataset = dataset, filename = name, url = href, acquired_start = NA_character_,
      acquired_end = NA_character_, size_bytes = NA_real_,
      license_url = NA_character_,
      citation = paste("OpenTopography dataset", dataset, "- license not supplied in this index. Obtain the dataset license, DOI and required producer citation from its landing page. Citation guidance: https://opentopography.org/citations"),
      geometry = sf::st_geometry(obj))
    if (identical(dataset, "Auckland_2013")) {
      rows$license_url <- "https://creativecommons.org/licenses/by/3.0/nz/"
      rows$citation <- paste("Copyright in this work is owned by Auckland Council.",
        "Auckland, New Zealand 2013. Distributed by OpenTopography.",
        "https://doi.org/10.5069/G9KW5CZ5 . For derivative works use the producer's derivative attribution; see dataset metadata.")
    }
    if (identical(dataset, "BR17_SaoPaulo")) {
      rows$license_url <- "https://www.gnu.org/licenses/gpl-3.0.html"
      rows$citation <- paste("Sao Paulo City Hall (PMSP) (2024). Sao Paulo, Brazil Lidar Survey 2017. Distributed by OpenTopography.",
        "https://doi.org/10.5069/G9NV9GD1 .",
        "Data contributors: Municipality of Sao Paulo, SMDU and M3DC; public cloud hosting: AWS.",
        "Provider lists GNU GPLv3; preserve licence and applicable redistribution terms.",
        "https://portal.opentopography.org/datasetMetadata?otCollectionID=OT.062020.31983.1")
    }
    results[[length(results) + 1L]] <- rows
    if (sum(vapply(results, nrow, integer(1))) > max_items)
      stop("Search exceeds max_items; use a smaller AOI.", call. = FALSE)
  }
  if (!length(results)) return(empty_tiles())
  ans <- do.call(rbind, results)
  ans[!duplicated(ans$url), , drop = FALSE]
}

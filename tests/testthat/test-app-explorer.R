square_ring <- function(x0, y0, x1, y1)
  list(list(x0, y0), list(x1, y0), list(x1, y1), list(x0, y1), list(x0, y0))

test_that("a leaflet.extras draw feature converts to the drawn polygon, not a corrupted one", {
  feature <- list(type = "Feature", properties = list(),
    geometry = list(type = "Polygon", coordinates = list(square_ring(-10, 20, -9, 21))))
  x <- alsdownloader:::leaflet_draw_feature_to_sf(feature)
  expect_s3_class(x, "sf")
  expect_equal(nrow(x), 1L)
  bb <- sf::st_bbox(x)
  expect_equal(unname(bb[c("xmin", "ymin", "xmax", "ymax")]), c(-10, 20, -9, 21))
  expect_equal(sf::st_crs(x)$epsg, 4326)
})

test_that("editing multiple drawn features keeps each geometry intact", {
  collection <- list(type = "FeatureCollection", features = list(
    list(type = "Feature", geometry = list(type = "Polygon", coordinates = list(square_ring(0, 0, 1, 1)))),
    list(type = "Feature", geometry = list(type = "Polygon", coordinates = list(square_ring(5, 5, 6, 6))))))
  x <- alsdownloader:::leaflet_draw_collection_to_sf(collection)
  expect_equal(nrow(x), 2L)
  bbs <- lapply(sf::st_geometry(x), sf::st_bbox)
  expect_equal(unname(bbs[[1]][c("xmin", "ymin")]), c(0, 0))
  expect_equal(unname(bbs[[2]][c("xmin", "ymin")]), c(5, 5))
})

test_that("non-polygon draw shapes are rejected with a clear message", {
  expect_error(
    alsdownloader:::leaflet_draw_polygon(list(type = "Point", coordinates = list(0, 0))),
    "polygon or rectangle")
})

test_that("reset area of interest clears only the study area and results, not job/download settings", {
  app <- als_app()
  shiny::testServer(app, {
    session$flushReact()
    state$aoi <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(matrix(
      c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE))), crs = 4326))
    state$tiles <- alsdownloader:::empty_tiles()
    state$search <- "Searching all configured sources..."
    state$jobtext <- "Complete: 1 successful, 0 failed. See manifest.csv."
    session$flushReact()
    session$setInputs(reset_aoi = 1)
    session$flushReact()
    expect_null(state$aoi)
    expect_null(state$tiles)
    expect_match(state$search, "No search results yet")
    expect_match(state$jobtext, "Complete: 1 successful")
  })
})

test_that("reset (general) clears the study area, results and job text", {
  app <- als_app()
  shiny::testServer(app, {
    session$flushReact()
    state$aoi <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(matrix(
      c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE))), crs = 4326))
    state$tiles <- alsdownloader:::empty_tiles()
    state$search <- "Searching all configured sources..."
    state$jobtext <- "Complete: 1 successful, 0 failed. See manifest.csv."
    session$flushReact()
    session$setInputs(reset_all = 1)
    session$flushReact()
    expect_null(state$aoi)
    expect_null(state$tiles)
    expect_match(state$search, "No search results yet")
    expect_match(state$jobtext, "No active download")
  })
})

test_that("opening the map does not error the server-driven layout-class observer", {
  app <- als_app()
  shiny::testServer(app, {
    session$flushReact()
    expect_no_error(session$setInputs(enter_map = 1))
    session$flushReact()
  })
})

test_that("report and transfer panels follow available results and job lifecycle", {
  shiny::testServer(als_app(), {
    session$flushReact()
    expect_identical(output$report_ready, "no")
    expect_identical(output$download_visible, "no")
    state$tiles <- sf::st_sf(tile_id = "one", size_bytes = 1024,
      acquired_start = "2020-01-01", acquired_end = "2020-12-31", provider = "test",
      dataset = "Test", filename = "one.laz", url = "https://example.org/one.laz",
      license_url = "https://example.org/license", citation = "Test fixture",
      geometry = sf::st_as_sfc(sf::st_bbox(c(xmin=0,ymin=0,xmax=1,ymax=1),crs=4326)))
    session$flushReact()
    expect_identical(output$report_ready, "yes")
    state$tiles <- NULL
    session$flushReact()
    expect_identical(output$report_ready, "no")
    state$job <- list(is_alive = function() TRUE, kill_tree = function() NULL)
    state$finished <- FALSE
    session$flushReact()
    expect_identical(output$download_visible, "yes")
    expect_identical(output$download_running, "yes")
    session$setInputs(cancel = 1)
    session$flushReact()
    expect_identical(output$download_running, "no")
    expect_identical(output$download_visible, "yes")
    expect_match(output$job_status, "Canceled")
    state$job <- NULL
  })
})

test_that("redact_urls_in_text strips only the query string of embedded URLs, not the rest of the message", {
  msg <- alsdownloader:::redact_urls_in_text(
    "Client error: (403) Forbidden for url: https://example.org/tile.laz?sig=SECRET&se=2099-01-01 -- check provider access")
  expect_match(msg, "https://example.org/tile.laz", fixed = TRUE)
  expect_false(grepl("SECRET", msg, fixed = TRUE))
  expect_match(msg, "check provider access", fixed = TRUE)
})

test_that("search results are grouped and legended by acquisition year, including undated tiles", {
  square <- sf::st_sfc(sf::st_polygon(list(matrix(
    c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE))), crs = 4326)
  ring <- sf::st_sfc(rep(list(sf::st_polygon(list(matrix(
    c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE)))), 3), crs = 4326)
  fake_tiles <- function(aoi, provider, start, end, tile_index_dir) {
    if (!identical(provider, "usgs3dep")) return(alsdownloader:::empty_tiles())
    sf::st_sf(tile_id = paste0("t", 1:3), provider = "usgs3dep", dataset = "3DEP",
      filename = paste0("f", 1:3, ".laz"), url = paste0("https://example.org/f", 1:3, ".laz"),
      acquired_start = c("2018-01-01", "2020-06-01", NA), acquired_end = c("2018-03-01", "2020-08-01", NA),
      size_bytes = NA_real_, license_url = "https://example.org", citation = "Test fixture",
      geometry = ring)
  }
  # local_mocked_bindings() must be called directly inside test_that()'s own
  # frame: called from inside shiny::testServer()'s block instead, its
  # automatic unmock-on-exit never fires (that block is not a normal call
  # frame), leaking the mock into every later test in the same R process.
  local_mocked_bindings(find_tiles = fake_tiles, .package = "alsdownloader")
  app <- als_app()
  shiny::testServer(app, {
    session$flushReact()
    state$aoi <- sf::st_sf(geometry = square)
    session$setInputs(search = 1)
    session$flushReact()
    expect_equal(nrow(state$tiles), 3L)
    expect_setequal(state$tile_groups, c("Tiles: 2018", "Tiles: 2020", "Tiles: Unknown"))
  })
})

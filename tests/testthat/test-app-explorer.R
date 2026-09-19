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

test_that("resetting the explorer clears the study area, results and job text", {
  app <- als_app()
  shiny::testServer(app, {
    session$flushReact()
    state$aoi <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(matrix(
      c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE))), crs = 4326))
    state$tiles <- alsdownloader:::empty_tiles()
    state$search <- "Searching all configured sources..."
    state$jobtext <- "Complete: 1 successful, 0 failed. See manifest.csv."
    session$flushReact()
    session$setInputs(reset_explorer = 1)
    session$flushReact()
    expect_null(state$aoi)
    expect_null(state$tiles)
    expect_match(state$search, "Draw or upload a study area")
    expect_match(state$jobtext, "No active download")
  })
})

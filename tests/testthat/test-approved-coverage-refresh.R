test_that("only approved indexes change the coverage signature", {
  queue <- tempfile(); dir.create(queue)
  on.exit(unlink(queue, recursive=TRUE))
  approved <- file.path(queue, "approved")
  expect_identical(approved_coverage_signature(approved), character())
  dir.create(file.path(queue, "requests"))
  writeLines("pending", file.path(queue, "requests", "proposal.json"))
  expect_identical(approved_coverage_signature(approved), character())
  dir.create(approved)
  path <- file.path(approved, "example.tiles.geojson")
  writeLines("{}", path)
  first <- approved_coverage_signature(approved)
  expect_length(first, 1L)
  writeLines("{\"changed\":true}", path)
  expect_false(identical(first, approved_coverage_signature(approved)))
  unlink(path)
  expect_identical(approved_coverage_signature(approved), character())
})

test_that("file selection maps author polygons without overriding existing mappings", {
  g <- sf::st_sf(geometry=sf::st_as_sfc(sf::st_bbox(
    c(xmin=5,ymin=52,xmax=5.01,ymax=52.01),crs=4326)))
  mapped <- zenodo_map_boundary(g, "cloud.zip")
  expect_identical(mapped$file_key, "cloud.zip")
  expect_equal(sf::st_bbox(mapped), sf::st_bbox(g), tolerance=1e-9)
  expect_equal(as.numeric(sf::st_area(mapped)), as.numeric(sf::st_area(g)), tolerance=1e-7)
  expect_error(zenodo_map_boundary(mapped, "other.zip"), "conflicts")
})

test_that("coverage updates do not issue view or AOI changes", {
  g <- sf::st_sf(dataset="Approved source", geometry=sf::st_as_sfc(sf::st_bbox(
    c(xmin=5,ymin=52,xmax=5.01,ymax=52.01),crs=4326)))
  map <- add_in_app_coverage(leaflet::leaflet(), g)
  expect_identical(vapply(map$x$calls, `[[`, "", "method"), "addPolygons")
})

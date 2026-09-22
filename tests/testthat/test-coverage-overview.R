test_that("overview retains separate indexed regions instead of a bounding rectangle", {
  folder<-tempfile();dir.create(folder);on.exit(unlink(folder,recursive=TRUE))
  box<-function(x)sf::st_as_sfc(sf::st_bbox(c(xmin=x,ymin=0,xmax=x+.01,ymax=.01),crs=4326))
  g<-c(box(0),box(.03))
  sf::st_write(sf::st_sf(geometry=g),file.path(folder,"regions.gpkg"),quiet=TRUE)
  x<-coverage_overview(folder)
  expect_equal(nrow(x),1L)
  gap<-sf::st_sfc(sf::st_point(c(.02,.005)),crs=4326)
  expect_length(sf::st_intersects(gap,x)[[1]],0L)
  expect_length(sf::st_intersects(sf::st_sfc(sf::st_point(c(.005,.005)),crs=4326),x)[[1]],1L)
  expect_equal(nrow(coverage_overview()),0L)
})

test_that("bundled discovery masks retain regional geometry and provenance", {
  x <- discovery_coverage()
  expect_true(all(c("usgs3dep", "canelevation", "ahn6", "ignfr", "swisstopo") %in% x$provider))
  expect_true(all(sf::st_geometry_type(x) %in% c("POLYGON", "MULTIPOLYGON")))
  expect_false(any(sf::st_is_empty(x)))
  expect_true(all(sf::st_is_valid(x)))
  expect_true(all(nzchar(x$citation) & nzchar(x$info_url) & nzchar(x$license_url)))
  ca <- x[x$provider == "canelevation", ]
  # Northern Canada must not be painted as nationwide coverage.
  remote <- sf::st_sfc(sf::st_point(c(-100, 80)), crs = 4326)
  expect_length(sf::st_intersects(remote, ca)[[1]], 0L)
  expect_gt(nrow(ca), 100L)
  expect_equal(discovery_groups(), c("Countries", "In-App Access", "External Access", "AOI"))
})

canelevation_fixture <- function(url) {
  poly <- sf::st_sfc(sf::st_polygon(list(matrix(c(-113.5,54.5, -113.4,54.5, -113.4,54.6, -113.5,54.6, -113.5,54.5), ncol=2, byrow=TRUE))), crs=4326)
  sf::st_sf(url=url, geometry=poly)
}

test_that("CanElevation local index selects public-bucket assets within the AOI", {
  folder <- tempfile(); dir.create(folder); on.exit(unlink(folder, recursive=TRUE))
  x <- canelevation_fixture("https://canelevation-lidar-point-clouds.s3.ca-central-1.amazonaws.com/pointclouds_nuagespoints/AB/Athabasca_2018/pc_083I11NE41NE_20181006.copc.laz")
  sf::st_write(x, file.path(folder, "Athabasca_2018.gpkg"), quiet=TRUE)
  aoi <- sf::st_as_sfc(sf::st_bbox(c(xmin=-113.5,ymin=54.5,xmax=-113.4,ymax=54.6),crs=4326))
  found <- find_tiles(aoi, "canelevation", tile_index_dir=folder)
  expect_equal(nrow(found), 1L)
  expect_equal(found$provider, "canelevation")
  expect_equal(found$dataset, "Athabasca_2018")
  expect_match(found$url, "canelevation-lidar-point-clouds.s3.ca-central-1.amazonaws.com", fixed=TRUE)
  expect_match(found$license_url, "open.canada.ca", fixed=TRUE)
  expect_match(found$citation, "Natural Resources Canada", fixed=TRUE)

  away <- sf::st_as_sfc(sf::st_bbox(c(xmin=0,ymin=0,xmax=1,ymax=1),crs=4326))
  expect_equal(nrow(find_tiles(away, "canelevation", tile_index_dir=folder)), 0L)
})

test_that("CanElevation rejects assets outside the confirmed public bucket", {
  folder <- tempfile(); dir.create(folder); on.exit(unlink(folder, recursive=TRUE))
  x <- canelevation_fixture("https://evil.example/pc_083I11NE41NE_20181006.copc.laz")
  sf::st_write(x, file.path(folder, "Athabasca_2018.gpkg"), quiet=TRUE)
  aoi <- sf::st_as_sfc(sf::st_bbox(c(xmin=-113.5,ymin=54.5,xmax=-113.4,ymax=54.6),crs=4326))
  expect_error(find_tiles(aoi, "canelevation", tile_index_dir=folder), "Unexpected CanElevation")
})

test_that("CanElevation requires a configured directory and a URL field", {
  expect_error(search_canelevation(sf::st_as_sfc(sf::st_bbox(c(xmin=0,ymin=0,xmax=1,ymax=1),crs=4326)), NULL, 10L), "Configure a local")
  folder <- tempfile(); dir.create(folder); on.exit(unlink(folder, recursive=TRUE))
  poly <- sf::st_sfc(sf::st_polygon(list(matrix(c(-113.5,54.5, -113.4,54.5, -113.4,54.6, -113.5,54.6, -113.5,54.5), ncol=2, byrow=TRUE))), crs=4326)
  sf::st_write(sf::st_sf(name="x", geometry=poly), file.path(folder, "no_url.gpkg"), quiet=TRUE)
  aoi <- sf::st_as_sfc(sf::st_bbox(c(xmin=-113.5,ymin=54.5,xmax=-113.4,ymax=54.6),crs=4326))
  expect_error(find_tiles(aoi, "canelevation", tile_index_dir=folder), "URL field")
})

index_fixture <- function(change = identity) {
  source <- system.file("extdata", "contribution-template.geojson", package = "alsdownloader")
  x <- jsonlite::fromJSON(source, simplifyVector = FALSE)
  path <- tempfile(fileext = ".tiles.geojson")
  jsonlite::write_json(change(x), path, auto_unbox = TRUE, null = "null")
  path
}

test_that("approved indexes support AOI selection and unknown dates", {
  path <- index_fixture(); on.exit(unlink(path))
  x <- read_tile_index(path)
  expect_equal(nrow(x), 1L)
  expect_true(is.na(x$acquired_end))
  folder <- tempfile(); dir.create(folder); on.exit(unlink(folder, recursive=TRUE), add=TRUE)
  file.copy(path, file.path(folder, "survey.tiles.geojson"))
  found <- find_tiles(sf::st_geometry(x), "contributed", start="2025-01-01", tile_index_dir=folder)
  expect_equal(found$tile_id, x$tile_id)
  away <- sf::st_as_sfc(sf::st_bbox(c(xmin=0,ymin=0,xmax=1,ymax=1),crs=4326))
  expect_equal(nrow(find_tiles(away,"contributed",tile_index_dir=folder)),0L)
  script <- parse(text=selection_script(found))
  env <- new.env(); eval(script[[1]], env)
  expect_equal(env$tiles$url, found$url)
  dest <- tempfile(); dir.create(dest); on.exit(unlink(dest,recursive=TRUE),add=TRUE)
  local_mocked_bindings(fetch_asset=function(url,part,timeout) {
    payload <- raw(227); payload[1:4] <- charToRaw("LASF"); writeBin(payload,part)
    structure(list(status_code=200L,headers=list(`content-length`="227")),class="response")
  },.package="alsdownloader")
  expect_equal(download_tiles(found,dest,retries=0)$status,"downloaded")
  expect_equal(download_tiles(found,dest,retries=0)$status,"verified_existing")
  expect_equal(utils::read.csv(file.path(dest,"selected-tiles.csv"))$dataset,found$dataset)
})

test_that("invalid or incompatible contribution metadata is rejected", {
  changes <- list(
    function(x) {x$features[[1]]$properties$platform <- "TLS"; x},
    function(x) {x$features[[1]]$properties$acquired_start <- "2024-01-01"; x$features[[1]]$properties$acquired_end <- "2018-01-01"; x},
    function(x) {x$features[[1]]$properties$url <- "https://example.org/a.laz?token=secret"; x},
    function(x) {x$features <- c(x$features,x$features); x})
  for (change in changes) {
    path <- index_fixture(change)
    expect_error(read_tile_index(path))
    unlink(path)
  }
})

test_that("index preflight reads only bounded metadata, not tile assets", {
  path <- index_fixture(); on.exit(unlink(path))
  dir <- tempfile(); dir.create(dir); on.exit(unlink(dir,recursive=TRUE),add=TRUE)
  local_mocked_bindings(public_sample_target=function(...)list(host="example.org",ip="8.8.8.8"),.package="alsdownloader")
  response <- function(size) structure(list(status_code=200L,headers=list(`content-length`=as.character(size))),class="response")
  calls <- character()
  get <- function(url, dest, config, handle) {calls <<- c(calls,url); file.copy(path,dest); response(file.size(path))}
  checked <- source_preflight("https://example.org/index.geojson",dir,function(...)response(file.size(path)),get)
  expect_match(checked$summary,"1 tile footprints")
  expect_equal(calls,"https://example.org/index.geojson")
  expect_error(source_preflight("https://example.org/index.geojson",dir,function(...)response(6*1024^2),get),"5 MiB")
  expect_length(calls,1L)
  expect_false(file.exists(file.path(dir,"index.geojson")))
})

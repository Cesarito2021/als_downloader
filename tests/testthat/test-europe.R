test_that("embedded spatial query parameters are preserved", {
  args <- NULL
  local_mocked_bindings(GET=function(url,...) {
    args <<- list(...)
    structure(list(url=url,status_code=200L,headers=list(`Content-Type`="application/json"),content=charToRaw('{"ok":true}')),class="response")
  },.package="httr")
  expect_true(request_json("https://example.org/items?bbox=1,2,3,4")$ok)
  expect_false("query" %in% names(args))
})

test_that("European adapters select cloud assets and filter exact footprints", {
  aoi <- sf::st_as_sfc(sf::st_bbox(c(xmin=6,ymin=53,xmax=6.1,ymax=53.1),crs=4326))
  geometry <- list(type="Polygon",coordinates=list(list(c(6,53),c(6.1,53),c(6.1,53.1),c(6,53.1),c(6,53))))
  f <- list(id="tile",geometry=geometry,properties=list(Puntenwolk="https://basisdata.nl/hwh-ahn/AHN6/a.laz",datetime="2025-01-01"),assets=list(data=list(href="https://data.geo.admin.ch/ch.swisstopo.swisssurface3d/a.las.zip")))
  local_mocked_bindings(native_pages=function(...)list(f),.package="alsdownloader")
  x <- search_europe(aoi,"ahn6",10)
  expect_equal(nrow(x),1L);expect_match(x$url,"a.laz",fixed=TRUE)
  expect_true(is.na(x$acquired_end))
  y <- search_europe(aoi,"swisstopo",10)
  expect_match(y$url,"a.las.zip",fixed=TRUE)
  away <- sf::st_as_sfc(sf::st_bbox(c(xmin=0,ymin=0,xmax=1,ymax=1),crs=4326))
  expect_equal(nrow(search_europe(away,"ahn6",10)),0L)
})

test_that("pagination cannot silently cross hosts or exceed limits", {
  local_mocked_bindings(request_json=function(...)list(type="FeatureCollection",features=list(list(id="a")),links=list(list(rel="next",href="https://other.example/items"))),.package="alsdownloader")
  expect_error(native_pages("https://example.org/items","https://example.org/",10),"Unexpected")
  expect_error(native_pages("https://example.org/items","https://example.org/",0),"max_items")
})

test_that("Swiss originals are retained as ZIP without extraction", {
  folder <- tempfile();dir.create(folder);on.exit(unlink(folder,recursive=TRUE))
  las <- file.path(folder,"tile.las");payload<-raw(227);payload[1:4]<-charToRaw("LASF");writeBin(payload,las)
  archive <- file.path(folder,"tile.zip");zip::zipr(archive,las)
  tile <- list(provider="swisstopo",filename="tile.las.zip")
  expect_true(valid_tile_container(archive,tile))
  expect_false(valid_tile_container(las,tile))
  tile$provider <- "ahn6";expect_false(valid_tile_container(archive,tile))
})

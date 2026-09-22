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
  local_mocked_bindings(native_pages=function(...)list(f),.package="ALSdownloadeR")
  x <- search_europe(aoi,"ahn6",10)
  expect_equal(nrow(x),1L);expect_match(x$url,"a.laz",fixed=TRUE)
  expect_true(is.na(x$acquired_end))
  y <- search_europe(aoi,"swisstopo",10)
  expect_match(y$url,"a.las.zip",fixed=TRUE)
  away <- sf::st_as_sfc(sf::st_bbox(c(xmin=0,ymin=0,xmax=1,ymax=1),crs=4326))
  expect_equal(nrow(search_europe(away,"ahn6",10)),0L)
})

test_that("France LiDAR HD adapter selects COPC assets, host and reported dates", {
  aoi <- sf::st_as_sfc(sf::st_bbox(c(xmin=5.7,ymin=45.1,xmax=5.73,ymax=45.12),crs=4326))
  geometry <- list(type="Polygon",coordinates=list(list(c(5.7,45.1),c(5.73,45.1),c(5.73,45.12),c(5.7,45.12),c(5.7,45.1))))
  f <- list(id="LHD_FXX_0913_6450_PTS_LAMB93_IGN69_PM",geometry=geometry,
    properties=list(start_datetime="2021-08-06T00:00:00Z",end_datetime="2021-08-06T23:59:59Z",`lidarhd:date_edition`="2023-04-20"),
    assets=list(data=list(href="https://data.geopf.fr/telechargement/download/LiDARHD-NUALID/NUALHD_1-0__LAZ_LAMB93_PM_2023-04-20/LHD_FXX_0913_6450_PTS_LAMB93_IGN69.copc.laz")))
  local_mocked_bindings(native_pages=function(...)list(f),.package="ALSdownloadeR")
  x <- search_europe(aoi,"ignfr",10)
  expect_equal(nrow(x),1L)
  expect_match(x$url,"data.geopf.fr/telechargement/download/",fixed=TRUE)
  expect_equal(x$dataset,"IGN LiDAR HD")
  expect_equal(x$acquired_start,"2021-08-06");expect_equal(x$acquired_end,"2021-08-06")
  expect_match(x$license_url,"etalab-2.0",fixed=TRUE)
  expect_match(x$citation,"INRAE",fixed=TRUE)
  expect_match(x$citation,"IGN product edition: 2023-04-20",fixed=TRUE)
  expect_match(x$citation,x$url,fixed=TRUE)

  f$properties[["lidarhd:code_mission"]] <- "21LHD6PM"
  f$properties$start_datetime <- "2019-03-20T00:00:00Z"
  f$properties[["lidarhd:date_debut_acquisition"]] <- "2021-08-06"
  f$properties[["lidarhd:date_fin_acquisition"]] <- "2021-08-06"
  mission <- search_europe(aoi,"ignfr",10)
  expect_equal(mission$campaign_id, "21LHD6PM")
  expect_equal(mission$acquired_start, "2021-08-06")
  expect_equal(campaign_tile_rows(mission, "2019"), integer())
  expect_equal(campaign_tile_rows(mission, "2021", "ignfr / 21LHD6PM"), 1L)

  bad <- f; bad$assets$data$href <- "https://evil.example/x.copc.laz"
  local_mocked_bindings(native_pages=function(...)list(bad),.package="ALSdownloadeR")
  expect_error(search_europe(aoi,"ignfr",10),"Unexpected France")
})

test_that("IGN edition credits do not substitute acquisition or catalogue dates", {
  href <- "https://data.geopf.fr/telechargement/download/LiDARHD-NUALID/NUALHD_1-0__LAZ_LAMB93_NE_2026-04-30/tile.copc.laz"
  p <- list(`lidarhd:date_edition` = "2026-04-30", created = "2026-06-30T00:00:00Z",
    start_datetime = "2025-10-18T00:00:00Z")
  expect_match(ign_edition_credit(p, href), "2026-04-30", fixed = TRUE)
  p[["lidarhd:date_edition"]] <- NULL
  expect_error(ign_edition_credit(p, href), "unavailable")
  for (bad in c("2026-02-30", "2026-4-30", "unknown")) {
    p[["lidarhd:date_edition"]] <- bad
    expect_error(ign_edition_credit(p, href), "invalid")
  }
  p[["lidarhd:date_edition"]] <- "2026-04-29"
  expect_error(ign_edition_credit(p, href), "does not match")
})

test_that("pagination cannot silently cross hosts or exceed limits", {
  local_mocked_bindings(request_json=function(...)list(type="FeatureCollection",features=list(list(id="a")),links=list(list(rel="next",href="https://other.example/items"))),.package="ALSdownloadeR")
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

campaign_fixture <- function() data.frame(
  provider=c(rep("usgs3dep", 6), "other"), dataset="collection",
  campaign_id=c("GA_west", "GA_west", "GA_east", "GA_old", "cross_year", NA, "GA_west"),
  acquired_start=c(rep("2019-01-01", 3), "2018-01-01", "2018-11-01", NA, "2019-01-01"),
  acquired_end=c(rep("2019-12-31", 3), "2018-12-31", "2019-02-01", NA, "2019-12-31"))

test_that("year selection includes every campaign and overlapping acquisition interval", {
  x <- campaign_fixture()
  expect_equal(campaign_tile_rows(x, "2019", "all"), c(1L,2L,3L,5L,7L))
  expect_equal(campaign_tile_rows(x, "2018", "all"), c(4L,5L))
  expect_equal(campaign_tile_rows(x, "unknown", "all"), 6L)
  expect_equal(campaign_tile_rows(x), seq_len(7))
})

test_that("one or multiple campaigns select original rows without crossing provider or year", {
  x <- campaign_fixture()
  expect_equal(campaign_tile_rows(x, "2019", "usgs3dep / GA_west"), 1:2)
  expect_equal(campaign_tile_rows(x, "2019", c("usgs3dep / GA_west", "usgs3dep / GA_east")), 1:3)
  expect_equal(campaign_tile_rows(x, "2018", "usgs3dep / GA_west"), integer())
  expect_equal(campaign_tile_rows(x, "2019", character()), integer())
  expect_equal(campaign_tile_rows(x, "2019", "stale campaign"), integer())
  expect_equal(campaign_tile_rows(NULL), integer())
  expect_length(tile_year_membership(NULL), 0)
})

test_that("unrecognized filenames do not invent campaign identifiers", {
  x <- campaign_fixture()
  x$campaign_id <- NULL
  x$filename <- "GA_2019_campaign1.laz"
  expect_true(all(grepl("campaign not supplied", names(selection_campaign_groups(x)))))
  expect_equal(campaign_tile_rows(x, "unknown"), 6L)
})

test_that("recognized directories group projects and blocks without tile or edition IDs", {
  x <- campaign_fixture()[rep(1, 5), ]
  x$campaign_id <- NA_character_
  x$provider <- c("ignfr", "ignfr", "usgs3dep", "usgs3dep", "opentopography")
  x$dataset <- c(rep("collection", 4), "BR17_SaoPaulo")
  x$url <- c(
    "https://data.geopf.fr/telechargement/download/LiDARHD-NUALID/NUALHD_1-0__LAZ_LAMB93_PM_2023-04-20/a.copc.laz",
    "https://data.geopf.fr/telechargement/download/LiDARHD-NUALID/NUALHD_1-0__LAZ_LAMB93_PM_2025-03-25/b.copc.laz",
    "https://usgslidareuwest.blob.core.windows.net/usgs-3dep-copc/usgs-copc/FL_2019/copc/tile1.copc.laz",
    "https://usgslidareuwest.blob.core.windows.net/usgs-3dep-copc/usgs-copc/GA_2019/copc/tile2.copc.laz",
    "https://example.org/tile.laz")
  z <- tile_campaign_metadata(x)
  expect_equal(z$campaign_group, c("Block PM", "Block PM", "FL_2019", "GA_2019", "BR17_SaoPaulo"))
  expect_true(all(is.na(z$campaign_id)))
  expect_equal(campaign_tile_rows(z, "2019", "ignfr / Block PM (delivery block)"), 1:2)
  expect_equal(campaign_tile_rows(z, "2019", "usgs3dep / FL_2019 (project directory)"), 3L)
  x$campaign_id[1] <- "21LHD6PM"
  expect_equal(tile_campaign_metadata(x)$campaign_group[1], "21LHD6PM")
  x$url[3] <- "https://example.org/usgs-3dep-copc/usgs-copc/fake/copc/tile.laz"
  expect_true(is.na(tile_campaign_metadata(x)$campaign_group[3]))
  expect_equal(nrow(tile_campaign_metadata(empty_tiles())), 0L)
})

test_that("3DEP retains the official project identifier independently of collection and tile names", {
  polygon <- list(type="Polygon", coordinates=list(list(c(0,0),c(1,0),c(1,1),c(0,1),c(0,0))))
  feature <- function(id, project) list(id=id, collection="3dep-lidar-copc", geometry=polygon,
    properties=list(`3dep:usgs_id`=project, start_datetime="2019-01-01", end_datetime="2019-12-31"),
    assets=list(data=list(href=paste0("https://example.org/",id,".laz"))))
  local_mocked_bindings(request_json=function(...) list(features=list(
    feature("tile1", "GA_campaign_A"), feature("tile2", "GA_campaign_B"), feature("tile3", NULL)), links=list()),
    .package="ALSdownloadeR")
  aoi <- sf::st_sf(geometry=sf::st_sfc(sf::st_polygon(list(matrix(c(0,0,1,0,1,1,0,1,0,0),ncol=2,byrow=TRUE))),crs=4326))
  local_mocked_bindings(get_als_file_sizes=function(tiles,...)tiles,.package="ALSdownloadeR")
  x <- find_tiles(aoi,"planetary")
  expect_equal(x$campaign_id, c("GA_campaign_A", "GA_campaign_B", NA_character_))
  expect_equal(x$dataset, rep("3dep-lidar-copc", 3))
  expect_equal(campaign_tile_rows(x,"2019"),1:3)
})

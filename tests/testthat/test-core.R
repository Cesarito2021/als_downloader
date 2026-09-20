square <- function(x = 0, y = 0) sf::st_sfc(sf::st_polygon(list(matrix(
  c(x,y, x+1,y, x+1,y+1, x,y+1, x,y), ncol = 2, byrow = TRUE))), crs = 4326)

test_that("area is global, geodesic, and does not double count", {
  a <- read_aoi(square())
  expect_equal(sf::st_crs(a)$epsg, 4326)
  expect_gt(aoi_area(a), 12000)
  expect_lt(aoi_area(a), 13000)
  expect_equal(aoi_area(rbind(a, a)), aoi_area(a), tolerance = 1e-6)
  expect_gt(aoi_area(square(150, -35)), 9000)
  expect_lt(aoi_area(square(0, 70)), aoi_area(a))
  expect_error(read_aoi(sf::st_set_crs(a, NA)), "no CRS")
  expect_error(read_aoi(sf::st_sfc(sf::st_point(c(0,0)),crs=4326)), "polygons")
})

test_that("upload filenames and multilayer selection are honored", {
  path <- tempfile() # Shiny's extensionless temporary path
  sf::st_write(read_aoi(square()), path, driver="GeoJSON", quiet=TRUE)
  on.exit(unlink(path))
  expect_s3_class(read_aoi(list(name="study.geojson",datapath=path)), "sf")
  expect_error(read_aoi(path), "Use a zipped")
  gpkg <- tempfile(fileext=".gpkg"); on.exit(unlink(gpkg),add=TRUE)
  sf::st_write(read_aoi(square()),gpkg,layer="first",quiet=TRUE)
  sf::st_write(read_aoi(square(10,10)),gpkg,layer="second",quiet=TRUE)
  expect_error(read_aoi(gpkg), "Select a GeoPackage layer")
  expect_equal(nrow(read_aoi(gpkg,layer="second")),1L)
})

test_that("worker policy honors deployment, machine and provider", {
  expect_equal(download_worker_policy("hosted",32,20)$effective,1)
  expect_equal(download_worker_policy("local",16)$recommended,10)
  expect_equal(download_worker_policy("local",8)$effective,4)
  expect_equal(download_worker_policy("local",2)$effective,1)
  expect_equal(download_worker_policy("local",NA_integer_)$effective,1)
  expect_equal(download_worker_policy("local",32,18)$effective,18)
  expect_equal(download_worker_policy("local",32,18,provider_limit=2)$effective,2)
  expect_equal(download_worker_policy("local",32,jobs=0)$effective,0)
  for (bad in list(0,-1,NA_real_,Inf,1.5,"4",numeric()))
    expect_error(download_worker_policy("local",16,requested=bad))
})

test_that("preview is bounded, reproducible and preserves source", {
  points <- data.frame(X=1:100,Y=101:200,Z=201:300)
  p <- preview_points(points,10)
  expect_equal(nrow(p),10L)
  expect_equal(p,preview_points(points,10))
  expect_equal(points$X,1:100)
  expect_equal(attr(p,"origin"),c(X=1,Y=101,Z=201))
  expect_equal(min(p$Z),0)
  expect_error(preview_points(points,0),"budget")
  expect_error(preview_points(data.frame(X=NA_real_,Y=1,Z=1)),"finite")
})

test_that("STAC receives a Geometry and pagination failures are explicit", {
  expect_equal(alsdownloader:::aoi_geometry(square())$type,"Polygon")
  local_mocked_bindings(request_json=function(url,body=NULL,query=NULL)
    list(features=list(),links=list(list(rel="next",href=url,method="POST",body=body))),
    .package="alsdownloader")
  expect_error(find_tiles(square()),"pagination repeated")
})

test_that("invalid provider acquisition intervals stay unknown", {
  expect_true(all(is.na(stac_acquisition_period(list(start_datetime="2020-01-01",end_datetime="2019-12-31")))))
  expect_true(all(is.na(stac_acquisition_period(list(start_datetime="2020-02-30",end_datetime="unknown")))))
  expect_equal(stac_acquisition_period(list(start_datetime="2020-02-29")),c(start="2020-02-29",end=NA_character_))
})

test_that("candidate providers are not advertised as implemented", {
  catalog <- provider_catalog()
  expect_setequal(catalog$id[catalog$implemented],c("usgs3dep","opentopography","ahn6","swisstopo","ignfr","canelevation"))
  expect_true(all(grepl("^https://",catalog$info_url)))
})

test_that("OpenTopography accepts polygon indexes with measured coordinates", {
  root <- tempfile(); dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  source <- file.path(root, "source"); dir.create(source)
  coordinates <- cbind(matrix(c(0,0,1,0,1,1,0,1,0,0), ncol=2, byrow=TRUE), M=0)
  geometry <- sf::st_sfc(sf::st_polygon(list(coordinates), dim="XYM"), crs=4326)
  obj <- sf::st_sf(url="https://example.org/sample.laz", geometry=geometry)
  path <- file.path(source,"index.shp")
  sf::st_write(obj,path,quiet=TRUE)
  zip::zipr(file.path(root,"sample_TileIndex.zip"),list.files(source,full.names=TRUE))
  tiles <- find_tiles(square(),"opentopography",tile_index_dir=root)
  expect_equal(nrow(tiles),1L)
  expect_equal(tiles$filename,"sample.laz")
  expect_true(is.na(tiles$license_url))
  expect_equal(sf::st_crs(tiles)$epsg,4326)
})


test_that("collection dates do not fall back to publication or nominal dates", {
  p <- list(start_datetime="2018-10-24T00:00:00Z", end_datetime="2018-11-04T00:00:00Z",
    datetime="2020-01-01T00:00:00Z", created="2021-01-01", published="2022-01-01")
  expect_equal(stac_acquisition_period(p), c(start="2018-10-24",end="2018-11-04"))
  p$end_datetime <- NULL
  expect_true(is.na(stac_acquisition_period(p)["end"]))
  p$start_datetime <- NULL
  expect_true(all(is.na(stac_acquisition_period(p))))
  p$start_datetime <- p$end_datetime <- "2019-07-01T00:00:00Z"
  expect_equal(unname(stac_acquisition_period(p)), rep("2019-07-01",2))
})

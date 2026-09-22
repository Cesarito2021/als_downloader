test_that("USGS discovery uses original assets and explicit pagination", {
  roi<-sf::st_as_sfc(sf::st_bbox(c(xmin=-119,ymin=34,xmax=-118,ymax=35),crs=4326))
  item<-list(sourceId="id",title="Original cloud",sizeInBytes=123,
    downloadURL="https://rockyweb.usgs.gov/Projects/a/LAZ/t.laz",metaUrl="https://www.sciencebase.gov/catalog/item/id",
    vendorMetaUrl="https://example.org/t.xml",boundingBox=list(minX=-118.9,minY=34.1,maxX=-118.8,maxY=34.2))
  calls<-0L
  local_mocked_bindings(request_json=function(url,body=NULL,query=NULL) {
    expect_match(url,"tnmaccess",fixed=TRUE);calls<<-calls+1L
    if(calls==1L) list(total=2,items=list(item)) else list(total=2,items=list())
  },.package="ALSdownloadeR")
  expect_error(find_als_usgs(roi,10),"ended early")
  local_mocked_bindings(request_json=function(...)list(total=1,items=list(item)),.package="ALSdownloadeR")
  x<-find_als_usgs(roi,10)
  expect_equal(x$url,item$downloadURL);expect_equal(x$size_bytes,123)
  expect_equal(asset_access_url(x),item$downloadURL)
})

test_that("acquisition XML distinguishes ground dates from publication", {
  xml<-function(current,dates) paste0('<metadata><idinfo><timeperd><current>',current,
    '</current><timeinfo>',dates,'</timeinfo></timeperd></idinfo></metadata>')
  range<-'<rngdates><begdate>20180527</begdate><enddate>20181012</enddate></rngdates>'
  x<-extract_als_dates_usgs(xml("ground condition",range))
  expect_equal(x$year,2018);expect_equal(x$end,"2018-10-12")
  expect_null(extract_als_dates_usgs(xml("publication date",range)))
  days<-'<mdattim><sngdate><caldate>20220326</caldate></sngdate><sngdate><caldate>20220330</caldate></sngdate></mdattim>'
  expect_equal(extract_als_dates_usgs(xml("ground condition",days))$end,"2022-03-30")
  expect_null(extract_als_dates_usgs(xml("ground condition",gsub("20181012","20180230",range))))
  y<-extract_als_dates_usgs(xml("ground condition",'<rngdates><begdate>2018</begdate><enddate>2019</enddate></rngdates>'))
  expect_equal(y$year,2019);expect_true(is.na(y$end))
})

test_that("partial HTTP replies never mistake one byte for the file", {
  headers<-function(x) charToRaw(paste0("HTTP/1.1 206 Partial Content\r\n",x,"\r\n\r\n"))
  expect_equal(http_asset_size(206,headers("Content-Length: 1\r\nContent-Range: bytes 0-0/123456"),TRUE),123456)
  expect_true(is.na(http_asset_size(200,headers("Content-Length: 1"),TRUE)))
  expect_true(is.na(http_asset_size(206,headers("Content-Range: bytes 0-0/*"),TRUE)))
  expect_true(is.na(http_asset_size(200,headers("Content-Length: 200\r\nContent-Type: text/html"))))
  expect_true(is.na(http_asset_size(200,headers("Content-Length: 200\r\nContent-Encoding: gzip"))))
})

test_that("totals exclude duplicate accesses and remain incomplete with NA", {
  x<-data.frame(url=c("https://example.org/a?token=1","https://example.org/a?token=2","https://example.org/b"),size_bytes=c(100,100,NA))
  s<-summarize_als_download(x)
  expect_equal(s$files,2L);expect_equal(s$known_bytes,100)
  expect_equal(s$missing_sizes,1L);expect_true(is.na(s$total_bytes));expect_false(s$complete)
  expect_match(report_date_label(c("2020-01-01",NA),c("2020-12-31",NA)),"incomplete")
})

test_that("asset identities preserve resource queries but ignore access tokens", {
  expect_equal(asset_identity_url("https://example.org/t?id=1&token=a"),
    asset_identity_url("https://example.org/t?token=b&id=1"))
  expect_false(identical(asset_identity_url("https://example.org/t?id=1"),
    asset_identity_url("https://example.org/t?id=2")))
})

test_that("harmonized year selection uses the final acquisition year", {
  x<-data.frame(acquisition_year=c(2019L,NA_integer_),acquired_start=c("2018-01-01",NA),acquired_end=c("2019-02-01",NA))
  expect_equal(tile_year_membership(x),list("2019","unknown"))
})

test_that("AHN flight strips supply their own documented dates", {
  g<-sf::st_as_sfc(sf::st_bbox(c(xmin=100000,ymin=400000,xmax=100100,ymax=400100),crs=28992))
  index<-sf::st_sf(datum_str=c("2025-03-07","2025-03-09"),geometry=c(g,g))
  path<-tempfile(fileext=".gpkg");on.exit(unlink(path),add=TRUE)
  sf::st_write(index,path,quiet=TRUE)
  old<-options(ALSdownloadeR.ahn_date_index=path);on.exit(options(old),add=TRUE)
  tiles<-als_metadata_columns(sf::st_sf(provider="ahn6",filename="AHN6_2025_tile.laz",
    acquired_start=NA_character_,acquired_end=NA_character_,geometry=g))
  x<-extract_als_dates_ahn(tiles)
  expect_equal(x$acquired_end,"2025-03-09");expect_equal(x$acquisition_year,2025L)
  expect_equal(x$date_scope,"intersecting_flight_strips")
})

test_that("Swiss metadata must match the tile version", {
  g<-sf::st_as_sfc(sf::st_bbox(c(xmin=2494000,ymin=1140000,xmax=2495000,ymax=1141000),crs=2056))
  x<-als_metadata_columns(sf::st_sf(provider="swisstopo",filename="swisssurface3d_2015_2494-1140.las.zip",geometry=g))
  local_mocked_bindings(request_json=function(...)list(results=list(list(id="2494_1140",attributes=list(gpstime_min=2023,gpstime_max=2023)))),.package="ALSdownloadeR")
  expect_true(is.na(extract_als_dates_swisstopo(x)$acquisition_year))
  local_mocked_bindings(request_json=function(...)list(results=list(list(id="2494_1140",attributes=list(gpstime_min=2015,gpstime_max=2015)))),.package="ALSdownloadeR")
  y<-extract_als_dates_swisstopo(x)
  expect_equal(y$acquisition_year,2015L);expect_equal(y$date_precision,"year")
})

test_that("only the observed AHN sentinel allows exhausted pages with next links", {
  prefix<-"https://api.ellipsis-drive.com/test/"
  page<-list(type="FeatureCollection",features=list(),numberReturned=0,numberMatched=999999999,
    links=list(list(rel="self",href=paste0(prefix,"items")),list(rel="next",href=paste0(prefix,"items"))))
  local_mocked_bindings(request_json=function(...)page,.package="ALSdownloadeR")
  expect_length(native_pages(paste0(prefix,"items"),prefix,10),0)
  page$links[[2]]$href <- paste0(prefix,"items?limit=100")
  expect_length(native_pages(paste0(prefix,"items?offset=last"),prefix,10),0)
  expect_error(native_pages("https://example.org/items","https://example.org/",10),"Invalid index pagination")
})

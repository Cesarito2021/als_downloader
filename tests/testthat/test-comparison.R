box_cloud <- function(x=0,y=0,width=1000,height=1000) sf::st_sf(geometry=sf::st_sfc(sf::st_polygon(list(matrix(c(x,y,x+width,y,x+width,y+height,x,y+height,x,y),ncol=2,byrow=TRUE))),crs=32631))

test_that("one source campaign can be compared with a user cloud without inventing dates", {
  a<-box_cloud();a$dataset<-"Survey";a$provider<-"contributed";a$url<-"https://example.org/a.laz"
  a$acquired_start<-a$acquired_end<-"2020-01-01"
  key<-names(campaign_groups(a))[1];b<-box_cloud(10)
  expect_false(is.null(comparison_time_message(a)))
  expect_true(local_comparison_availability(a,a,key,b,TRUE,TRUE)$ready)
  expect_false(local_comparison_availability(a,a,key,b,TRUE,FALSE)$ready)
  expect_false(local_comparison_availability(a,a,key,NULL,TRUE,TRUE)$ready)
  expect_false(local_comparison_availability(a,a,key,box_cloud(5000),TRUE,TRUE)$ready)
})

test_that("local B is read without a second download and incompatible CRS is rejected", {
  a<-box_cloud();b<-a;b$filename<-"user.laz"
  path<-tempfile(fileext=".laz");writeBin(charToRaw("test"),path)
  directory<-tempfile();dir.create(directory)
  on.exit({unlink(path);unlink(directory,recursive=TRUE)})
  downloads<-0L;local_crs<-32631
  cloud<-function(crs)list(points=data.frame(X=1:10,Y=1:10,Z=1:10),crs=sf::st_crs(crs),units_note="Test metres")
  local_mocked_bindings(preview_remote_tile=function(...){downloads<<-downloads+1L;cloud(32631)},
    read_comparison_cloud=function(file,...){expect_identical(file,path);cloud(local_crs)},.package="alsdownloader")
  result<-compare_campaigns(a,b,a,directory,"m","m",path)
  expect_equal(downloads,1L);expect_equal(result$counts,c(10,10))
  expect_match(result$method,"user supplied")
  local_crs<-32632
  expect_error(compare_campaigns(a,b,a,directory,"m","m",path),"CRS definitions differ")
})

test_that("a real local LAS header supplies an approximate footprint without changing points", {
  skip_if_not_installed("lidR")
  xyz<-data.frame(X=c(500000,500010,500020,500030),Y=c(5000000,5000010,5000020,5000030),Z=c(10,20,30,40))
  las<-lidR::LAS(xyz);sf::st_crs(las)<-sf::st_crs(32631)
  path<-tempfile(fileext=".las");on.exit(unlink(path));lidR::writeLAS(las,path)
  before<-tools::md5sum(path)
  tile<-local_comparison_tile(list(name="user.las",datapath=path))
  expect_equal(sf::st_crs(tile)$epsg,4326)
  expect_equal(tile$filename,"user.las")
  expect_error(read_comparison_cloud(path,tile),"known horizontal and elevation units")
  points<-read_comparison_cloud(path,tile,z_units="m")
  expect_gt(nrow(points$points),0)
  expect_identical(tools::md5sum(path),before)
})

test_that("comparison windows crop large AOIs to the requested side", {
  a <- box_cloud(width=3000,height=3000)
  small <- comparison_region(a,a,a,100)
  large <- comparison_region(a,a,a,1000)
  expect_equal(aoi_area(small$overlap), .01, tolerance=.0001)
  expect_equal(aoi_area(large$overlap), 1, tolerance=.001)
  expect_error(comparison_region(a,a,a,1001), "between 100 and 1000")
  expect_error(comparison_region(a,a,a,NA), "between 100 and 1000")
  narrow <- box_cloud(width=40,height=40)
  expect_lt(aoi_area(comparison_region(a,a,narrow,100)$overlap), .002)
  distant <- rbind(a,box_cloud(x=6000))
  expect_equal(nrow(comparison_region(distant,a,a,100)$a),1L)
})

test_that("visual overlap clips both footprints and enforces the one km2 limit", {
  a <- box_cloud(); b <- box_cloud(500); roi <- box_cloud(width=3000,height=3000)
  overlap <- comparison_overlap(a,b,roi)
  expect_equal(aoi_area(overlap),.5,tolerance=.005)
  bb <- sf::st_bbox(sf::st_transform(overlap,32631))
  expect_equal(unname(bb[c('xmin','xmax')]),c(500,1000),tolerance=.01)
  expect_lt(aoi_area(comparison_overlap(a,b,box_cloud(width=750))),.3)
  expect_error(comparison_overlap(a,box_cloud(2000),roi),'no overlapping')
  expect_error(comparison_overlap(a,box_cloud(1000),roi),'no overlapping')
  expect_error(comparison_overlap(roi,roi,roi),'exceeds 1 km2')
  expect_error(comparison_overlap(data.frame(),b,roi),'footprints')
})

test_that("campaigns use provider dates without year verification", {
  tiles <- data.frame(dataset='3dep',provider='usgs3dep',
    url=c('https://example.org/projectA/copc/a.laz','https://example.org/projectB/copc/b.laz'),
    acquired_start=c('2020-01-01',NA),acquired_end=c('2019-12-31',NA))
  groups <- campaign_groups(tiles)
  expect_length(groups,2)
  expect_true(any(grepl('2020-01-01 to 2019-12-31',names(groups))))
  expect_true(any(grepl('unknown',names(groups))))
})

test_that("visualization shares coordinates without generating analysis outputs", {
  a <- box_cloud();a$epoch <- 'a'; b <- a;b$epoch <- 'b'
  directory <- tempfile();dir.create(directory);on.exit(unlink(directory,recursive=TRUE))
  crs_b <- 32631
  local_mocked_bindings(preview_remote_tile=function(tile,...) list(points=data.frame(X=1:10,Y=1,Z=(1:10)+if(tile$epoch=='b')3 else 0),crs=sf::st_crs(if(tile$epoch=='b')crs_b else 32631)),.package='alsdownloader')
  r <- compare_campaigns(a,b,a,directory)
  expect_equal(r$b[,3]-r$a[,3],rep(3,10))
  expect_equal(r$a[,1],r$b[,1])
  expect_false(any(c('grid','delta','resolution') %in% names(r)))
  expect_lte(r$overlap_km2,1)
  crs_b <- 32632
  expect_error(compare_campaigns(a,b,a,directory),'CRS definitions differ')
  expect_error(compare_campaigns(a[rep(1,5),],b,a,directory),'at most four')
})

test_that("visual comparison starts without campaigns or analysis controls", {
  app <- als_app()
  ui <- as.character(comparison_ui())
  expect_false(grepl('difference_csv|compare_resolution|compare_verified',ui))
  shiny::testServer(app,{
    session$flushReact()
    expect_match(output$aoi_status,'No study area')
    expect_match(output$compare_status,'Choose two campaigns')
  })
})

test_that("comparison is optional and requires an eligible pair in the same AOI", {
  a <- box_cloud();a$dataset <- 'A';a$provider <- 'test';a$acquired_start <- '2020-01-01';a$acquired_end <- '2020-01-31'
  b <- a;b$dataset <- 'B';b$acquired_start <- '2021-01-01';b$acquired_end <- '2021-01-31'
  c <- a;c$dataset <- 'C';c$acquired_start <- '2022-01-01';c$acquired_end <- '2022-01-31'
  tiles <- rbind(a,b,c);keys <- names(campaign_groups(tiles))
  gate <- comparison_availability
  expect_false(gate(a,a,keys[1],keys[2],TRUE)$ready)
  expect_false(gate(tiles,a,keys[1],keys[2],FALSE)$ready)
  expect_false(gate(tiles,a,'','',TRUE)$ready)
  expect_false(gate(tiles,a,keys[1],keys[1],TRUE)$ready)
  expect_true(gate(tiles,a,keys[1],keys[3],TRUE)$ready)
  expect_false(gate(tiles,NULL,keys[1],keys[2],TRUE)$ready)
  sf::st_geometry(b) <- sf::st_geometry(box_cloud(2000))
  expect_false(gate(rbind(a,b),box_cloud(width=4000),keys[1],keys[2],TRUE)$ready)
})

test_that("multiple files or projects do not imply distinct survey times", {
  a <- box_cloud(); a$dataset <- 'A'; a$provider <- 'test'
  a$acquired_start <- '2020-01-01'; a$acquired_end <- '2020-01-31'; a$url <- 'https://example.org/a.laz'
  b <- a; b$dataset <- 'B'; b$url <- 'https://example.org/b.laz'
  expect_match(comparison_time_message(rbind(a,b)), 'overlap or are the same')
  b$acquired_start <- NA_character_
  expect_match(comparison_time_message(rbind(a,b)), 'fewer than two')
  b$acquired_start <- '2020-03-01'; b$acquired_end <- '2020-03-31'
  tiles <- rbind(a,b); keys <- names(campaign_groups(tiles))
  expect_null(comparison_time_message(tiles))
  expect_true(comparison_availability(tiles,a,keys[1],keys[2],TRUE)$ready)
  tiles$url[2] <- tiles$url[1]
  expect_match(comparison_availability(tiles,a,keys[1],keys[2],TRUE)$message, 'share a source file')
})

test_that("a state-sized footprint still produces only a bounded comparison window", {
  state <- box_cloud(width=500000,height=600000)
  result <- comparison_region(state,state,state,1000)
  expect_equal(aoi_area(result$overlap),1,tolerance=.01)
})

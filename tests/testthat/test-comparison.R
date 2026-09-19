box_cloud <- function(x=0,y=0,width=1000,height=1000) sf::st_sf(geometry=sf::st_sfc(sf::st_polygon(list(matrix(c(x,y,x+width,y,x+width,y+height,x,y+height,x,y),ncol=2,byrow=TRUE))),crs=32631))

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

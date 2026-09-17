box_cloud <- function(x=0,y=0,width=1000,height=1000) sf::st_sf(geometry=sf::st_sfc(sf::st_polygon(list(matrix(c(x,y,x+width,y,x+width,y+height,x,y+height,x,y),ncol=2,byrow=TRUE))),crs=32631))

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

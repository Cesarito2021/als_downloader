test_that("declared square uses metric half-width and validates coordinates", {
  g<-zenodo_square(12,45,500,c("a.laz","b.laz"))
  expect_equal(nrow(g),2L)
  local<-sf::st_transform(g,"+proj=aeqd +lat_0=45 +lon_0=12 +datum=WGS84 +units=m +no_defs")
  bb<-sf::st_bbox(local)
  expect_equal(as.numeric(bb[c(3,4)]-bb[c(1,2)]),c(1000,1000),tolerance=.001)
  expect_true(all(sf::st_is_valid(g)))
  expect_true(all(sf::st_is_valid(zenodo_square(179.999,0,1000,"a.laz"))))
  expect_error(zenodo_square(181,0,500,"a.laz"),"longitude")
  expect_error(zenodo_square(0,86,500,"a.laz"),"latitude")
  expect_error(zenodo_square(0,0,0,"a.laz"),"Distance")
  expect_true(all(sf::st_is_valid(zenodo_square(0,0,10000,"a.laz"))))
  expect_error(zenodo_square(0,0,10001,"a.laz"),"Distance")
  expect_error(zenodo_square(0,0,500,character()),"Select")
})

test_that("approval preserves approximate labels and never invents a precise footprint", {
  queue<-tempfile();on.exit(unlink(queue,recursive=TRUE))
  m<-zenodo_metadata(list(id=12345,metadata=list(title="Synthetic square test",doi="10.5281/zenodo.12345",
    access_right="open",publication_date="2024-01-01",creators=list(list(name="Example")),
    license=list(id="cc-by-4.0")),files=list(list(key="a.laz",size=1000,checksum="md5:test"))))
  local_mocked_bindings(inspect_zenodo=function(...)m,.package="alsdownloader")
  g<-zenodo_square(12,45,500,"a.laz")
  p<-zenodo_build(m,g,"2018","ALS")
  expect_match(zenodo_coverage_label(p),"APPROXIMATE")
  props<-p$index$features[[1]]$properties
  expect_equal(props$coverage_method,"author_approximate_square")
  expect_equal(props$extent_distance_m,500)
  id<-submit_zenodo(p,queue)
  expect_equal(zenodo_submissions(queue)$status,"pending")
  expect_error(review_zenodo_submission(queue,id,"approve","Reviewer"),"confirmation")
  path<-review_zenodo_submission(queue,id,"approve","Reviewer",TRUE)
  tiles<-read_tile_index(path)
  expect_match(tiles$dataset,"approximate extent",fixed=TRUE)
  expect_match(tiles$citation,"not verified",fixed=TRUE)
  stored<-jsonlite::fromJSON(path,simplifyVector=FALSE)
  expect_equal(stored$features[[1]]$properties$coverage_method,"author_approximate_square")
})

test_that("the form requires explicit square confirmation and invalidates changed extents", {
  queue<-tempfile();on.exit(unlink(queue,recursive=TRUE))
  m<-zenodo_metadata(list(id=12345,metadata=list(title="Synthetic form test",doi="10.5281/zenodo.12345",
    access_right="open",publication_date="2024-01-01",creators=list(list(name="Example")),
    license=list(id="cc-by-4.0")),files=list(list(key="a.laz",size=1000,checksum="md5:test"))))
  local_mocked_bindings(inspect_zenodo=function(...)m,.package="alsdownloader")
  shiny::testServer(function(input,output,session)zenodo_submission_server(input,output,session,queue),{
    session$setInputs(zenodo_link="12345",zenodo_has_boundary="no",zenodo_longitude=12,
      zenodo_latitude=45,zenodo_distance=500,zenodo_extent_files="a.laz",zenodo_extent_confirm=FALSE,
      zenodo_acquired="2018",zenodo_platform="ALS",zenodo_email="")
    session$setInputs(zenodo_inspect=1)
    session$setInputs(zenodo_prepare=1)
    expect_match(output$zenodo_status,"Confirm")
    session$setInputs(zenodo_extent_confirm=TRUE)
    session$setInputs(zenodo_prepare=2)
    expect_match(output$zenodo_status,"AUTHOR-DECLARED APPROXIMATE")
    session$setInputs(zenodo_send=1)
    expect_equal(nrow(zenodo_submissions(queue)),1L)
    expect_equal(zenodo_submissions(queue)$status,"pending")
    session$setInputs(zenodo_distance=1000)
    session$setInputs(zenodo_send=2)
    expect_match(output$zenodo_status,"Check the proposal first")
    expect_equal(nrow(zenodo_submissions(queue)),1L)
  })
})

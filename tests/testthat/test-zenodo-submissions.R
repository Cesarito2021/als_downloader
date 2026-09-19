zenodo_fixture <- function(keys="survey.laz") {
  zenodo_metadata(list(id=12345,metadata=list(title="Fictitious aerial survey",doi="10.5281/zenodo.12345",
    access_right="open",publication_date="2024-06-01",creators=list(list(name="Example Author")),
    license=list(id="cc-by-4.0"),description="Synthetic test only",notes="Example credits"),
    files=lapply(keys,function(key)list(key=key,size=1000,checksum="md5:example"))))
}
zenodo_shape <- function() sf::st_sf(geometry=sf::st_as_sfc(sf::st_bbox(c(xmin=1,ymin=1,xmax=1.01,ymax=1.01),crs=4326)))

test_that("coverage polygons are required and legacy approximate proposals are rejected", {
  m<-zenodo_fixture();g<-zenodo_shape()
  expect_error(zenodo_build(m,NULL,"2018","ALS"))
  point<-sf::st_sf(geometry=sf::st_sfc(sf::st_point(c(1,1)),crs=4326))
  expect_error(zenodo_build(m,point,"2018","ALS"),"polygons")
  p<-zenodo_build(m,g,"2018","ALS")
  p$index$features[[1]]$properties$coverage_method<-"author_approximate_square"
  queue<-tempfile();on.exit(unlink(queue,recursive=TRUE))
  expect_error(submit_zenodo(p,queue),"no longer accepted")
  expect_false(dir.exists(queue))
  g$coverage_method<-"author_approximate_square"
  expect_error(zenodo_build(m,g,"2018","ALS"),"no longer accepted")
  # A saved legacy request also cannot be approved under the new policy.
  zenodo_write(p,file.path(queue,"requests",paste0(p$id,".json")))
  local_mocked_bindings(inspect_zenodo=function(...)m,.package="alsdownloader")
  expect_error(review_zenodo_submission(queue,p$id,"approve","Reviewer",TRUE),"no longer accepted")
  expect_false(dir.exists(file.path(queue,"approved")))
})

test_that("mail preview, failure and retry preserve pending approval and deduplicate", {
  queue<-tempfile();on.exit(unlink(queue,recursive=TRUE))
  config<-list(from="app@example.org",to="owner@example.org",review_url="http://127.0.0.1:8792/",preview=TRUE)
  old_options<-options(alsdownloader.submission_mail=config);on.exit(options(old_options),add=TRUE)
  p<-zenodo_build(zenodo_fixture(),zenodo_shape(),"2018","ALS","person@example.org")
  id<-submit_zenodo(p,queue)
  receipt<-file.path(queue,"notifications",paste0(id,".json"))
  expect_equal(jsonlite::fromJSON(receipt)$status,"preview")
  eml<-paste(readLines(file.path(queue,"notifications",paste0(id,".eml"))),collapse="\n")
  expect_match(eml,"To: owner@example.org",fixed=TRUE)
  body<-rawToChar(jsonlite::base64_dec(strsplit(eml,"\n\n",fixed=TRUE)[[1]][2]))
  expect_match(body,paste0("?zenodo_review=",id),fixed=TRUE)
  expect_match(body,"person@example.org",fixed=TRUE)
  expect_equal(zenodo_submissions(queue)$status,"pending")
  expect_false(dir.exists(file.path(queue,"approved")))
  config$preview<-FALSE;options(alsdownloader.submission_mail=config)
  local_mocked_bindings(zenodo_mail_send=function(...)stop("secret diagnostic"),.package="alsdownloader")
  expect_identical(submit_zenodo(p,queue),id)
  expect_equal(jsonlite::fromJSON(receipt)$status,"failed")
  expect_false(any(grepl("secret",readLines(receipt))))
  calls<-0L
  local_mocked_bindings(zenodo_mail_send=function(...){calls<<-calls+1L;TRUE},.package="alsdownloader")
  submit_zenodo(p,queue);submit_zenodo(p,queue)
  expect_equal(calls,1L)
  expect_equal(jsonlite::fromJSON(receipt)$status,"sent")
  expect_equal(zenodo_submissions(queue)$status,"pending")
  config$to<-"owner@example.org\r\nBcc: another@example.org"
  expect_error(zenodo_mail_message(p,config),"addresses")
  config$to<-"owner@example.org";config$review_url<-"https://public.example.org/"
  expect_error(zenodo_mail_message(p,config),"localhost")
})

test_that("Zenodo metadata and geometry create bounded explicit file mappings", {
  expect_equal(zenodo_record_id("https://zenodo.org/records/12345?preview_file=survey.laz"),"12345")
  expect_equal(zenodo_record_id("https://doi.org/10.5281/zenodo.12345"),"12345")
  expect_error(zenodo_record_id("https://example.org/records/12345"),"published")
  m<-zenodo_fixture(); g<-zenodo_shape()
  p<-zenodo_build(m,g,"","ALS")
  expect_null(p$index$features[[1]]$properties$acquired_start)
  expect_equal(p$index$features[[1]]$properties$url,"https://zenodo.org/records/12345/files/survey.laz")
  expect_equal(zenodo_dates("2018"),c("2018-01-01","2018-12-31"))
  expect_error(zenodo_dates("2020-02-30"),"dates")
  expect_error(zenodo_build(zenodo_fixture(c("a.laz","b.laz")),g,"","ALS"),"file_key")
  g$file_key<-"wrong.laz"
  expect_error(zenodo_build(m,g,"","ALS"),"Every file_key")
  g$file_key<-"survey.laz"
  expect_length(zenodo_build(m,rbind(g,g),"2018","ALS")$index$features,1)
  z<-zenodo_build(zenodo_fixture("survey clouds.zip"),zenodo_shape(),"2018","UAV-LiDAR")
  expect_match(z$index$features[[1]]$properties$url,"survey%20clouds.zip",fixed=TRUE)
  expect_error(zenodo_build(m,sf::st_set_crs(g,NA),"","ALS"),"CRS")
  expect_error(zenodo_build(m,g,"","TLS"),"Confirm")
  expect_error(zenodo_boundary_download(m,"missing.geojson"),"listed")
})

test_that("pending requests require maintainer acceptance and a stable record", {
  queue<-tempfile();on.exit(unlink(queue,recursive=TRUE))
  m<-zenodo_fixture();p<-zenodo_build(m,zenodo_shape(),"2018","ALS","private@example.org")
  local_mocked_bindings(inspect_zenodo=function(...)m,.package="alsdownloader")
  id<-submit_zenodo(p,queue)
  expect_identical(zenodo_build(m,zenodo_shape(),"2018","ALS")$id,id)
  expect_identical(submit_zenodo(p,queue),id)
  expect_equal(nrow(zenodo_submissions(queue)),1)
  expect_equal(zenodo_submissions(queue)$status,"pending")
  expect_false(dir.exists(file.path(queue,"approved")))
  expect_error(review_zenodo_submission(queue,id,"approve","Maintainer"),"confirmation")
  path<-review_zenodo_submission(queue,id,"approve","Maintainer",TRUE)
  expect_equal(nrow(read_tile_index(path)),1)
  expect_equal(zenodo_submissions(queue)$status,"approve")
  expect_false(any(grepl("private@example.org",readLines(path),fixed=TRUE)))
  expect_equal(nrow(find_tiles(zenodo_shape(),"contributed",tile_index_dir=file.path(queue,"approved"))),1)
  expect_error(review_zenodo_submission(queue,id,"approve","Maintainer",TRUE),"already has")
})

test_that("changed metadata, tampering and rejection cannot activate a proposal", {
  queue<-tempfile();on.exit(unlink(queue,recursive=TRUE))
  m<-zenodo_fixture();p<-zenodo_build(m,zenodo_shape(),"2018","ALS")
  id<-submit_zenodo(p,queue)
  changed<-m;changed$fingerprint<-"changed"
  local_mocked_bindings(inspect_zenodo=function(...)changed,.package="alsdownloader")
  expect_error(review_zenodo_submission(queue,id,"approve","Maintainer",TRUE),"metadata changed")
  expect_false(dir.exists(file.path(queue,"approved")))
  changed<-m
  p$index$features[[1]]$properties$url<-"https://example.org/evil.laz"
  zenodo_write(p,file.path(queue,"requests",paste0(id,".json")))
  # Approval must validate the submitted index as well as rebuilding it.
  expect_error(review_zenodo_submission(queue,id,"approve","Maintainer",TRUE),"contents")
  review_zenodo_submission(queue,id,"reject","Maintainer",reason="Incorrect mapping")
  expect_equal(zenodo_submissions(queue)$status,"reject")
  expect_false(dir.exists(file.path(queue,"approved")))
})

test_that("the public form can queue a proposal but cannot invoke reviewer actions", {
  queue<-tempfile();on.exit(unlink(queue,recursive=TRUE))
  coverage<-tempfile(fileext=".geojson");on.exit(unlink(coverage),add=TRUE)
  sf::st_write(zenodo_shape(),coverage,quiet=TRUE)
  local_mocked_bindings(inspect_zenodo=function(...)zenodo_fixture(),.package="alsdownloader")
  shiny::testServer(function(input,output,session)zenodo_submission_server(input,output,session,queue),{
    session$setInputs(zenodo_link="12345",zenodo_boundary_source="upload",zenodo_acquired="2018",
      zenodo_platform="ALS",zenodo_email="",zenodo_boundary=list(name="coverage.geojson",datapath=coverage))
    session$setInputs(zenodo_inspect=1)
    expect_match(output$zenodo_metadata_status,"Fictitious aerial survey")
    session$setInputs(zenodo_prepare=1)
    expect_match(output$zenodo_status,"Ready for maintainer review")
    session$setInputs(zenodo_send=1)
    expect_equal(zenodo_submissions(queue)$status,"pending")
    session$setInputs(zenodo_review_id=zenodo_submissions(queue)$id,zenodo_review_confirm=TRUE,zenodo_review_approve=1)
    expect_false(dir.exists(file.path(queue,"approved")))
    session$setInputs(zenodo_acquired="2019")
    session$setInputs(zenodo_send=2)
    expect_match(output$zenodo_status,"Check the proposal first")
  })
})

test_that("sample access rejects private networks and URL credentials before download", {
  resolve <- function(ip) function(host) ip
  for (ip in c("127.0.0.1","10.1.2.3","169.254.169.254","192.168.0.1","172.16.0.1","100.64.0.1","198.18.0.1","203.0.113.1","::1"))
    expect_error(public_sample_target("https://data.example/tile.laz",resolve(ip)),"publicly routable")
  for (url in c("http://data.example/a.laz","https://user:secret@data.example/a.laz","https://data.example:8443/a.laz"))
    expect_error(public_sample_target(url,resolve("8.8.8.8")),"public HTTPS")
  expect_equal(public_sample_target("https://data.example/a.laz",resolve("8.8.8.8"))$ip,"8.8.8.8")
  expect_error(public_sample_target("https://data.example/a.laz",resolve(c("8.8.8.8","10.0.0.1"))),"publicly routable")
})

test_that("connection UI separates technical completion from approval", {
  ui <- as.character(source_preflight_ui())
  expect_match(ui,"not approval for publication")
  expect_false(grepl("canvas|source_url|50 MB",ui))
  expect_match(as.character(source_submission_ui()),"source_email")
  shiny::testServer(als_app(), {
    session$setInputs(source_test=1,source_url="")
    expect_match(as.character(output$source_test_progress$html),"Add a public record link")
    expect_match(as.character(output$source_submission$html),"disabled")
  })
})


test_that("connection check only requests headers, regardless of cloud size", {
  dir <- tempfile();dir.create(dir);on.exit(unlink(dir,recursive=TRUE))
  local_mocked_bindings(public_sample_target=function(...)list(host="data.example",ip="8.8.8.8"),.package="alsdownloader")
  calls <- 0L
  head <- function(...) {calls <<- calls+1L;structure(list(status_code=200L,headers=list("content-type"="application/octet-stream","content-length"="9000000000")),class="response")}
  r <- source_preflight("https://data.example/cloud.laz",dir,head)
  expect_equal(calls,1L)
  expect_match(r$summary,"No point-cloud bytes")
  expect_false(any(c("points","origin") %in% names(r)))
  expect_equal(list.files(dir),"stage.txt")
  expect_error(source_preflight("https://data.example/index",dir,head),"manual review")
  html <- function(...)structure(list(status_code=200L,headers=list("content-type"="text/html")),class="response")
  expect_error(source_preflight("https://data.example/cloud.laz",dir,html),"web page")
})



test_that("other-source proposals use stable non-Zenodo links and optional contact", {
  x<-list(source_origin="https://repository.example.org/record/123",source_url="https://data.example.org/cloud.laz",
    source_license_url="https://creativecommons.org/licenses/by/4.0/",source_platform="Aircraft / helicopter ALS",
    source_open_license=TRUE,source_repository_confirm=TRUE)
  expect_true(source_request(x)$valid)
  x$source_scope<-"Local survey";expect_match(source_request(x)$body,"Scope: Local survey",fixed=TRUE)
  x$source_email<-"invalid";expect_false(source_request(x)$valid);x$source_email<-""
  x$source_year<-"2050";expect_false(source_request(x)$valid);x$source_year<-""
  x$source_repository_confirm<-FALSE;expect_false(source_request(x)$valid);x$source_repository_confirm<-TRUE
  x$source_origin<-"10.5281/zenodo.12345";expect_match(source_request(x)$message,"Share Zenodo dataset")
  x$source_origin<-"https://repository.example.org/record/123"
  for(url in c("https://drive.google.com/file/123","https://data.example.org/a.laz?X-Amz-Signature=secret","https://user:secret@data.example.org/a.laz")) {
    x$source_url<-url;expect_false(source_request(x)$valid)
  }
})

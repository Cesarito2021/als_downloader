test_that("sample access rejects private networks and URL credentials before download", {
  resolve <- function(ip) function(host) ip
  for (ip in c("127.0.0.1","10.1.2.3","169.254.169.254","192.168.0.1","172.16.0.1","100.64.0.1","198.18.0.1","203.0.113.1","::1"))
    expect_error(public_sample_target("https://data.example/tile.laz",resolve(ip)),"publicly routable")
  for (url in c("http://data.example/a.laz","https://user:secret@data.example/a.laz","https://data.example:8443/a.laz"))
    expect_error(public_sample_target(url,resolve("8.8.8.8")),"public HTTPS")
  expect_equal(public_sample_target("https://data.example/a.laz",resolve("8.8.8.8"))$ip,"8.8.8.8")
  expect_error(public_sample_target("https://data.example/a.laz",resolve(c("8.8.8.8","10.0.0.1"))),"publicly routable")
})

test_that("sample UI separates technical completion from approval", {
  ui <- as.character(source_preflight_ui())
  expect_match(ui,"not publication approval")
  expect_match(ui,"50 MB")
  expect_match(ui,"source_license_url")
  shiny::testServer(als_app(), {
    session$setInputs(source_test=1,source_sample_url="")
    expect_match(as.character(output$source_test_progress$html),"Provide a direct")
    session$setInputs(source_sample_url="https://data.example/a.laz",source_open_license=FALSE,source_test=2)
    expect_match(as.character(output$source_test_progress$html),"Declare an aerial")
  })
})

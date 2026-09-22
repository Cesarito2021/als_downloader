test_that("report GeoJSON retains the study polygon rather than its bounding box", {
  region <- sf::st_sf(geometry=sf::st_sfc(sf::st_polygon(list(matrix(c(0,0,2,0,1,1,0,0),ncol=2,byrow=TRUE))),crs=4326))
  geo <- report_geojson(region)
  expect_equal(geo$features[[1]]$geometry$type, "Polygon")
  expect_length(geo$features[[1]]$geometry$coordinates[[1]], 4)
})

test_that("map captures are tied to the current selection and cleaned up", {
  region <- sf::st_sf(geometry=sf::st_sfc(sf::st_polygon(list(matrix(c(0,0,1,0,1,1,0,1,0,0),ncol=2,byrow=TRUE))),crs=4326))
  tiles <- region; tiles$citation <- "Test source"; tiles$license_url <- "https://example.org/terms"
  png <- tempfile(fileext=".png"); on.exit(unlink(png))
  grDevices::png(png,400,400); graphics::plot(1:3); grDevices::dev.off()
  data <- paste0("data:image/png;base64,",jsonlite::base64_enc(readBin(png,"raw",n=file.info(png)$size)))
  server <- function(input,output,session) {
    selection <- shiny::reactiveVal(tiles)
    getmap <- report_map_server(input,output,session,shiny::reactive(region),shiny::reactive(selection()))
  }
  shiny::testServer(server, {
    messages <- list()
    session$sendCustomMessage <- function(type,message) messages[[type]] <<- message
    session$setInputs(report_map_request=list(action="download_report_pdf",nonce=1))
    token <- messages[['als-report-map']]$token
    expect_true(nzchar(token))
    session$setInputs(report_map_result=list(token=token,png=data,credits="OpenStreetMap contributors"))
    path <- getmap()$path
    expect_true(file.exists(path))
    expect_identical(messages[['als-report-map-ready']],"download_report_pdf")
    changed <- tiles; changed$citation <- "Changed selection"; selection(changed); session$flushReact()
    expect_false(file.exists(path))
    expect_error(getmap(),"not ready")
    session$setInputs(report_map_result=list(token=token,png=data,credits="OpenStreetMap contributors",nonce=2))
    expect_match(messages[['als-report-map-error']],"Selection changed")
  })
})

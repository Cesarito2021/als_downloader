review_box <- function(x=500000,width=1000) sf::st_sf(geometry=sf::st_as_sfc(sf::st_bbox(c(xmin=x,ymin=5000000,xmax=x+width,ymax=5001000),crs=32631)))

test_that("comparison choices require distinct periods and near-complete overlap", {
  x<-do.call(rbind,list(review_box(),review_box(),review_box(500900),review_box(504000),review_box()))
  x$acquired_start<-x$acquired_end<-c("2020-01-01","2021-01-01","2022-01-01","2023-01-01","2020-01-01")
  x$url<-paste0("https://example.org/",1:5,".laz")
  roi<-review_box(499000,7000)
  p<-comparison_tile_pairs(x,roi)
  expect_true(any(p$a==1 & p$b==2))
  expect_false(any(p$a==1 & p$b==5))
  expect_false(any(p$a %in% c(3,4) | p$b %in% c(3,4)))
  expect_true(all(p$fraction>=.99))
  x$url[2]<-paste0(x$url[1],"?token=other")
  expect_false(any(with(comparison_tile_pairs(x,roi),a==1 & b==2)))
  expect_equal(nrow(comparison_tile_pairs(x,review_box(600000))),0L)
  x$acquired_start[2]<-NA_character_
  expect_equal(nrow(comparison_tile_pairs(x,roi)),0L)
})

test_that("polygon ID mappings preserve file identity and reject incomplete choices", {
  g<-rbind(zenodo_shape(),zenodo_shape());g$cloud_id<-c("a","b")
  mapped<-zenodo_id_mapping(g,"cloud_id",c("a.laz","b.laz"))
  expect_identical(mapped$file_key,c("a.laz","b.laz"))
  expect_error(zenodo_id_mapping(g,"missing",c("a.laz","b.laz")),"column")
  expect_error(zenodo_id_mapping(g,"cloud_id",c("a.laz","b.laz"),c(a="a.laz")),"every tile")
  expect_error(zenodo_id_mapping(g,"cloud_id",c("a.laz","b.laz"),c(a="absent.laz",b="b.laz")),"Match every")
})

test_that("nonconsecutive years survive proposal and inbox reconstruction", {
  years<-zenodo_selected_years("multiple",3,c("2024","2017","2020"))
  expect_identical(years,"2017, 2020, 2024")
  expect_equal(zenodo_dates(years),c("2017-01-01","2024-12-31"))
  p<-zenodo_build(zenodo_fixture(),zenodo_shape(),years,"ALS")
  expect_identical(p$index$features[[1]]$properties$acquisition_years,c(2017L,2020L,2024L))
  expect_error(zenodo_selected_years("multiple",2,c("2020","2020")),"distinct")
})

test_that("report rendering failure is visible and the worker is cleared", {
  server<-function(input,output,session){
    report_download_server(input,output,session,function()NULL,function()NULL,function()NULL)
  }
  shiny::testServer(server,{
    sent<-list();session$sendCustomMessage<-function(type,message)sent[[type]]<<-message
    session$setInputs(report_prepare=1)
    expect_match(sent[['als-report-status']],"Report failed: Search for tiles")
  })
})

test_that("report worker receives an empty image list when no uploads exist", {
  captured<-NULL
  local_mocked_bindings(background_job=function(task,args){captured<<-args;list(is_alive=function()TRUE,kill_tree=function()NULL)},.package="ALSdownloadeR")
  server<-function(input,output,session){report_download_server(input,output,session,function()zenodo_shape(),function()NULL,function()NULL)}
  shiny::testServer(server,{
    session$setInputs(report_rgb=FALSE,report_details=FALSE,report_prepare=1)
    expect_identical(captured[[1]]$figures,character())
    expect_identical(captured[[1]]$format,"pdf")
  })
})

test_that("comparison controls only enable a verified pair", {
  x<-rbind(review_box(),review_box(),review_box(504000))
  x$acquired_start<-x$acquired_end<-c("2020-01-01","2021-01-01","2022-01-01")
  x$url<-paste0("https://example.org/",1:3,".laz");x$filename<-paste0(1:3,".laz");x$dataset<-"Survey"
  server<-function(input,output,session){
    state<-shiny::reactiveValues(tiles=x,aoi=review_box(499000,7000))
    comparison_server(input,output,session,state,"local",tempfile())
  }
  shiny::testServer(server,{
    session$setInputs(compare_source="remote",compare_opt_in=TRUE,epoch_a="1",epoch_b="2",compare_side_m=100)
    expect_equal(output$compare_temporal_ready,"yes")
    expect_match(output$compare_availability,"Ready")
    session$setInputs(epoch_b="3")
    expect_match(output$compare_availability,"Choose a matching pair")
    state$tiles<-x[c(1,3),];session$flushReact()
    expect_equal(output$compare_temporal_ready,"no")
  })
})

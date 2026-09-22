test_that("exported script preserves selected records and runs with local settings", {
  tiles <- data.frame(tile_id="a", provider="usgs3dep", dataset='A "quoted" survey',
    filename="a.laz",url="https://example.org/a.laz?token=private",
    size_bytes=123, citation="First line\nSecond line",license_url="https://example.org/license")
  script <- selection_script(tiles)
  expect_false(any(grepl("token=private",script,fixed=TRUE)))
  env <- new.env(parent=globalenv())
  calls <- list()
  env$download_als_data <- function(tiles,output_dir,workers,provider_limit,mode) {
    calls[[length(calls)+1L]] <<- list(tiles=tiles,output_dir=output_dir,workers=workers,provider_limit=provider_limit,mode=mode)
    data.frame(status="test_only")
  }
  invisible(capture.output(eval(parse(text=script),envir=env)))
  expect_length(calls,1L)
  expect_identical(calls[[1]]$mode,"local")
  expect_equal(calls[[1]]$workers,2L)
  expect_equal(calls[[1]]$provider_limit,2L)
  expect_equal(calls[[1]]$tiles$dataset,tiles$dataset)
  expect_equal(calls[[1]]$tiles$citation,tiles$citation)
  expect_equal(calls[[1]]$tiles$url,"https://example.org/a.laz")
  expect_equal(env$downloads$status,"test_only")
})

test_that("the greeting respects standard startup-message suppression", {
  expect_message(.onAttach(NULL,NULL),"Manuscript in preparation.",fixed=TRUE)
  expect_silent(suppressPackageStartupMessages(.onAttach(NULL,NULL)))
})

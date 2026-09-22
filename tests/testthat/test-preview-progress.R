test_that("cancelling a preview terminates its worker and removes temporary files", {
  shiny::testServer(als_app(), {
    session$flushReact()
    stopped <- FALSE
    path <- tempfile(fileext = ".laz")
    writeBin(as.raw(1:10), path)
    writeLines("Downloading test data...", paste0(path, ".status"))
    state$preview_path <- path
    state$preview_started <- Sys.time()
    state$preview_label <- "Test tile"
    state$preview_job <- list(is_alive = function() TRUE, kill_tree = function() stopped <<- TRUE)
    session$flushReact()
    expect_match(output$preview_status, "Downloading test data")
    expect_match(output$preview_status, "Elapsed:")
    session$setInputs(cancel_preview = 1)
    expect_true(stopped)
    expect_null(state$preview_job)
    expect_false(file.exists(path))
    expect_false(file.exists(paste0(path, ".status")))
    expect_match(output$preview_status, "cancelled")
  })
})

test_that("a stuck preview is stopped at its overall time limit", {
  shiny::testServer(als_app(), {
    session$flushReact()
    stopped <- FALSE
    state$preview_path <- tempfile()
    state$preview_started <- Sys.time() - 901
    state$preview_job <- list(is_alive = function() TRUE, kill_tree = function() stopped <<- TRUE)
    session$flushReact()
    expect_true(stopped)
    expect_null(state$preview_job)
    expect_match(output$preview_status, "15 minutes")
  })
})

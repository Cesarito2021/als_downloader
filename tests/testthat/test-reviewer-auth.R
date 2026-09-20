test_that("reviewer credentials store hashes and reject unauthorized identifiers", {
  skip_if_not_installed("sodium")
  path <- tempfile(); on.exit(unlink(path))
  password <- "Test-only administrator phrase 123"
  reviewer_store_credentials(path,c("FIRST@example.org","second@example.org"),password)
  cfg <- reviewer_credentials(path)
  expect_false(any(grepl(password,unlist(cfg),fixed=TRUE)))
  clock <- 1000
  access <- reviewer_access_controller(function()reviewer_credentials(path),function()clock)
  expect_null(access$login("outsider@example.org",password))
  expect_null(access$login("first@example.org","incorrect"))
  for(email in c("first@example.org","SECOND@example.org")) {
    token <- access$login(email,password)
    expect_true(access$valid(token))
  }
  clock <- 2800
  expect_false(access$valid(token))
  clock <- 1001
  reviewer_store_credentials(path,c("first@example.org","second@example.org"),"A different test-only phrase 456")
  expect_false(access$valid(token))
  unlink(path)
  expect_null(access$login("first@example.org",password))
  expect_false(access$valid(token))
})

test_that("rate limits span sessions sharing the application controller", {
  skip_if_not_installed("sodium")
  path <- tempfile(); on.exit(unlink(path))
  password <- "Test-only administrator phrase 123"
  reviewer_store_credentials(path,"owner@example.org",password)
  clock <- 1000
  access <- reviewer_access_controller(function()reviewer_credentials(path),function()clock)
  for(i in 1:5) expect_null(access$login("owner@example.org","wrong"))
  expect_null(access$login("owner@example.org",password))
  clock <- 1061
  expect_true(access$valid(access$login("owner@example.org",password)))
})

test_that("forged review inputs cannot disclose or decide pending requests", {
  skip_if_not_installed("sodium")
  path <- tempfile(); queue <- tempfile()
  on.exit(unlink(c(path,queue),recursive=TRUE))
  password <- "Test-only administrator phrase 123"
  reviewer_store_credentials(path,c("first@example.org","second@example.org"),password)
  clock <- new.env(); clock$value <- 1000
  access <- reviewer_access_controller(function()reviewer_credentials(path),function()clock$value)
  p <- zenodo_build(zenodo_fixture(),zenodo_shape(),"2018","ALS","private@example.org")
  id <- submit_zenodo(p,queue)
  local_mocked_bindings(inspect_zenodo=function(...)zenodo_fixture(),.package="alsdownloader")
  shiny::testServer(function(input,output,session)zenodo_submission_server(input,output,session,queue,"Maintainer",access),{
    session$setInputs(zenodo_review_id=id,zenodo_review_confirm=TRUE,zenodo_review_approve=1)
    expect_identical(zenodo_submissions(queue)$status,"pending")
    expect_error(output$zenodo_review_details,class="shiny.silent.error")
    session$setInputs(review_login_email="outsider@example.org",review_login_password=password,review_login=1)
    session$setInputs(zenodo_review_reject=1)
    expect_identical(zenodo_submissions(queue)$status,"pending")
    session$setInputs(review_login_email="second@example.org",review_login_password=password,review_login=2)
    session$setInputs(zenodo_review_id=id)
    expect_match(output$zenodo_review_details,"private@example.org",fixed=TRUE)
    clock$value <- 2801
    session$setInputs(zenodo_review_id=paste(rep("0",64),collapse=""))
    expect_error(output$zenodo_review_details,class="shiny.silent.error")
    session$setInputs(zenodo_review_id=id)
    session$setInputs(zenodo_review_approve=2,zenodo_review_confirm=TRUE)
    expect_identical(zenodo_submissions(queue)$status,"pending")
    session$setInputs(review_login_password=password,review_login=3)
    session$setInputs(zenodo_review_id=id,zenodo_review_confirm=TRUE,zenodo_review_approve=3)
    expect_identical(zenodo_submissions(queue)$status,"approve")
    decision <- jsonlite::fromJSON(file.path(queue,"decisions",paste0(id,".json")))
    expect_match(decision$reviewer,"second@example.org",fixed=TRUE)
    session$setInputs(review_logout=1)
    expect_error(output$zenodo_review_details,class="shiny.silent.error")
  })
})

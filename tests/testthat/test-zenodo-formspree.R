test_that("Formspree configuration accepts only public form endpoints", {
  old <- options(alsdownloader.formspree = list(endpoint="https://formspree.io/f/example", attachments=TRUE))
  on.exit(options(old))
  expect_true(zenodo_formspree_config()$attachments)
  for (url in c("http://formspree.io/f/example", "https://evil.test/f/example",
                "https://formspree.io@evil.test/f/example", "https://formspree.io/f/example?key=secret")) {
    options(alsdownloader.formspree=list(endpoint=url))
    expect_error(zenodo_formspree_config(), "public")
  }
})

test_that("link submissions preserve coverage mapping without contact or geometry loss", {
  p <- zenodo_build(zenodo_fixture(c("survey.laz","coverage.geojson")),zenodo_shape(),"2020","ALS","")
  config <- list(endpoint="https://formspree.io/f/example",attachments=FALSE)
  source <- list(kind="zenodo",key="coverage.geojson",mapping="survey.laz")
  fields <- zenodo_formspree_fields(p,source,config)
  expect_identical(fields$boundary,"https://zenodo.org/records/12345/files/coverage.geojson")
  expect_identical(fields$file_mapping,"survey.laz")
  expect_null(fields$email)
  expect_match(fields$subject,"[ALS Downloader]",fixed=TRUE)
  expect_error(zenodo_formspree_fields(p,list(kind="upload"),config),"enable proposal attachments")
  expect_error(zenodo_formspree_fields(p,list(kind="zenodo",key="absent.geojson"),config),"Coverage")
  html <- as.character(zenodo_formspree_ui(p,source,config))
  expect_match(html,'method="POST"',fixed=TRUE)
  expect_match(html,'target="_blank"',fixed=TRUE)
  expect_false(grepl("als-formspree-proposal",html,fixed=TRUE))
})

test_that("attachments carry the complete proposal and declared extents retain units", {
  p <- zenodo_build(zenodo_fixture(),zenodo_square(1,2,1000,"survey.laz"),"","ALS","person@example.org")
  config <- list(endpoint="https://formspree.io/f/example",attachments=FALSE)
  fields <- zenodo_formspree_fields(p,list(kind="approximate"),config)
  expect_identical(fields$centre_to_side_metres,"1000")
  expect_identical(fields$centre_longitude,"1")
  expect_identical(fields$email,"person@example.org")
  config$attachments <- TRUE
  html <- as.character(zenodo_formspree_ui(p,list(kind="upload"),config))
  expect_match(html,"als-zenodo-proposal-v1",fixed=TRUE)
  expect_match(html,"author_approximate_square",fixed=TRUE)
  expect_match(html,'name="attachment"',fixed=TRUE)
  expect_false(grepl("review_key",html,fixed=TRUE))
})

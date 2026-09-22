test_that("inbox imports are pending, persistent and deduplicated", {
  queue <- tempfile(); on.exit(unlink(queue, recursive = TRUE))
  options_old <- options(ALSdownloadeR.submission_mail = NULL)
  on.exit(options(options_old), add = TRUE)
  meta <- zenodo_fixture()
  local_mocked_bindings(inspect_zenodo = function(...) meta, .package = "ALSdownloadeR")
  shape <- zenodo_square(1, 1, 100, "survey.laz")
  p <- zenodo_build(meta, shape, "2018", "ALS", "private@example.org")
  fields <- zenodo_formspree_fields(p, list(kind = "approximate"), list(attachments = FALSE))
  id <- import_zenodo_inbox(fields, queue)
  expect_identical(id, p$id)
  expect_identical(import_zenodo_inbox(fields, queue), id)
  expect_equal(nrow(zenodo_submissions(queue)), 1L)
  expect_equal(zenodo_submissions(queue)$status, "pending")
  expect_false(dir.exists(file.path(queue, "approved")))
  expect_identical(zenodo_proposal(queue, id)$contact_email, "private@example.org")
  expect_false("contact_email" %in% names(zenodo_submissions(queue)))
  fields$reference <- paste(rep("0",64),collapse="")
  expect_error(import_zenodo_inbox(fields, queue), "changed")
  fields$record <- "https://zenodo.org/records/99999"
  expect_error(import_zenodo_inbox(fields, queue), "disagree")
})

test_that("inbox boundary import cannot fetch arbitrary URLs or change file mapping", {
  queue <- tempfile(); on.exit(unlink(queue, recursive = TRUE))
  options_old <- options(ALSdownloadeR.submission_mail = NULL)
  on.exit(options(options_old), add = TRUE)
  meta <- zenodo_fixture(c("survey.laz", "boundary.geojson"))
  p <- zenodo_build(meta, zenodo_shape(), "2018", "ALS")
  fields <- zenodo_formspree_fields(p, list(kind = "zenodo", key = "boundary.geojson",
    mapping = "survey.laz"), list(attachments = FALSE))
  calls <- 0L
  local_mocked_bindings(inspect_zenodo = function(...) meta,
    zenodo_boundary_download = function(...) {
      calls <<- calls + 1L
      path <- tempfile(fileext = ".geojson")
      sf::st_write(zenodo_shape(), path, quiet = TRUE)
      path
    }, .package = "ALSdownloadeR")
  bad <- fields; bad$boundary <- "http://127.0.0.1/private"
  expect_error(import_zenodo_inbox(bad, queue), "exact file link")
  expect_equal(calls, 0L)
  expect_identical(import_zenodo_inbox(fields, queue), p$id)
  bad <- fields; bad$files <- "boundary.geojson"
  expect_error(import_zenodo_inbox(bad, queue), "does not match")
  bad <- fields; bad$files <- "missing.laz"
  expect_error(import_zenodo_inbox(bad, queue), "not present")
  expect_equal(calls, 2L)
})

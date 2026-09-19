report_fixture <- function() {
  data.frame(
    filename = c("a.laz", "b.laz"), dataset = c("A", "B"), provider = c("usgs3dep", "usgs3dep"),
    url = c("https://example.org/a.laz", "https://example.org/b.laz"),
    acquired_start = c("2018-01-01", NA), acquired_end = c("2018-01-02", NA),
    size_bytes = c(1e8, NA), license_url = c("https://example.org/licence", "https://example.org/licence"),
    citation = c("Example provider", "Example provider"), stringsAsFactors = FALSE
  )
}

test_that("als_report renders a self-contained HTML summary without touching original data", {
  testthat::skip_if_not_installed("rmarkdown")
  testthat::skip_if_not(rmarkdown::pandoc_available())
  tiles <- report_fixture()
  dir <- tempfile(); on.exit(unlink(dir, recursive = TRUE))
  path <- als_report(tiles, dir, aoi_area_km2 = 1.5)
  expect_true(file.exists(path))
  expect_match(path, "\\.html$")
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(html, "a.laz", fixed = TRUE)
  expect_match(html, "usgs3dep", fixed = TRUE)
  expect_match(html, "1.5000", fixed = TRUE)
  expect_equal(tiles, report_fixture())
})

test_that("als_report handles an empty selection without erroring", {
  testthat::skip_if_not_installed("rmarkdown")
  testthat::skip_if_not(rmarkdown::pandoc_available())
  dir <- tempfile(); on.exit(unlink(dir, recursive = TRUE))
  path <- als_report(report_fixture()[0, ], dir)
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(html, "No tiles in this selection", fixed = TRUE)
})

test_that("als_report validates its inputs before touching rmarkdown", {
  expect_error(als_report(list(a = 1), tempfile()), "data frame")
  expect_error(als_report(report_fixture(), character(0)), "output directory")
})

test_that("als_report falls back to HTML when PDF isn't renderable", {
  testthat::skip_if_not_installed("rmarkdown")
  testthat::skip_if_not(rmarkdown::pandoc_available())
  dir <- tempfile(); on.exit(unlink(dir, recursive = TRUE))
  has_pdf <- requireNamespace("tinytex", quietly = TRUE) && isTRUE(tinytex::is_tinytex())
  if (has_pdf) {
    path <- als_report(report_fixture(), dir, format = "pdf")
    expect_match(path, "\\.pdf$")
  } else {
    expect_warning(path <- als_report(report_fixture(), dir, format = "pdf"), "tinytex")
    expect_match(path, "\\.html$")
  }
})

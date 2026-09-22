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
  path <- als_report(tiles, dir, aoi_area_km2 = 1.5, details = TRUE)
  expect_true(file.exists(path))
  expect_match(path, "\\.html$")
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(html, "a.laz", fixed = TRUE)
  expect_match(html, "usgs3dep", fixed = TRUE)
  expect_match(html, "1.5000", fixed = TRUE)
  expect_match(html, "Discover, inspect and download airborne LiDAR", fixed = TRUE)
  expect_true(grepl("GiB", html, fixed = TRUE))
  expect_true(grepl("size[[:space:]]+not[[:space:]]+reported", html))
  expect_true(grepl("not measured speeds", html, fixed = TRUE))
  expect_equal(tiles, report_fixture())
})

test_that("report figures reject non-PNG files and excessive attachments", {
  bad <- tempfile(); writeLines("not an image", bad)
  on.exit(unlink(bad))
  expect_error(als_report(report_fixture(), tempfile(), figures = bad), "PNG files")
  expect_error(als_report(report_fixture(), tempfile(), figures = rep(bad, 7)), "six PNG")
})

test_that("unknown sizes do not produce a zero-duration promise and figures embed", {
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available())
  x <- report_fixture(); x$size_bytes <- c(NA, -1)
  dir <- tempfile(); fig <- tempfile(fileext = ".png")
  on.exit(unlink(c(dir, fig), recursive = TRUE))
  grDevices::png(fig, width = 400, height = 400)
  graphics::plot(1:5, main = "Synthetic test figure")
  grDevices::dev.off()
  path <- als_report(x, dir, figures = fig)
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_true(grepl("Total unavailable: all file sizes are NA", html, fixed=TRUE))
  expect_false(grepl("0.000 GiB", html, fixed=TRUE))
  expect_true(grepl("data:image/png;base64,", html, fixed = TRUE))
  expect_true(grepl("Figure 1.", html, fixed = TRUE))
  expect_true(grepl("Summary", html, fixed = TRUE))
  expect_false(grepl("Conclusions and next steps", html, fixed = TRUE))
  expect_true(grepl("Software citation", html, fixed = TRUE))
  expect_true(grepl("2409885", html, fixed = TRUE))
  expect_true(grepl("2409886", html, fixed = TRUE))
  expect_true(grepl("2409887", html, fixed = TRUE))
  expect_false(grepl("Technical appendix", html, fixed = TRUE))
  mapped <- als_report(x, dir, map_image = fig, map_credits = "Test imagery provider",
    figures=fig, figure_captions="3D point cloud: example tile, France.")
  mapped_html <- paste(readLines(mapped, warn = FALSE), collapse = "\n")
  expect_true(grepl("Study area", mapped_html, fixed = TRUE))
  expect_true(grepl("Figure 2.", mapped_html, fixed = TRUE))
  expect_true(grepl("3D point cloud: example tile, France.", mapped_html, fixed = TRUE))
  expect_true(grepl("Test imagery provider", mapped_html, fixed = TRUE))
  expect_true(grepl("data:image/png;base64,", mapped_html, fixed = TRUE))
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
  expect_error(als_report(report_fixture(), tempfile(), details = NA), "TRUE or FALSE")
})

sf_report_fixture <- function() {
  square <- function(x, y, w) sf::st_sfc(sf::st_polygon(list(matrix(
    c(x, y, x + w, y, x + w, y + w, x, y + w, x, y), ncol = 2, byrow = TRUE))), crs = 4326)
  sf::st_sf(
    filename = c("a.laz", "b.laz"), dataset = c("A", "B"), provider = c("usgs3dep", "usgs3dep"),
    url = c("https://example.org/a.laz", "https://example.org/b.laz"),
    acquired_start = c("2018-01-01", "2020-01-01"), acquired_end = c("2018-01-02", "2020-01-02"),
    size_bytes = c(1e8, NA), license_url = c("https://example.org/licence", "https://example.org/licence"),
    citation = c("Example provider", "Example provider"), stringsAsFactors = FALSE,
    geometry = c(square(0, 0, 1), square(0.5, 0.5, 1))
  )
}

aoi_report_fixture <- function() sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(matrix(
  c(0.2, 0.2, 0.8, 0.2, 0.8, 0.8, 0.2, 0.8, 0.2, 0.2), ncol = 2, byrow = TRUE))), crs = 4326))

test_that("als_report draws a map figure when tiles and aoi both carry geometry", {
  testthat::skip_if_not_installed("rmarkdown")
  testthat::skip_if_not(rmarkdown::pandoc_available())
  tiles <- sf_report_fixture(); aoi <- aoi_report_fixture()
  dir <- tempfile(); on.exit(unlink(dir, recursive = TRUE))
  path <- als_report(tiles, dir, aoi_area_km2 = 0.36, aoi = aoi, details = TRUE)
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(html, "a.laz", fixed = TRUE)
  expect_match(html, "data:image/png;base64,", fixed = TRUE)
  expect_match(html, "100.00 MB", fixed = TRUE)
  expect_equal(tiles, sf_report_fixture())
})

test_that("als_report skips the map figure gracefully without aoi geometry", {
  testthat::skip_if_not_installed("rmarkdown")
  testthat::skip_if_not(rmarkdown::pandoc_available())
  dir <- tempfile(); on.exit(unlink(dir, recursive = TRUE))
  path <- als_report(sf_report_fixture(), dir)
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_false(grepl("Figure 1.", html, fixed = TRUE))
})

test_that("report dates and storage preserve precision without false zeros", {
  expect_equal(report_date_label("2022-08-03", "2022-08-03"), "2022")
  expect_equal(report_date_label("2022-01-01", "2022-12-31"), "2022")
  expect_equal(report_date_label("2022-08-03", "2022-08-05"), "2022")
  expect_equal(report_date_label(NA, NA), "NA; 1 file(s) with incomplete acquisition dates")
  expect_equal(report_storage_label(100e6), "100.00 MB")
  expect_equal(report_storage_label(c(1e9, 500e6)), "1.50 GB")
  expect_equal(report_storage_label(c(NA, -1)), "Total unavailable: all file sizes are NA")
  expect_equal(report_storage_label(c(100e6, NA)), "100.00 MB known subtotal; total incomplete; 1 file(s) with size not reported")
  expect_equal(report_storage_label(100), "<0.01 MB")
  expect_error(als_report(report_fixture(), tempfile(), figure_captions="extra"), "caption per figure")
})

test_that("als_report validates the aoi argument", {
  expect_error(als_report(report_fixture(), tempfile(), aoi = data.frame(x = 1)), "sf polygon")
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

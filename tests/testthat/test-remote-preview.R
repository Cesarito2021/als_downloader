test_that("remote previews reject unknown or excessive sizes before downloading", {
  skip_if_not_installed("lidR")
  tile <- data.frame(url = "https://example.org/tile.laz", provider = "opentopography", filename = "tile.laz",
    license_url = "https://example.org/terms", citation = "Test fixture only")
  for (reported in c(NA_character_, "0", as.character(201 * 1024^2))) {
    local_mocked_bindings(HEAD = function(...) structure(list(status_code = 200L,
      headers = list(`content-length` = reported)), class = "response"),
      GET = function(...) stop("GET must not run"), .package = "httr")
    expect_error(alsdownloader:::preview_remote_tile(tile), "known file size")
  }
  tile$url <- "http://example.org/tile.laz"
  expect_error(alsdownloader:::preview_remote_tile(tile), "Only HTTPS")
})

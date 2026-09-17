test_that("shared-grid differences preserve offsets and do not invent missing coverage", {
  a <- data.frame(X = c(rep(1, 5), rep(11, 5)), Y = 1, Z = rep(1:5, 2))
  b <- data.frame(X = c(rep(1, 5), rep(21, 5)), Y = 1, Z = rep(4:8, 2))
  g <- alsdownloader:::comparison_grid(a, b, 10, 5)
  expect_equal(nrow(g), 3L)
  expect_equal(sum(g$eligible), 1L)
  expect_equal(g$delta_b_minus_a[g$eligible], 3)
  expect_true(all(is.na(g$delta_b_minus_a[!g$eligible])))
  expect_equal(alsdownloader:::comparison_grid(a, a, 10, 5)$delta_b_minus_a, c(0, 0))
  expect_equal(alsdownloader:::comparison_grid(a, transform(a, Z = Z - 2), 10, 5)$delta_b_minus_a, c(-2, -2))
  expect_false(any(alsdownloader:::comparison_grid(a, b[1:4, ], 10, 5)$eligible))
  expect_error(alsdownloader:::comparison_grid(a, b, 0), "resolution")
  expect_error(alsdownloader:::comparison_grid(a, b, 10, 1), "three")
})

test_that("campaigns separate survey projects and keep unknown acquisition dates explicit", {
  tiles <- data.frame(dataset = "3dep", provider = "usgs3dep",
    url = c("https://example.org/projectA/copc/a.laz", "https://example.org/projectB/copc/b.laz", "https://example.org/projectA/copc/c.laz"),
    acquired_start = c("2018-01-01", "2024-01-01", NA), acquired_end = c("2018-12-31", "2024-12-31", NA))
  groups <- alsdownloader:::campaign_groups(tiles)
  expect_length(groups, 3)
  expect_true(any(grepl("unknown", names(groups))))
  expect_true(any(grepl("projectB.*2024", names(groups))))
})

test_that("comparison samples share an origin and reject mismatched CRS", {
  aoi <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(matrix(c(0,0,.0001,0,.0001,.0001,0,.0001,0,0), ncol = 2, byrow = TRUE))), crs = 4326))
  a <- data.frame(epoch = "a"); b <- data.frame(epoch = "b")
  directory <- tempfile(); dir.create(directory); on.exit(unlink(directory, recursive = TRUE))
  crs_b <- 32631
  local_mocked_bindings(preview_remote_tile = function(tile, ...) list(
    points = data.frame(X = 500001:500010, Y = 1000001, Z = (1:10) + if (tile$epoch == "b") 3 else 0),
    crs = sf::st_crs(if (tile$epoch == "b") crs_b else 32631)), .package = "alsdownloader")
  r <- alsdownloader:::compare_campaigns(a, b, aoi, 20, 5, directory)
  expect_equal(r$b[,3] - r$a[,3], rep(3, 10))
  expect_equal(r$a[,1], r$b[,1])
  expect_equal(r$grid$delta_b_minus_a, 3)
  expect_equal(r$origin, c(500001, 1000001, 1))
  crs_b <- 32632
  expect_error(alsdownloader:::compare_campaigns(a, b, aoi, 20, 5, directory), "CRS definitions differ")
  expect_error(alsdownloader:::compare_campaigns(a[rep(1, 5), , drop = FALSE], b, aoi, 20, 5, directory), "at most four")
})
test_that("difference gate requires chronological surveys and verified vertical references", {
  dates <- list(a = data.frame(acquired_start = "2018-01-01", acquired_end = "2018-12-31"),
                b = data.frame(acquired_start = "2024-01-01", acquired_end = "2024-12-31"))
  result <- list(grid = data.frame(eligible = TRUE))
  gate <- alsdownloader:::comparison_gate
  expect_match(gate(result, dates, FALSE, "NAVD88", "NAVD88"), "Verify matching")
  expect_match(gate(result, dates, TRUE, "NAVD88", "ellipsoid"), "Verify matching")
  expect_match(gate(result, dates, TRUE, "", ""), "Verify matching")
  expect_match(gate(result, dates, TRUE, "NAVD88", "navd88"), "Exploratory difference enabled")
  bad_dates <- dates; bad_dates$a$acquired_start <- "2019-01-01"
  expect_match(gate(result, bad_dates, TRUE, "NAVD88", "NAVD88"), "Invalid acquisition interval")
  expect_match(gate(list(grid = data.frame(eligible = FALSE)), dates, TRUE, "NAVD88", "NAVD88"), "No shared")
  dates$b$acquired_start <- "2018-01-01"
  expect_match(gate(result, dates, TRUE, "NAVD88", "NAVD88"), "non-overlapping")
  dates$b$acquired_start <- NA_character_
  expect_match(gate(result, dates, TRUE, "NAVD88", "NAVD88"), "unknown")
})
test_that("app comparison starts safely without a search or campaigns", {
  shiny::testServer(als_app(), {
    session$flushReact()
    expect_match(output$aoi_status, "No study area")
    expect_match(output$compare_gate, "Load two campaigns")
  })
})

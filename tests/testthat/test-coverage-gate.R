test_that("unverified research footprints are excluded from active coverage", {
  catalog <- provider_catalog()
  expect_equal(nrow(catalog), 37L)
  expect_false(any(catalog$id %in% c("sila_zenodo", "treelims_zenodo", "eba_zenodo", "pnoa")))
  expect_false(any(catalog$name == "OpenTopography AUS11_Victor"))
  expect_false(exists("zenodo_source_ui", envir=asNamespace("alsdownloader"), inherits=FALSE))
  expect_s3_class(als_app(), "shiny.appobj")
})

test_that("Italy and Spain reappear only as portal links, not the removed research/blocked rows", {
  catalog <- provider_catalog()
  italy <- catalog[catalog$country_code == 380, ]
  spain <- catalog[catalog$country_code == 724, ]
  expect_equal(nrow(italy), 1L)
  expect_equal(nrow(spain), 1L)
  expect_false(any(italy$implemented))
  expect_false(any(spain$implemented))
  expect_false("sila_zenodo" %in% italy$id)
  expect_false("pnoa" %in% spain$id)
})

test_that("national portals remain accessible without claiming an AOI adapter", {
  catalog <- provider_catalog()
  for (country in c(246,578,616,724,442,380,705)) {
    x <- catalog[catalog$country_code == country, ]
    expect_equal(nrow(x),1L)
    expect_false(x$implemented)
    ui <- as.character(country_source_links(catalog,country))
    expect_match(ui,x$info_url,fixed=TRUE)
    expect_match(ui,"In-app AOI search is not yet available")
  }
})

test_that("all EU member states mapped so far resolve to a catalogued country code", {
  catalog <- provider_catalog()
  eu_codes <- c(
    40, 56, 100, 191, 196, 203, 208, 233, 246, 250, 276, 300, 348, 372, 380,
    428, 440, 442, 470, 528, 616, 620, 642, 703, 705, 724, 752)
  mapped <- eu_codes %in% catalog$country_code
  expect_true(sum(mapped) >= 25L)
  expect_setequal(setdiff(eu_codes, catalog$country_code), c(196, 300))
})

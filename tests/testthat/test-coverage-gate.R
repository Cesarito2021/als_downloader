test_that("unverified research footprints are excluded from active coverage", {
  catalog <- provider_catalog()
  expect_equal(nrow(catalog), 16L)
  expect_false(any(catalog$id %in% c("sila_zenodo", "treelims_zenodo", "eba_zenodo", "pnoa")))
  expect_false(any(catalog$country_code == 380))
  expect_false(any(catalog$country_code == 724))
  expect_false(any(catalog$name == "OpenTopography AUS11_Victor"))
  expect_false(exists("zenodo_source_ui", envir=asNamespace("alsdownloader"), inherits=FALSE))
  expect_s3_class(als_app(), "shiny.appobj")
})

test_that("national portals remain accessible without claiming an AOI adapter", {
  catalog <- provider_catalog()
  for (country in c(124,250,578)) {
    x <- catalog[catalog$country_code == country, ]
    expect_equal(nrow(x),1L)
    expect_false(x$implemented)
    ui <- as.character(country_source_links(catalog,country))
    expect_match(ui,x$info_url,fixed=TRUE)
    expect_match(ui,"In-app AOI search is not yet available")
  }
})

test_that("figure credits preserve distinct source notices and exact licence links", {
  x <- data.frame(citation = c("Producer A; distributor B", "Producer A; distributor B", "Producer C"),
    license_url = c("https://example.org/terms?dataset=1", "https://example.org/terms?dataset=1", "https://example.org/terms/2"))
  credit <- figure_attribution(x)
  expect_length(credit, 2L)
  expect_match(credit[1], "Producer A; distributor B", fixed = TRUE)
  expect_match(credit[1], "https://example.org/terms?dataset=1", fixed = TRUE)
  expect_match(credit[2], "Producer C", fixed = TRUE)
})

test_that("unknown local-file terms are not presented as licensed", {
  expect_match(figure_attribution(), "not supplied", fixed = TRUE)
  x <- data.frame(citation = c(NA_character_, ""), license_url = c("", NA_character_))
  expect_length(figure_attribution(x), 1L)
  expect_match(figure_attribution(x), "verify with provider", fixed = TRUE)
  expect_false(any(grepl("GPL|MIT|CC BY", figure_attribution(x))))
})

test_that("Auckland figure credits use the derivative notice without changing copy metadata", {
  x <- data.frame(dataset = c("Auckland_2013", "Other dataset"),
    citation = rep("Copyright in this work is owned by Auckland Council", 2),
    license_url = rep("https://creativecommons.org/licenses/by/3.0/nz/", 2))
  original <- x
  credit <- figure_attribution(x)
  expect_match(credit[1], "underlying dataset from which this work has been derived", fixed = TRUE)
  expect_match(credit[2], "Copyright in this work", fixed = TRUE)
  expect_identical(x, original)
})

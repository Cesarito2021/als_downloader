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

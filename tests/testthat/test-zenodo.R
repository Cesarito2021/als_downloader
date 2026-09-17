test_that("Zenodo connection offers only the reviewed original aerial file", {
  aerial <- list(key = "merged.las", size = 6822549289,
    links = list(self = "https://zenodo.org/api/records/3633629/files/merged.las/content"), checksum = "md5:sample")
  record <- list(id = 3633629L, metadata = list(license = list(id = "cc-by-4.0"), doi = "10.5281/zenodo.3633629"),
    files = list(list(key = "ID_1.las"), aerial, list(key = "myLas_norm_lt22.las")))
  expect_identical(sila_source(record)$filename, "merged.las")
  expect_equal(sila_source(record)$size, 6822549289)
  missing <- record; missing$files <- list(list(key = "ID_1.las"))
  expect_error(sila_source(missing), "unavailable")
  changed <- record; changed$files[[2]]$links$self <- "https://example.org/merged.las"
  expect_error(sila_source(changed), "link has changed")
  changed <- record; changed$metadata$license$id <- "restricted"
  expect_error(sila_source(changed), "license or DOI")
  changed <- record; changed$id <- 1L
  expect_error(sila_source(changed), "Unexpected")
})

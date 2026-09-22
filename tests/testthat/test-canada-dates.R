test_that("Canada dates use the exact province/project metadata rather than file years", {
  x <- als_metadata_columns(data.frame(provider="canelevation",filename="tile_2025.laz",
    url="https://canelevation-lidar-point-clouds.s3.ca-central-1.amazonaws.com/pointclouds_nuagespoints/AB/Athabasca_2018/tile_2025.laz",
    acquired_start=NA_character_,acquired_end=NA_character_))
  md <- data.frame(ID="AB_Athabasca_2018_PointCloud",TEMPORAL_EXTENT_DATE_MIN="2018-10-06",TEMPORAL_EXTENT_DATE_MAX="2018-10-10")
  old <- options(ALSdownloadeR.canada_date_index=md);on.exit(options(old))
  y <- extract_als_dates_canelevation(x)
  expect_equal(y$acquisition_year,2018L)
  expect_equal(y$acquired_end,"2018-10-10")
  expect_equal(y$date_scope,"project")
  md$ID <- "ON_Athabasca_2018_PointCloud";options(ALSdownloadeR.canada_date_index=md)
  expect_equal(extract_als_dates_canelevation(x)$date_status,"not_provided")
  md$ID <- "AB_Athabasca_2018_PointCloud";md$TEMPORAL_EXTENT_DATE_MAX <- "2017-01-01"
  options(ALSdownloadeR.canada_date_index=md)
  expect_equal(extract_als_dates_canelevation(x)$date_status,"conflict")
})

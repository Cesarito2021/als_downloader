test_that("display crop and voxel thinning keep actual source coordinates", {
  points <- expand.grid(X=0:20,Y=0:20,Z=0:4)
  original <- points
  crop <- alsdownloader:::forest_display_sample(points,25,50,50,0)
  restored <- as.data.frame(Map(`+`,crop,attr(crop,'origin')))
  expect_true(all(restored$X>=7.5 & restored$X<=12.5))
  expect_true(all(restored$Y>=7.5 & restored$Y<=12.5))
  expect_equal(points,original)
  voxel <- alsdownloader:::forest_display_sample(points,100,50,50,2)
  coordinates <- as.data.frame(Map(`+`,voxel,attr(voxel,'origin')))
  expect_lt(nrow(voxel),nrow(points))
  expect_true(all(paste(coordinates$X,coordinates$Y,coordinates$Z) %in% paste(points$X,points$Y,points$Z)))
  extreme <- rbind(points,data.frame(X=10,Y=10,Z=999))
  full <- alsdownloader:::forest_display_sample(extreme)
  expect_equal(max(full$Z)+attr(full,'origin')[['Z']],999)
  expect_lte(nrow(alsdownloader:::forest_display_sample(points,max_points=100)),100)
  expect_error(alsdownloader:::forest_display_sample(points,window=0),'Invalid')
})

test_that("classification stays aligned through filtering, cropping and sampling", {
  p <- data.frame(X=c(NA, 0:10), Y=c(NA, 0:10), Z=c(NA, 0:10),
    Classification=c(99, 0:10))
  sampled <- forest_display_sample(p, window=50, center_x=50, center_y=50, voxel=2, max_points=3)
  expect_equal(attr(sampled, "classification"),
    as.integer(sampled$X + attr(sampled, "origin")[["X"]]))
  expect_named(sampled, c("X", "Y", "Z"))
  invalid <- preview_points(data.frame(X=1:4,Y=1:4,Z=1:4,Classification=c(2,NA,300,2.5)))
  expect_identical(attr(invalid,"classification"), c(2L,NA_integer_,NA_integer_,NA_integer_))
  expect_null(attr(preview_points(p[c("X","Y","Z")]), "classification"))
})


test_that("intensity stays aligned with retained source returns", {
  p <- data.frame(X=c(NA,0:10),Y=c(NA,0:10),Z=c(NA,0:10),Intensity=c(99,100:110))
  out <- forest_display_sample(p,window=50,voxel=2,max_points=3)
  expect_equal(attr(out,"intensity"),out$X+attr(out,"origin")[["X"]]+100)
  expect_null(attr(preview_points(p[c("X","Y","Z")]),"intensity"))
  p$Intensity <- rep(-1,nrow(p))
  expect_true(all(is.na(attr(preview_points(p),"intensity"))))
})

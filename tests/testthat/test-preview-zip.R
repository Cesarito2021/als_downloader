test_that("ZIP previews select one cloud and clean up extracted files", {
  d<-tempfile();dir.create(d);on.exit(unlink(d,recursive=TRUE))
  writeBin(c(charToRaw("LASF"),raw(400)),file.path(d,"one.las"))
  z<-file.path(d,"one.zip");zip::zipr(z,"one.las",root=d)
  seen<-NULL
  p<-read_zip_preview(z,function(f){seen<<-f;data.frame(X=1,Y=2,Z=3)},10000)
  expect_equal(nrow(p),1L);expect_equal(attr(p,"archive_member"),"one.las")
  expect_false(file.exists(seen));expect_false(dir.exists(paste0(z,".contents")))
  expect_error(read_zip_preview(z,function(f)stop("reader failed"),10000),"reader failed")
  expect_false(dir.exists(paste0(z,".contents")))
  expect_error(read_zip_preview(z,identity,10),"uncompressed size")
  file.copy(file.path(d,"one.las"),file.path(d,"two.laz"))
  zip::zipr(z,c("one.las","two.laz"),root=d)
  expect_error(read_zip_preview(z,identity,10000),class="als_zip_selection")
  p<-read_zip_preview(z,function(f)data.frame(X=1),10000,"two.laz")
  expect_equal(attr(p,"archive_member"),"two.laz")
  expect_error(read_zip_preview(z,identity,10000,"missing.las"),"listed LAS/LAZ")
})

test_that("ZIP previews reject traversal and non-cloud contents before extraction", {
  local_mocked_bindings(unzip=function(...)data.frame(Name="../escape.las",Length=400),.package="utils")
  expect_error(read_zip_preview("unused",identity,10000),"Unsafe")
})

test_that("ZIP previews reject files with invalid LAS signatures", {
  d<-tempfile();dir.create(d);on.exit(unlink(d,recursive=TRUE))
  writeLines("not a point cloud",file.path(d,"one.las"))
  zip::zipr(file.path(d,"bad.zip"),"one.las",root=d)
  expect_error(read_zip_preview(file.path(d,"bad.zip"),identity,10000),"not a LAS/LAZ")
})

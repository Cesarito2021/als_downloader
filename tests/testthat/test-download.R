fixture_tile <- function() data.frame(tile_id="example", provider="opentopography",dataset="fixture",
  filename="tile.laz",url="https://example.org/tile.laz?secret=do-not-log",size_bytes=227,
  citation="Fixture only",license_url="https://example.org/terms")

fake_response <- function(part, status=200L, valid=TRUE, reported=227) {
  payload <- raw(227)
  payload[1:4] <- charToRaw(if(valid) "LASF" else "HTML")
  writeBin(payload,part)
  structure(list(status_code=status,headers=list(`content-length`=as.character(reported))),class="response")
}

test_that("transfer records permit validated resume and redact URLs", {
  dest <- tempfile();dir.create(dest);on.exit(unlink(dest,recursive=TRUE))
  local_mocked_bindings(fetch_asset=function(url,part,timeout) fake_response(part),.package="alsdownloader")
  first <- download_tiles(fixture_tile(),dest,retries=0,workers=1)
  expect_equal(first$status,"downloaded")
  second <- download_tiles(fixture_tile(),dest,retries=0,workers=1)
  expect_equal(second$status,"verified_existing")
  expect_false(any(grepl("secret",readLines(file.path(dest,"manifest.csv")))))
  writeBin(raw(250),first$path)
  third <- download_tiles(fixture_tile(),dest,retries=0,workers=1)
  expect_equal(third$status,"downloaded")
})

test_that("HTTP and validation failures cannot be marked downloaded", {
  for (scenario in c("http","html","size")) {
    dest <- tempfile();dir.create(dest)
    local_mocked_bindings(fetch_asset=function(url,part,timeout)
      fake_response(part,status=if(scenario=="http")500L else 200L,
                    valid=scenario!="html",reported=if(scenario=="size")300 else 227),.package="alsdownloader")
    result <- download_tiles(fixture_tile(),dest,retries=0,workers=1)
    expect_equal(result$status,"failed")
    expect_false(file.exists(result$path))
    unlink(dest,recursive=TRUE)
  }
})

test_that("throttling stops without automatic retry", {
  dest <- tempfile();dir.create(dest);on.exit(unlink(dest,recursive=TRUE))
  calls <- 0L
  local_mocked_bindings(fetch_asset=function(url,part,timeout) {
    calls <<- calls+1L; fake_response(part,status=429L)
  },.package="alsdownloader")
  result <- download_tiles(fixture_tile(),dest,retries=2,workers=1)
  expect_equal(calls,1L)
  expect_equal(result$status,"failed")
  expect_match(result$message,"429")
})

test_that("untracked files and concurrent writers are protected", {
  dest <- tempfile();dir.create(dest);on.exit(unlink(dest,recursive=TRUE))
  tile <- fixture_tile();file <- alsdownloader:::asset_path(as.list(tile),dest)
  writeBin(charToRaw("User-owned file"),file)
  result <- download_tiles(tile,dest,retries=0)
  expect_equal(result$status,"failed")
  expect_match(result$message,"Untracked")
  expect_equal(readChar(file,15),"User-owned file")
  dir.create(file.path(dest,".als-transfer-lock"))
  expect_error(download_tiles(tile,dest),"locked")
})

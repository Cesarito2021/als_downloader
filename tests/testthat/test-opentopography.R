ot_square <- function(x=0) sf::st_polygon(list(matrix(c(x,0,x+1,0,x+1,1,x,1,x,0),ncol=2,byrow=TRUE)))

ot_fixture <- function(urls=c("https://opentopography.s3.sdsc.edu/pc-bulk/Test/a.laz",
                            "https://opentopography.s3.sdsc.edu/pc-bulk/Test/b.laz")) {
  root <- tempfile(); dir.create(root)
  x <- sf::st_sf(url=urls,geometry=sf::st_sfc(ot_square(),ot_square(3),crs=4326))
  sf::st_write(x,file.path(root,"Test.shp"),quiet=TRUE)
  path <- file.path(root,"Test_TileIndex.zip")
  zip::zipr(path,list.files(root,full.names=TRUE),root=root)
  list(root=root,path=path,tiles=x)
}

ot_test_registry <- function(f) sf::st_sf(dataset="Test",title="Test survey",
  access_status="ready",index_url="https://opentopography.s3.sdsc.edu/pc-bulk/Test/Test_TileIndex.zip",
  index_sha256=digest::digest(file=f$path,algo="sha256"),acquired_start="2019-01-01",
  acquired_end="2019-12-31",license_url="https://opentopography.org/usageterms",
  citation="Test producer. Distributed by OpenTopography.",
  geometry=sf::st_union(f$tiles))

test_that("native OpenTopography search preserves tile gaps and metadata", {
  f <- ot_fixture(); on.exit(unlink(f$root,recursive=TRUE))
  r <- ot_test_registry(f); reader <- function(record) f$path
  aoi <- read_aoi(sf::st_sfc(ot_square(.1),crs=4326))
  tiles <- search_ot_catalog(aoi,100,r,reader)
  expect_equal(tiles$url,f$tiles$url[1])
  expect_equal(tiles$dataset,"Test")
  expect_equal(tiles$acquired_start,"2019-01-01")
  expect_silent(require_data_terms(tiles))
  gap <- read_aoi(sf::st_sfc(ot_square(1.5),crs=4326))
  expect_equal(nrow(search_ot_catalog(gap,100,r,function(...) stop("must not fetch"))),0)
  r$access_status <- "external"
  expect_equal(nrow(search_ot_catalog(aoi,100,r,function(...) stop("must not fetch"))),0)
})

test_that("all intersecting OT campaigns are queried and partial results fail", {
  f <- ot_fixture(); on.exit(unlink(f$root,recursive=TRUE))
  r <- ot_test_registry(f); r2 <- r; r2$dataset <- "Test2"
  r <- rbind(r,r2); seen <- character()
  reader <- function(record) {seen <<- c(seen,record$dataset); f$path}
  out <- search_ot_catalog(read_aoi(f$tiles),100,r,reader)
  expect_equal(seen,c("Test","Test2"))
  expect_equal(nrow(out),2) # Only identical file URLs deduplicate.
  expect_error(search_ot_catalog(read_aoi(f$tiles),1,r,reader),"max_items")
})

test_that("missing links and non-OT asset hosts cannot be advertised as downloadable", {
  for (bad in c(NA_character_,"https://example.org/b.laz")) {
    f <- ot_fixture(c("https://opentopography.s3.sdsc.edu/pc-bulk/Test/a.laz",bad))
    r <- ot_test_registry(f)
    expect_error(search_ot_catalog(read_aoi(f$tiles),100,r,function(...) f$path),"Unverified")
    unlink(f$root,recursive=TRUE)
  }
})

test_that("index cache is hash-verified and restricted to the official host", {
  f <- ot_fixture(); on.exit(unlink(f$root,recursive=TRUE))
  r <- ot_test_registry(f)
  r$index_url <- "https://example.org/index.zip"
  expect_error(ot_index_file(r),"Unexpected")
  r$dataset <- "../escape"
  expect_error(ot_index_file(r),"identifier")
  r <- ot_test_registry(f)
  cache <- file.path(tempdir(),"als-opentopography-indexes"); dir.create(cache,showWarnings=FALSE)
  dest <- file.path(cache,paste0("Test-",r$index_sha256,".zip"))
  file.copy(f$path,dest,overwrite=TRUE); on.exit(unlink(dest),add=TRUE)
  expect_equal(ot_index_file(r),dest)
})

test_that("default R API searches OpenTopography without a local folder", {
  f <- ot_fixture(); on.exit(unlink(f$root,recursive=TRUE))
  local_mocked_bindings(ot_registry=function() ot_test_registry(f),
    ot_index_file=function(...) f$path,.package="alsdownloader")
  out <- find_tiles(read_aoi(f$tiles),provider="opentopography")
  expect_equal(nrow(out),2)
  expect_true(all(out$provider=="opentopography"))
})

test_that("detailed map coverage clips original footprints without bridging gaps", {
  f <- ot_fixture(); on.exit(unlink(f$root,recursive=TRUE))
  r <- ot_test_registry(f)
  r$west <- 0;r$east <- 4;r$south <- 0;r$north <- 1
  view <- ot_visible_coverage(r,list(west=.2,south=.2,east=3.8,north=.8))
  expect_equal(nrow(view),1)
  gap <- sf::st_sfc(sf::st_point(c(2,.5)),crs=4326)
  expect_equal(lengths(sf::st_intersects(view,gap)),0L)
  expect_equal(nrow(ot_visible_coverage(r,list(west=10,south=10,east=11,north=11))),0)
  expect_equal(nrow(ot_visible_coverage(r,list(west=NA_real_,south=0,east=1,north=1))),0)
})

test_that("packaged OT access claims agree with the complete object audit", {
  registry <- ot_registry()
  expect_gt(nrow(registry),400L)
  ready <- registry[registry$access_status=="ready",,drop=FALSE]
  external <- registry[registry$access_status=="external",,drop=FALSE]
  expect_true(all(ready$platform=="Airborne Lidar"))
  expect_true(all(grepl("^[0-9a-f]{64}$",ready$index_sha256)))
  expect_true(all(ready$verified_objects>0 & ready$tile_count>=ready$verified_objects))
  expect_silent(require_data_terms(ready))
  proof <- jsonlite::fromJSON(system.file("extdata","opentopography-verification.json",package="alsdownloader"))$collections
  checks <- proof[match(ready$dataset,proof$dataset),]
  expect_false(anyNA(checks$dataset))
  expect_true(all(checks$missing_count==0 & checks$sample$las_signature))
  expect_equal(checks$url_set_sha256,ready$url_set_sha256)
  expect_true(all(c("IA14_Kumar_1064","IL14_Kumar_1064","CA14_Dietrich_G") %in% external$dataset))
  expect_false(any(external$dataset %in% ready$dataset))
  # These native projected unions retain every tile without global S2 rebuilding.
  expect_equal(sf::st_crs(registry)$epsg,3857)
  expect_false(any(sf::st_is_empty(registry)))
})

test_that("incremental snapshots require completion and matching checksums", {
  root <- tempfile();dir.create(root);dir.create(file.path(root,"catalog"))
  on.exit(unlink(root,recursive=TRUE))
  expect_error(ot_snapshot_path(root),"incomplete")
  files <- c("opentopography-registry.rds","opentopography-access-audit.csv",
             "opentopography-verification.json","opentopography-update-state.json")
  for(name in files)writeLines("fixture",file.path(root,"catalog",name))
  hashes <- setNames(lapply(files,function(name)digest::digest(file=file.path(root,"catalog",name),algo="sha256")),files)
  jsonlite::write_json(list(catalog="catalog",files=hashes),file.path(root,"complete.json"),auto_unbox=TRUE)
  expect_equal(ot_snapshot_path(root),file.path(root,"catalog",files[1]))
  writeLines("damaged",file.path(root,"catalog",files[2]))
  expect_error(ot_snapshot_path(root),"checksum mismatch")
})

test_that("compact catalogue bounds never replace original tile selection", {
  f <- ot_fixture();on.exit(unlink(f$root,recursive=TRUE))
  r <- ot_test_registry(f)
  sf::st_geometry(r) <- sf::st_as_sfc(sf::st_bbox(f$tiles))
  attr(r,"detail_resource") <- list(url="not requested by search")
  gap <- read_aoi(sf::st_sfc(ot_square(1.5),crs=4326))
  expect_equal(nrow(search_ot_catalog(gap,100,r,function(...)f$path)),0)
  tile <- read_aoi(sf::st_sfc(ot_square(3.1),crs=4326))
  expect_equal(nrow(search_ot_catalog(tile,100,r,function(...)f$path)),1)
})

test_that("detailed map resource is pinned and lazily loaded", {
  f <- ot_fixture();on.exit(unlink(f$root,recursive=TRUE))
  full <- ot_test_registry(f);path <- file.path(f$root,"full.rds");saveRDS(full,path)
  compact <- full
  attr(compact,"detail_resource") <- list(
    url="https://github.com/Cesarito2021/als_downloader/releases/download/catalog-test/opentopography-registry.rds",
    sha256=digest::digest(file=path,algo="sha256"),size_bytes=file.info(path)$size)
  calls <- 0L
  fetch <- function(url,dest,size){calls <<- calls+1L;file.copy(path,dest)}
  expect_identical(ot_detail_registry(full,function(...)stop("must not fetch")),full)
  expect_identical(ot_detail_registry(compact,fetch),full)
  expect_identical(ot_detail_registry(compact,fetch),full)
  expect_equal(calls,1L)
  attr(compact,"detail_resource")$sha256 <- paste(rep("0",64),collapse="")
  expect_error(ot_detail_registry(compact,fetch),"checksum")
})

test_that("a failed snapshot cannot replace the last working runtime catalogue", {
  old <- Sys.getenv("ALS_OT_CATALOG_SNAPSHOT",unset=NA_character_)
  on.exit({if(is.na(old))Sys.unsetenv("ALS_OT_CATALOG_SNAPSHOT")else Sys.setenv(ALS_OT_CATALOG_SNAPSHOT=old);ot_registry()},add=TRUE)
  Sys.unsetenv("ALS_OT_CATALOG_SNAPSHOT")
  before <- ot_registry()
  Sys.setenv(ALS_OT_CATALOG_SNAPSHOT=tempfile())
  expect_warning(after <- ot_registry(),"Keeping the last valid catalogue")
  expect_identical(after,before)
  expect_silent(expect_identical(ot_registry(),before))
})

test_that("compressed packaged access audit exports the original CSV bytes", {
  root<-tempfile();dir.create(root);on.exit(unlink(root,recursive=TRUE))
  payload<-charToRaw('dataset,status\r\nExample,ready\r\n')
  connection<-gzfile(file.path(root,'opentopography-access-audit.csv.gz'),'wb')
  writeBin(payload,connection);close(connection)
  dest<-file.path(root,'export.csv');ot_export_audit(dest,root)
  expect_identical(readBin(dest,'raw',n=1000),payload)
})

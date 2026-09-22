test_that("USGS metadata discovery avoids large spatial inventories", {
  prefixes <- character()
  root <- "StagedProducts/Elevation/metadata/test/"
  local_mocked_bindings(usgs_metadata_listing = function(prefix, delimiter = "/") {
    prefixes <<- c(prefixes, prefix)
    if (prefix == root) return(list(keys=character(), folders=paste0(root,c("reports/","spatial_metadata/"))))
    if (prefix == paste0(root,"reports/")) return(list(keys=character(),folders=paste0(prefix,c("vendor_provided_xml/","Additional Reports/"))))
    list(keys=paste0(prefix,c("ClassifiedPointCloud.xml","DEM.xml")),folders=character())
  }, .package="ALSdownloadeR")
  result <- usgs_project_xml("https://rockyweb.usgs.gov/Projects/test/LAZ/t.laz")
  expect_match(result,"ClassifiedPointCloud.xml",fixed=TRUE)
  expect_length(result,1L)
  expect_false(any(grepl("spatial_metadata|Additional Reports",prefixes)))
})

usgs_test_xml <- function(title, year=2021L) paste0('<metadata><idinfo><citation><citeinfo><title>',title,
  '</title></citeinfo></citation><timeperd><current>ground condition</current><timeinfo><rngdates><begdate>',
  year,'0307</begdate><enddate>',year,'0413</enddate></rngdates></timeinfo></timeperd></idinfo></metadata>')

test_that("documented acquisition overrides project names and cached dates survive budget", {
  xml_calls <- 0L
  local_mocked_bindings(usgs_project_xml=function(...) "https://example.org/cloud.xml",
    als_metadata_text=function(...) {xml_calls <<- xml_calls+1L;usgs_test_xml("OH 2020; Classified Point Cloud")},
    .package="ALSdownloadeR")
  cache<-new.env(parent=emptyenv())
  asset<-"https://rockyweb.usgs.gov/Projects/OH_2020/LAZ/t.laz"
  x<-usgs_asset_period(asset,NA_character_,cache,Sys.time()+10)
  expect_equal(x$year,2021L);expect_equal(x$end,"2021-04-13")
  y<-usgs_asset_period(sub("t.laz","u.laz",asset,fixed=TRUE),NA_character_,cache,Sys.time()-1)
  expect_equal(y$year,2021L);expect_equal(xml_calls,1L)
})

test_that("legacy metadata cannot assign one tile's dates to its neighbours", {
  xml<-usgs_test_xml("S1305405.LAS",2007)
  expect_equal(usgs_document_period(xml,"https://example.org/S1305405.laz")$year,2007L)
  expect_null(usgs_document_period(xml,"https://example.org/S1305406.laz"))
  expect_null(usgs_document_period(usgs_test_xml("DEM"),"https://example.org/t.laz"))
  local_mocked_bindings(usgs_project_xml=function(...) NA_character_,
    als_metadata_text=function(...) stop("HTML landing page must not be fetched"),.package="ALSdownloadeR")
  expect_null(usgs_asset_period("https://example.org/t.laz","https://example.org/index.html?prefix=metadata/",
    new.env(parent=emptyenv()),Sys.time()+10))
})

test_that("conflicting acquisition years remain unresolved", {
  local_mocked_bindings(usgs_project_xml=function(...) c("https://example.org/2020.xml","https://example.org/2021.xml"),
    als_metadata_text=function(url) usgs_test_xml("Classified Point Cloud",if(grepl("2020",url))2020L else 2021L),
    .package="ALSdownloadeR")
  x<-usgs_asset_period("https://example.org/t.laz",NA_character_,new.env(parent=emptyenv()),Sys.time()+10)
  expect_true(x$conflict)
})
test_that("an exhausted lookup budget is not reported as absent metadata", {
  local_mocked_bindings(usgs_project_xml=function(...) stop("must not query"),.package="ALSdownloadeR")
  x<-usgs_asset_period("https://example.org/t.laz",NA_character_,new.env(parent=emptyenv()),Sys.time()-1)
  expect_s3_class(x,"metadata_budget")
})
test_that("valid matching XML remains usable when an alternative is malformed", {
  local_mocked_bindings(usgs_project_xml=function(...) c("https://example.org/broken.xml","https://example.org/valid.xml"),
    als_metadata_text=function(url) if(grepl("broken",url)) "<metadata><broken&>" else usgs_test_xml("Classified Point Cloud",2019L),
    .package="ALSdownloadeR")
  x<-usgs_asset_period("https://example.org/t.laz",NA_character_,new.env(parent=emptyenv()),Sys.time()+10)
  expect_equal(x$year,2019L)
  expect_equal(x$source,"https://example.org/valid.xml")
})

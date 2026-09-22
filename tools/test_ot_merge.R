# Offline integration test: Rscript tools/test_ot_merge.R (run from package root).
stopifnot(requireNamespace('sf',quietly=TRUE),requireNamespace('jsonlite',quietly=TRUE))
root <- tempfile('ot-merge-');dir.create(root)
baseline <- file.path(root,'baseline');dir.create(baseline)
square <- sf::st_polygon(list(matrix(c(0,0,1,0,1,1,0,1,0,0),ncol=2,byrow=TRUE)))
x <- sf::st_sf(dataset=c('Keep','Change'),title=c('Original','Old'),platform='Airborne Lidar',
 access_status='ready',access_reason='Verified',tile_count=1L,verified_objects=1L,
 info_url='https://example.org',doi='https://doi.org/example',
 index_url='https://opentopography.s3.sdsc.edu/pc-bulk/test/test_TileIndex.zip',
 index_portal_url='',index_sha256=paste(rep('a',64),collapse=''),url_set_sha256='set',
 license_url='https://opentopography.org/usageterms',license_source='Terms',citation='Producer',
 reviewed_on='2020-01-01',acquired_start='1998-01-01',acquired_end='1998-12-31',
 west=0,south=0,east=1,north=1,map_lon=.5,map_lat=.5,
 geometry=sf::st_sfc(square,square,crs=3857))
saveRDS(x,file.path(baseline,'opentopography-registry.rds'))
write.csv(sf::st_drop_geometry(x),file.path(baseline,'opentopography-access-audit.csv'),row.names=FALSE)
jsonlite::write_json(list(reviewed_on='2020-01-01',collections=list(
 list(dataset='Keep',missing_count=0L),list(dataset='Change',missing_count=0L))),
 file.path(baseline,'opentopography-verification.json'),auto_unbox=TRUE)
run_script <- function(name,args) {
 env <- new.env();env$commandArgs <- function(...)args
 sys.source(file.path('tools',name),envir=env)
}
work <- file.path(root,'run');dir.create(work);dir.create(file.path(work,'catalog'))
delta <- file.path(work,'delta-catalog');dir.create(delta)
y <- x[2,,drop=FALSE];y$title <- 'Corrected';y$reviewed_on <- '2026-09-20'
saveRDS(y,file.path(delta,'opentopography-registry.rds'))
write.csv(sf::st_drop_geometry(y),file.path(delta,'opentopography-access-audit.csv'),row.names=FALSE)
jsonlite::write_json(list(reviewed_on='2026-09-20',collections=list(list(dataset='Change',missing_count=0L))),
 file.path(delta,'opentopography-verification.json'),auto_unbox=TRUE)
plan <- list(changes=list(list(dataset='Change',previous_dataset='Change')),unchanged=list('Keep'),missing=list())
jsonlite::write_json(plan,file.path(work,'update-plan.json'),auto_unbox=TRUE)
writeLines('{}',file.path(work,'pointcloud-catalog.json'))
run_script('merge_ot_update.R',c(baseline,work))
merged <- readRDS(file.path(work,'catalog','opentopography-registry.rds'))
stopifnot(inherits(sf::st_geometry(merged),'sfc'),identical(sf::st_crs(merged),sf::st_crs(x)),
 identical(sf::st_geometry(merged)[[1]],sf::st_geometry(x)[[1]]),
 identical(merged$reviewed_on,c('2020-01-01','2026-09-20')),
 identical(merged$title,c('Original','Corrected')))
proof <- jsonlite::fromJSON(file.path(work,'catalog','opentopography-verification.json'),simplifyVector=FALSE)
stopifnot(identical(proof$collections[[1]]$verified_on,'2020-01-01'))
plan$missing <- list(list(dataset='Keep'))
jsonlite::write_json(plan,file.path(work,'update-plan.json'),auto_unbox=TRUE)
run_script('merge_ot_update.R',c(baseline,work))
merged <- readRDS(file.path(work,'catalog','opentopography-registry.rds'))
stopifnot(nrow(merged)==2L,merged$access_status[1]=='external',merged$reviewed_on[1]=='2020-01-01')

# New photogrammetry requires no tile scan or geometry generation. The builder
# must support an empty spatial delta while retaining the exclusion decision.
audit <- file.path(root,'photo');dir.create(audit);dir.create(file.path(audit,'objects-verified'))
metadata <- list(list(dataset='Photo',title='Photo',platform='Structure from Motion / Photogrammetry',
 zip_valid=FALSE,info_url='https://example.org',doi='https://doi.org/example',
 index_url='https://example.org/index',license_url='https://opentopography.org/usageterms',
 license_source='Terms',citation='Producer',temporal='1998-01-01'))
jsonlite::write_json(metadata,file.path(audit,'dataset-audit-final.json'),auto_unbox=TRUE)
jsonlite::write_json(list(Datasets=list()),file.path(audit,'pointcloud-catalog.json'),auto_unbox=TRUE)
writeLines('[]',file.path(audit,'objects-verified','summary.json'))
write.csv(data.frame(dataset=character(),tiles=integer(),missing=integer(),crs=logical(),error=character()),file.path(audit,'index-summary-current.csv'),row.names=FALSE)
write.csv(data.frame(dataset=character(),url=character()),file.path(audit,'tile-links-current.csv'),row.names=FALSE)
run_script('build_ot_registry.R',c(audit,file.path(root,'photo-result'),baseline))
photo <- read.csv(file.path(root,'photo-result','opentopography-access-audit.csv'))
stopifnot(photo$access_status=='out_of_scope',nrow(readRDS(file.path(root,'photo-result','opentopography-registry.rds')))==0L)
stopifnot(startsWith(normalizePath(root,winslash='/'),paste0(normalizePath(tempdir(),winslash='/'),'/')))
unlink(root,recursive=TRUE)
cat('PASS: incremental geometry, dates, missing records and non-ALS exclusion.\n')

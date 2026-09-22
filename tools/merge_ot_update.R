# Merge a verified delta without recomputing existing footprints or review dates.
args <- commandArgs(trailingOnly=TRUE); stopifnot(length(args)==2L)
stopifnot(requireNamespace('sf',quietly=TRUE))
baseline <- args[1]; root <- args[2]; output <- file.path(root,'catalog')
plan <- jsonlite::fromJSON(file.path(root,'update-plan.json'),simplifyVector=FALSE)
read_table <- function(folder)read.csv(file.path(folder,'opentopography-access-audit.csv'),stringsAsFactors=FALSE,check.names=FALSE)
old_table <- read_table(baseline)
old_registry <- readRDS(file.path(baseline,'opentopography-registry.rds'))
old_proof <- jsonlite::fromJSON(file.path(baseline,'opentopography-verification.json'),simplifyVector=FALSE)
drop <- unique(unlist(lapply(plan$changes,function(x)c(x$dataset,x$previous_dataset))))
table <- old_table[!old_table$dataset%in%drop,,drop=FALSE]
registry <- old_registry[!old_registry$dataset%in%drop,,drop=FALSE]
collections <- Filter(function(x)!x$dataset%in%drop,old_proof$collections)
for(i in seq_along(collections)) {
 if(is.null(collections[[i]]$verified_on))collections[[i]]$verified_on <- old_proof$reviewed_on
}
bind_columns <- function(a,b) {
 for(n in setdiff(names(a),names(b)))b[[n]] <- rep(NA,nrow(b))
 for(n in setdiff(names(b),names(a)))a[[n]] <- rep(NA,nrow(a))
 rbind(a,b[,names(a),drop=FALSE])
}
if(length(plan$changes)) {
 delta <- file.path(root,'delta-catalog')
 table <- bind_columns(table,read_table(delta))
 registry <- bind_columns(registry,readRDS(file.path(delta,'opentopography-registry.rds')))
 proof <- jsonlite::fromJSON(file.path(delta,'opentopography-verification.json'),simplifyVector=FALSE)
 for(i in seq_along(proof$collections))proof$collections[[i]]$verified_on <- proof$reviewed_on
 collections <- c(collections,proof$collections)
}
missing <- vapply(plan$missing,`[[`,character(1),'dataset')
mark_presence <- function(x) {
 x$catalog_presence <- ifelse(x$dataset%in%missing,'missing; retained for review','present')
 absent <- x$dataset%in%missing
 x$access_status[absent & x$access_status=='ready'] <- 'external'
 x$access_reason[absent] <- paste('Absent from latest provider catalogue; previous record retained.',x$access_reason[absent])
 x
}
table <- mark_presence(table);registry <- mark_presence(registry)
kept <- old_registry[!old_registry$dataset%in%c(drop,missing),,drop=FALSE]
after <- registry[match(kept$dataset,registry$dataset),,drop=FALSE]
for(name in names(kept))stopifnot(identical(kept[[name]],after[[name]]))
stopifnot(!anyDuplicated(registry$dataset),
 all(registry$dataset%in%table$dataset),all(registry$platform=='Airborne Lidar'))
stopifnot(inherits(sf::st_geometry(registry),'sfc'),
 identical(sf::st_crs(registry),sf::st_crs(old_registry)),!any(sf::st_is_empty(registry)))
ready <- registry$access_status=='ready'
stopifnot(all(nzchar(registry$index_sha256[ready])),all(nzchar(registry$license_url[ready])),
 all(nzchar(registry$citation[ready])))
connection <- xzfile(file.path(output,'opentopography-registry.rds'),'wb',compression=1)
saveRDS(registry,connection);close(connection)
write.csv(table,file.path(output,'opentopography-access-audit.csv'),row.names=FALSE,na='',fileEncoding='UTF-8')
proof <- list(reviewed_on=as.character(Sys.Date()),update_mode='incremental',
 baseline_catalog_sha256=digest::digest(file=file.path(baseline,'opentopography-registry.rds'),algo='sha256'),
 catalog_sha256=digest::digest(file=file.path(root,'pointcloud-catalog.json'),algo='sha256'),
 catalog_endpoint='https://portal.opentopography.org/API/otCatalog',include_federated=FALSE,
 productFormat='PointCloud',changed_collections=length(plan$changes),
 unchanged_collections=length(plan$unchanged),missing_collections=length(plan$missing),
 airborne_records=nrow(registry),ready_airborne_records=sum(ready),
 external_airborne_records=sum(!ready),ready_tile_rows=sum(registry$tile_count[ready]),
 scope=paste('Incremental snapshot. Unchanged records retain their original verification dates.',
 'Only selected collections were checked in this run; no fresh availability claim for all old URLs.',
 'See collections[].verified_on and the access-audit reviewed_on column.'),collections=collections)
# Keep previous live tests explicitly historical; do not claim they ran again.
if(!is.null(old_proof$live_download_tests)) {
 proof$historical_live_download_tests <- old_proof$live_download_tests
} else if(!is.null(old_proof$historical_live_download_tests)) {
 proof$historical_live_download_tests <- old_proof$historical_live_download_tests
}
jsonlite::write_json(proof,file.path(output,'opentopography-verification.json'),auto_unbox=TRUE,pretty=TRUE,null='null')
cat('Merged',length(plan$changes),'selected collections;',sum(ready),'ready collections.\n')

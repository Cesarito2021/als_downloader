# Maintenance-only: Rscript tools/build_ot_registry.R AUDIT_DIR OUTPUT_DIR
# Run after refresh_ot_metadata.py, scan_ot_indexes.R and audit_ot_objects.py.
args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)%in%c(2L,3L))
root <- args[1]; output <- args[2]
metadata <- jsonlite::fromJSON(file.path(root,'dataset-audit-final.json'),simplifyVector=FALSE)
catalog <- jsonlite::fromJSON(file.path(root,'pointcloud-catalog.json'),simplifyVector=FALSE)$Datasets
summary <- read.csv(file.path(root,'index-summary-current.csv'),stringsAsFactors=FALSE)
audits <- jsonlite::fromJSON(file.path(root,'objects-verified','summary.json'),simplifyVector=FALSE)
names(audits) <- vapply(audits,`[[`,character(1),'dataset')
context_audits <- audits
if(length(args)==3L) {
 old_proof <- jsonlite::fromJSON(file.path(args[3],'opentopography-verification.json'),simplifyVector=FALSE)
 previous <- old_proof$collections
 names(previous) <- vapply(previous,`[[`,character(1),'dataset')
 context_audits <- c(audits,previous[!names(previous)%in%names(audits)])
}
stopifnot(length(audits)==sum(vapply(metadata,function(x)isTRUE(x$zip_valid),logical(1))))
reviewed <- as.character(Sys.Date()); registry <- list(); report <- list()
get <- function(x,key,fallback='') if(is.null(x[[key]])) fallback else x[[key]]
for (x in metadata) {
 cat('Registry:',x$dataset,'\n')
 name <- x$dataset; audit <- audits[[name]]; index <- summary[summary$dataset==name,,drop=FALSE]
 linked_dataset <- if(!is.null(audit$sample$url)) sub('/.*$','',sub('^https://opentopography[.]s3[.]sdsc[.]edu/pc-bulk/','',audit$sample$url)) else name
 linked_audit <- context_audits[[linked_dataset]]
 shared_alias <- !identical(linked_dataset,name) && !is.null(linked_audit) &&
   !is.null(audit$url_set_sha256) && identical(audit$url_set_sha256,linked_audit$url_set_sha256)
 terms_ready <- grepl('^https://[^[:space:]]+$',get(x,'license_url')) && nzchar(trimws(get(x,'citation')))
 ready <- identical(x$platform,'Airborne Lidar') && isTRUE(x$zip_valid) &&
   !shared_alias && terms_ready &&
   is.null(x$refresh_error) && is.null(x$metadata_error) &&
   !is.null(audit) && is.null(audit$error) && identical(audit$missing_count,0L) &&
   isTRUE(audit$sample$las_signature) && nrow(index)==1L && index$missing==0L &&
   !index$crs && (is.na(index$error) || !nzchar(index$error))
 reason <- if(ready) 'All indexed file URLs matched positive-sized public objects; one LAS header checked per collection.' else
   if(x$platform!='Airborne Lidar') paste('Outside airborne-LiDAR scope:',x$platform) else
   if(shared_alias) paste('Index repeats every file from',linked_dataset,'under different collection metadata. Use the source portal to resolve acquisition/channel identity; not advertised as an independent downloadable survey.') else
   if(!terms_ready) 'Dataset license or attribution unresolved; use the source portal.' else
   if(!isTRUE(x$zip_valid)) paste('Public TileIndex unavailable:',get(x,'error','no valid ZIP'),'- use the provider portal.') else
   paste('Tile access verification incomplete:',get(audit,'error',paste(get(audit,'missing_count','unknown'),'missing links')))
 status <- if(ready) 'ready' else if(x$platform=='Airborne Lidar') 'external' else 'out_of_scope'
 tile_count <- if(nrow(index)) index$tiles else 0L
 row <- data.frame(dataset=name,title=x$title,platform=x$platform,access_status=status,
   access_reason=reason,tile_count=tile_count,verified_objects=as.integer(get(audit,'found',0L)),
   info_url=x$info_url,doi=x$doi,index_url=x$index_url,index_portal_url=get(x,'index_portal_url'),index_sha256=get(x,'index_sha256'),
   url_set_sha256=get(audit,'url_set_sha256'),license_url=x$license_url,license_source=x$license_source,
   citation=x$citation,reviewed_on=reviewed,stringsAsFactors=FALSE)
 dates <- trimws(strsplit(get(x,'temporal'),'/')[[1]])
 period <- rep(NA_character_,2)
 if(length(dates)>0L && length(dates)<=2L && all(grepl('^[0-9]{4}-[0-9]{2}-[0-9]{2}$',dates))) {
   valid <- !is.na(as.Date(dates,format='%Y-%m-%d'))
   if(all(valid)) period <- c(dates[1],dates[length(dates)])
 }
 if(!anyNA(period) && period[1]>period[2])period[] <- NA_character_
 row$acquired_start <- period[1];row$acquired_end <- period[2]
 report[[length(report)+1L]] <- row
 if(x$platform!='Airborne Lidar')next
 if(ready) {
   path <- file.path(root,'indexes',paste0(name,'_TileIndex.zip'))
   stopifnot(identical(digest::digest(file=path,algo='sha256'),x$index_sha256))
   geom <- readRDS(file.path(root,'footprints',paste0(name,'.rds')))
 } else {
   item <- Filter(function(y)identical(y$Dataset$alternateName,name),catalog)[[1]]$Dataset
   geom <- sf::st_geometry(sf::st_read(jsonlite::toJSON(item$spatialCoverage$geo$geojson,auto_unbox=TRUE),quiet=TRUE))
 }
 # Keep large dissolved footprints in a projected CRS. Adjacent tile edges
 # are straight segments, not great-circle arcs; spherical cross-ring
 # validation is both inappropriate for these unions and prohibitively slow.
 geom <- sf::st_transform(geom,3857)
 if(!all(sf::st_is_valid(geom)))geom <- sf::st_make_valid(geom)
 geom <- suppressWarnings(sf::st_collection_extract(geom,'POLYGON'))
 if(length(geom)>1L)geom <- sf::st_union(geom)
 stopifnot(length(geom)==1L,!sf::st_is_empty(geom),all(sf::st_is_valid(geom)))
 bb <- sf::st_bbox(sf::st_transform(geom,4326))
 row$west <- bb[['xmin']]; row$south <- bb[['ymin']];row$east <- bb[['xmax']];row$north <- bb[['ymax']]
 centre <- suppressWarnings(sf::st_coordinates(sf::st_point_on_surface(sf::st_transform(geom,3857))))[1,1:2]
 point <- sf::st_transform(sf::st_sfc(sf::st_point(centre),crs=3857),4326)
 row$map_lon <- sf::st_coordinates(point)[1,1];row$map_lat <- sf::st_coordinates(point)[1,2]
 registry[[length(registry)+1L]] <- sf::st_sf(row,geometry=geom)
}
r <- if(length(registry))do.call(rbind,registry)else if(length(args)==3L)readRDS(file.path(args[3],'opentopography-registry.rds'))[0,]else sf::st_sf(geometry=sf::st_sfc(crs=3857))
table <- if(length(report))do.call(rbind,report)else if(length(args)==3L)read.csv(file.path(args[3],'opentopography-access-audit.csv'),stringsAsFactors=FALSE)[0,]else stop('Empty audit requires a baseline schema')
# Community Dataspace deposits are a different access route, not hosted tile indexes.
for (record in catalog) {
 d <- record$Dataset
 if(!is.null(d$alternateName))next
 row <- table[NA_integer_,,drop=FALSE]; row[1,] <- NA
 row$dataset <- d$identifier$value; row$title <- d$name;row$platform <- 'Not reviewed'
 row$access_status <- 'external';row$access_reason <- 'Community Dataspace deposit; no hosted TileIndex adapter. Review acquisition type and files at the source.'
 row$info_url <- d$url;row$doi <- d$url;row$reviewed_on <- reviewed
 table <- rbind(table,row)
}
dir.create(output,recursive=TRUE,showWarnings=FALSE)
connection <- xzfile(file.path(output,'opentopography-registry.rds'),'wb',compression=1)
saveRDS(r,connection);close(connection)
write.csv(table,file.path(output,'opentopography-access-audit.csv'),row.names=FALSE,na='',fileEncoding='UTF-8')
ready <- r$access_status=='ready'
links <- read.csv(file.path(root,'tile-links-current.csv'),stringsAsFactors=FALSE)
proof <- list(reviewed_on=reviewed,catalog_endpoint='https://portal.opentopography.org/API/otCatalog',
 include_federated=FALSE,productFormat='PointCloud',
 catalog_sha256=digest::digest(file=file.path(root,'pointcloud-catalog.json'),algo='sha256'),
 hosted_records=length(metadata),airborne_records=nrow(r),ready_airborne_records=sum(ready),
 external_airborne_records=sum(!ready),ready_tile_rows=sum(r$tile_count[ready]),
 ready_dataset_object_references=sum(r$verified_objects[ready]),
 ready_distinct_file_urls=length(unique(links$url[links$dataset %in% r$dataset[ready]])),
 scope='Selected airborne-LiDAR TileIndex URLs checked against positive-sized public objects; one anonymous LAS-header sample per readable collection. Not full decompression of every file.',
 collections=unname(audits))
live <- file.path(root,'live-download-tests.json')
if(file.exists(live))proof$live_download_tests <- jsonlite::fromJSON(live,simplifyVector=FALSE)
jsonlite::write_json(proof,file.path(output,'opentopography-verification.json'),auto_unbox=TRUE,pretty=TRUE,null='null')
cat('Airborne registry:',nrow(r),'ready:',sum(r$access_status=='ready'),'external:',sum(r$access_status=='external'),'\n')
cat('Verified tile rows:',sum(r$tile_count[r$access_status=='ready']),'objects:',sum(r$verified_objects[r$access_status=='ready']),'audit records:',nrow(table),'\n')

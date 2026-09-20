# Keep exact, immutable detailed coverage in a release asset; bundle only search
# bounds and survey locations. Never draw those bounds as red tile footprints.
args <- commandArgs(trailingOnly=TRUE);stopifnot(length(args)==3L)
source <- file.path(args[1],'opentopography-registry.rds');output <- args[2];url <- args[3]
x <- readRDS(source)
for(i in which(x$access_status=='ready')) {
 box <- sf::st_as_sfc(sf::st_bbox(c(xmin=x$west[i],ymin=x$south[i],xmax=x$east[i],ymax=x$north[i]),crs=4326))
 sf::st_geometry(x)[i] <- sf::st_geometry(sf::st_transform(box,3857))
}
attr(x,'detail_resource') <- list(url=url,sha256=digest::digest(file=source,algo='sha256'),
 size_bytes=unname(file.info(source)$size))
dir.create(output,recursive=TRUE,showWarnings=FALSE)
saveRDS(x,file.path(output,'opentopography-registry.rds'),compress='xz')
aux <- c('opentopography-access-audit.csv','opentopography-verification.json','opentopography-update-state.json')
for(name in aux)if(normalizePath(file.path(args[1],name),winslash='/',mustWork=FALSE)!=
                  normalizePath(file.path(output,name),winslash='/',mustWork=FALSE))
 stopifnot(file.copy(file.path(args[1],name),file.path(output,name),overwrite=TRUE))
state_path <- file.path(output,'opentopography-update-state.json')
state <- jsonlite::fromJSON(state_path,simplifyVector=FALSE)
state$files[['opentopography-registry.rds']] <- digest::digest(file=file.path(output,'opentopography-registry.rds'),algo='sha256')
jsonlite::write_json(state,state_path,auto_unbox=TRUE,pretty=TRUE,null='null')
input <- file(file.path(output,'opentopography-access-audit.csv'),'rb')
compressed <- gzfile(file.path(output,'opentopography-access-audit.csv.gz'),'wb')
repeat {chunk<-readBin(input,'raw',n=65536L);if(!length(chunk))break;writeBin(chunk,compressed)}
close(input);close(compressed)
cat('Packaged bounds:',file.info(file.path(output,'opentopography-registry.rds'))$size,'bytes; exact coverage remains in pinned asset.\n')

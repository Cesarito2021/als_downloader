# Maintenance-only: Rscript tools/scan_ot_indexes.R AUDIT_DIR
args <- commandArgs(trailingOnly=TRUE); stopifnot(length(args)==1L)
root <- args[1]
metadata <- jsonlite::fromJSON(file.path(root,'dataset-audit-final.json'),simplifyVector=FALSE)
metadata <- Filter(function(x)isTRUE(x$zip_valid),metadata)
dir.create(file.path(root,'footprints'),showWarnings=FALSE)
rows <- list(); links <- list()
for(i in seq_along(metadata)) {
 record <- metadata[[i]]; name <- record$dataset
 path <- file.path(root,'indexes',paste0(name,'_TileIndex.zip'))
 tryCatch({
  hash <- digest::digest(file=path,algo='sha256'); stopifnot(identical(hash,record$index_sha256))
  members <- utils::unzip(path,list=TRUE)
  if(sum(members$Length)>500*1024^2 || any(grepl('(^/|^[A-Za-z]:|(^|/)\\.\\.(/|$))',gsub('\\\\','/',members$Name))))stop('Unsafe index archive')
  x <- sf::st_read(paste0('/vsizip/',normalizePath(path,winslash='/')),quiet=TRUE); names(x) <- tolower(names(x))
  url <- if('url'%in%names(x))as.character(x$url)else rep(NA_character_,nrow(x))
  valid <- !is.na(url)&grepl('^https://opentopography[.]s3[.]sdsc[.]edu/pc-bulk/.+[.](las|laz)$',url,ignore.case=TRUE)
  rows[[i]] <- data.frame(dataset=name,tiles=nrow(x),missing=sum(!valid),crs=is.na(sf::st_crs(x)),error='')
  links[[i]] <- data.frame(dataset=name,url=url)
  if(!all(valid) || is.na(sf::st_crs(x)))next
  dest <- file.path(root,'footprints',paste0(name,'.rds')); stamp <- paste0(dest,'.sha256')
  if(!file.exists(dest) || !file.exists(stamp) || !identical(readLines(stamp,warn=FALSE),hash)) {
   g <- sf::st_zm(sf::st_geometry(x),drop=TRUE,what='ZM')
   if(!all(sf::st_geometry_type(g)%in%c('POLYGON','MULTIPOLYGON')))stop('Invalid geometry type')
   g <- sf::st_transform(g,3857)
   g <- sf::st_make_valid(sf::st_union(sf::st_make_valid(g)))
   saveRDS(g,dest,compress='xz');writeLines(hash,stamp)
  }
 },error=function(e)rows[[i]]<<-data.frame(dataset=name,tiles=0,missing=NA,crs=NA,error=conditionMessage(e)))
 if(i%%25==0)cat('Scanned',i,'/',length(metadata),'\n')
}
write.csv(if(length(rows))do.call(rbind,rows)else data.frame(dataset=character(),tiles=integer(),missing=integer(),crs=logical(),error=character()),file.path(root,'index-summary-current.csv'),row.names=FALSE)
write.csv(if(length(links))do.call(rbind,links)else data.frame(dataset=character(),url=character()),file.path(root,'tile-links-current.csv'),row.names=FALSE)

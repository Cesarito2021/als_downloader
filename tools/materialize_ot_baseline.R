# Expand only the geometry resource before merging. This does not re-audit tiles.
args <- commandArgs(trailingOnly=TRUE);stopifnot(length(args)==2L)
source <- args[1];target <- args[2]
x <- readRDS(file.path(source,'opentopography-registry.rds'))
resource <- attr(x,'detail_resource')
if(is.null(resource))quit(status=0)
stopifnot(grepl('^https://github[.]com/Cesarito2021/als_downloader/releases/download/[^/]+/opentopography-registry[.]rds$',resource$url))
dir.create(target,recursive=TRUE,showWarnings=FALSE)
path <- file.path(target,'opentopography-registry.rds')
response <- httr::GET(resource$url,httr::write_disk(path),httr::timeout(180),
 httr::config(maxfilesize_large=resource$size_bytes))
stopifnot(httr::status_code(response)==200L,identical(digest::digest(file=path,algo='sha256'),resource$sha256))
full <- readRDS(path)
stopifnot(inherits(sf::st_geometry(full),'sfc'),identical(full$dataset,x$dataset))
files <- c('opentopography-access-audit.csv','opentopography-verification.json','opentopography-update-state.json')
stopifnot(all(file.copy(file.path(source,files),file.path(target,files))))
state <- jsonlite::fromJSON(file.path(target,files[3]),simplifyVector=FALSE)
state$files[['opentopography-registry.rds']] <- resource$sha256
jsonlite::write_json(state,file.path(target,files[3]),auto_unbox=TRUE,pretty=TRUE,null='null')

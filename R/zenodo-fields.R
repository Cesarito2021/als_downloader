zenodo_selected_years <- function(mode, count, values) {
  n<-if(identical(mode,"multiple"))as.integer(count) else 1L
  if(length(n)!=1L || is.na(n) || n<1L || n>30L)stop("Choose between 1 and 30 acquisition years.")
  years<-suppressWarnings(as.integer(values))
  if(length(years)!=n || anyNA(years) || any(years<1900L | years>as.integer(format(Sys.Date(),"%Y"))))stop("Choose each acquisition year.")
  if(anyDuplicated(years))stop("Choose distinct acquisition years.")
  paste(sort(years),collapse=", ")
}

zenodo_id_mapping <- function(boundary, column, keys, mapping=NULL) {
  g<-zenodo_boundary(boundary)
  if(length(column)!=1L || !column %in% names(sf::st_drop_geometry(g)))stop("Choose the column containing the point-cloud tile ID.")
  ids<-as.character(g[[column]])
  if(anyNA(ids) || any(!nzchar(trimws(ids))))stop("Every polygon needs a point-cloud tile ID.")
  allowed<-keys[grepl("\\.(las|laz|zip)$",keys,ignore.case=TRUE)]
  if(is.null(mapping)) {
    matches<-vapply(ids,function(id){
      if(id %in% allowed)return(id)
      candidate<-allowed[tools::file_path_sans_ext(basename(allowed))==id]
      if(length(candidate)==1L)candidate else ""
    },"")
  } else {
    if(is.null(names(mapping)) || anyDuplicated(names(mapping)) || !all(unique(ids) %in% names(mapping)))stop("Match every tile ID to a Zenodo file.")
    matches<-unname(as.character(mapping[ids]))
  }
  if(any(!matches %in% allowed))stop("Match every tile ID to a LAS, LAZ or ZIP file in this Zenodo record.")
  g$file_key<-matches
  g
}

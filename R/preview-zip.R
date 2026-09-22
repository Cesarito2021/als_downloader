read_zip_preview <- function(path, reader, max_uncompressed_bytes, member = NULL, progress = function(x) NULL) {
  progress("Inspecting ZIP contents...")
  entries <- tryCatch(utils::unzip(path,list=TRUE),error=function(e)stop("Invalid ZIP archive."))
  names <- gsub("\\\\","/",entries$Name)
  if(!nrow(entries)||nrow(entries)>10000L||anyNA(entries$Length)||any(entries$Length<0)||
     any(grepl("(^/|:|(^|/)\\.\\.(/|$))",names))||anyDuplicated(tolower(names)))
    stop("Unsafe or invalid ZIP contents.")
  if(sum(entries$Length)>max_uncompressed_bytes)
    stop("ZIP contents exceed the uncompressed size limit.")
  clouds <- which(grepl("\\.(las|laz)$",names,ignore.case=TRUE))
  if(!length(clouds))stop("ZIP contains no LAS/LAZ point cloud.")
  if(!is.null(member)&&(length(member)!=1L||is.na(member)))stop("Select one ZIP member.")
  if(is.null(member)||!nzchar(member)) {
    if(length(clouds)>1L)stop(structure(list(message="Select a LAS/LAZ file from the ZIP archive.",call=NULL,
      members=names[clouds]),class=c("als_zip_selection","error","condition")))
    member<-names[clouds]
  }
  if(length(member)!=1L||is.na(member)||!member %in% names[clouds])stop("Select a listed LAS/LAZ member of this ZIP.")
  i<-match(member,names);folder<-paste0(path,".contents")
  if(!dir.create(folder,showWarnings=FALSE))stop("Could not create temporary ZIP extraction directory.")
  on.exit(unlink(folder,recursive=TRUE),add=TRUE)
  progress(paste("Extracting",member,"..."))
  utils::unzip(path,files=entries$Name[i],exdir=folder)
  file<-file.path(folder,member)
  link<-Sys.readlink(file)
  if(any(!is.na(link)&nzchar(link)))stop("ZIP members must be regular point-cloud files.")
  if(!file.exists(file)||file.size(file)!=entries$Length[i]||!valid_las_header(file))
    stop("Extracted ZIP member is incomplete or is not a LAS/LAZ file.")
  progress(paste("Reading and sampling",member,"..."))
  result<-reader(file);attr(result,"archive_member")<-member;result
}

cleanup_preview_files <- function(path) {
  if(is.null(path))return(invisible(NULL))
  unlink(c(path,paste0(path,".status")))
  unlink(paste0(path,".contents"),recursive=TRUE)
}

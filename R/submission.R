source_request <- function(input) {
  value<-function(id)if(is.null(input[[id]]))"" else trimws(as.character(input[[id]])[1])
  fail<-function(message)list(valid=FALSE,message=message)
  origin<-value("source_origin");url<-value("source_url");licence<-value("source_license_url")
  platform<-value("source_platform");year<-value("source_year");email<-value("source_email")
  boundary<-value("source_boundary")
  if(any(!nzchar(c(origin,url,licence,platform))))return(fail("Add a public record link, data/index link, licence link and acquisition platform."))
  if(any(nchar(c(origin,url,licence,platform,year,email,boundary))>1000))return(fail("Keep each field under 1,000 characters."))
  if(grepl("zenodo\\.org|10\\.5281/zenodo",paste(origin,url),ignore.case=TRUE))return(fail("For Zenodo records, use the Zenodo dataset tab."))
  if(grepl("^10\\.[0-9]{4,9}/[^[:space:]]+$",origin))origin<-paste0("https://doi.org/",origin)
  public_url<-function(x){
    u<-tryCatch(httr::parse_url(x),error=function(e)NULL)
    !is.null(u)&&identical(u$scheme,"https")&&!is.null(u$hostname)&&nzchar(u$hostname)&&
      is.null(u$username)&&is.null(u$password)&&!grepl("[[:space:]]",x)
  }
  if(!all(vapply(c(origin,url,licence,if(nzchar(boundary))boundary),public_url,logical(1))))return(fail("Use public HTTPS links without credentials, or a DOI for the record."))
  hosts<-tolower(vapply(c(origin,url),function(x)httr::parse_url(x)$hostname,""))
  if(any(grepl("(^|\\.)(drive\\.google\\.com|docs\\.google\\.com|colab\\.research\\.google\\.com|dropbox\\.com|dropboxusercontent\\.com|onedrive\\.live\\.com|1drv\\.ms)$",hosts)) ||
     grepl("[?&](token|access_token|signature|sig|expires|x-amz-signature|x-goog-signature)=",url,ignore.case=TRUE))
    return(fail("Use a stable institutional or scientific repository link, not a personal drive, notebook or expiring download link."))
  if(nzchar(email)&&!grepl("^[^[:space:]@]+@[^[:space:]@]+\\.[^[:space:]@]+$",email))return(fail("Check the optional contact email."))
  if(nzchar(year)) {valid<-tryCatch({zenodo_dates(year);TRUE},error=function(e)FALSE);if(!valid)return(fail("Use an acquisition year or interval, or leave it blank if unknown."))}
  if(!platform %in% c("Aircraft / helicopter ALS","UAV LiDAR","Mixed aerial laser platforms"))return(fail("Select an aerial laser-scanning platform."))
  if(!isTRUE(input$source_open_license)||!isTRUE(input$source_repository_confirm))return(fail("Confirm the data licence and stable public repository."))
  labels<-c("Public record / DOI","Data or tile index","Polygon/index link","Acquisition","Platform","Licence","Scope","Optional private contact")
  values<-c(origin,url,boundary,if(nzchar(year))year else "Unknown",platform,licence,value("source_scope"),email)
  list(valid=TRUE,message="Ready to check public access. Hosting, licence and coverage remain subject to team review.",
    title=paste("ALS source proposal:",origin),body=paste(labels,values,sep=": ",collapse="\n\n"))
}

source_submission_ui <- function() shiny::modalDialog(
  title="Submit ALS data - Zenodo",size="l",easyClose=FALSE,
  zenodo_submission_ui(),footer=shiny::modalButton("Close"))

source_submission_server <- function(input,output,session,check) {
  shiny::observeEvent(input$suggest_source,shiny::showModal(source_submission_ui()))
}

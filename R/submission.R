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
  title="Contribute ALS data",size="l",easyClose=FALSE,
  shiny::tabsetPanel(shiny::tabPanel("Zenodo dataset",zenodo_submission_ui()),
    shiny::tabPanel("Other data source",
      shiny::p("Share a stable, public institutional or scientific repository. The ALS Downloader team reviews access, coverage and licensing before integration."),
      shiny::textInput("source_origin","1. Dataset record link or DOI *"),
      shiny::textInput("source_url","2. Direct LAS/LAZ or tile-index link *",placeholder="Public HTTPS; no login or expiring links"),
      shiny::textInput("source_boundary","Coverage polygons / index link (if separate)",placeholder="GeoJSON, GeoPackage or zipped Shapefile; polygons linked to files"),
      shiny::textInput("source_year","3. Acquisition year or interval (optional)",placeholder="2019 or 2018-2020; leave blank if unknown"),
      shiny::selectInput("source_platform","4. Acquisition platform *",c("Choose a platform"="","Aircraft / helicopter ALS","UAV LiDAR","Mixed aerial laser platforms")),
      shiny::textInput("source_license_url","5. Data licence link *"),
      shiny::textInput("source_email","6. Contact email (optional, private)"),
      shiny::radioButtons("source_scope","Coverage scope (optional)",c("Country-wide","National or regional agency","Local survey"),selected=character(0),inline=TRUE),
      shiny::checkboxInput("source_repository_confirm","Stable public repository: no login, personal-drive links or temporary notebooks.",FALSE),
      shiny::checkboxInput("source_open_license","The data licence explicitly permits reuse.",FALSE),
      shiny::tags$details(shiny::tags$summary("Polygon-to-file mapping"),
        shiny::p("An index connects each coverage polygon to its original file URL. Replace all example values before submitting."),
        shiny::downloadButton("source_index_template","Download index example (GeoJSON)")),
      source_preflight_ui(),shiny::textOutput("source_form_status"),shiny::uiOutput("source_submission"),
      shiny::helpText("Send request opens an email draft for you to review and send. No dataset is published automatically."))),
  footer=shiny::modalButton("Close"))

source_submission_server <- function(input,output,session,check) {
  output$source_index_template <- shiny::downloadHandler(filename = "my-campaign.tiles.geojson", content = function(file) {
    file.copy(system.file("extdata", "contribution-template.geojson", package = "alsdownloader"), file, overwrite = TRUE)
  })
  shiny::observeEvent(input$suggest_source,shiny::showModal(source_submission_ui()))
  request <- shiny::reactive(source_request(input))
  output$source_form_status <- shiny::renderText(request()$message)
  proposal <- shiny::reactive({
    r <- request();shiny::req(r$valid,check()$ready)
    r$body <- paste(r$body,check()$summary,"Awaiting ALS Downloader team review. No dataset has been added.",sep="\n\n");r
  })
  output$source_submission <- shiny::renderUI({
    if(!request()$valid || !isTRUE(check()$ready))return(shiny::actionButton("source_submit_disabled","Submit request",disabled=TRUE))
    r <- proposal();encode <- function(x)utils::URLencode(enc2utf8(x),reserved=TRUE)
    shiny::tagList(shiny::tags$a(id="source_submit",class="btn als-primary",href=paste0("mailto:calvites1990@gmail.com?subject=",encode(r$title),"&body=",encode(r$body)),"Submit request"),
      shiny::downloadButton("source_proposal_file","Save request (.txt)"))
  })
  output$source_proposal_file <- shiny::downloadHandler(filename="als-source-request.txt",content=function(file){r<-proposal();writeLines(enc2utf8(c(r$title,"",r$body)),file,useBytes=TRUE)})
}

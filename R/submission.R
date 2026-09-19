source_request <- function(input) {
  ids <- c("source_name","source_email","source_description","source_origin","source_year","source_platform","source_url","source_license_url","source_access","source_notes","source_scope")
  values <- vapply(ids,function(id)if(is.null(input[[id]]))"" else trimws(input[[id]]),character(1))
  if (grepl("^[0-9]+$",values['source_origin'])) values['source_origin'] <- paste0("10.5281/zenodo.",values['source_origin'])
  fail <- function(message)list(valid=FALSE,message=message)
  if(any(!nzchar(values[seq_len(9)])))return(fail("Complete the nine starred fields below; notes and dataset type are optional."))
  if(!grepl("^[^[:space:]@]+@[^[:space:]@]+\\.[^[:space:]@]+$",values['source_email']))return(fail("That doesn't look like a valid email address - mind checking it?"))
  if(length(strsplit(values['source_description'],"[[:space:]]+")[[1]])>50)return(fail("Just a little shorter, please: keep the description to 50 words or fewer."))
  if(!grepl("^(https://(dx\\.)?doi\\.org/)?10\\.[0-9]{4,9}/[^[:space:]]+$",values['source_origin']))return(fail("We need a dataset DOI, for example 10.5281/zenodo.3633629."))
  if(!grepl("^[0-9]{4}(-[0-9]{4})?$",values['source_year']))return(fail("Let us know the collection year, for example 2019 or 2018-2020."))
  years <- as.integer(strsplit(values['source_year'],"-",fixed=TRUE)[[1]])
  if(any(years<1900 | years>as.integer(format(Sys.Date(),"%Y"))) || (length(years)==2 && years[2]<years[1]))return(fail("That collection-year range doesn't look right - could you double-check it?"))
  if(!values['source_platform'] %in% c("Aircraft / helicopter ALS","UAV LiDAR","Mixed aerial laser platforms"))return(fail("Pick the platform that collected this data."))
  if(!all(grepl("^https://[^[:space:]]+$",values[c('source_url','source_license_url')])))return(fail("The data and license links need to be public HTTPS URLs."))
  if(!isTRUE(input$source_open_license))return(fail("Please confirm the open-data license checkbox below."))
  if(any(nchar(values)>1000))return(fail("Keep each field under 1,000 characters."))
  boundary <- if(is.null(input$source_boundary)) "" else trimws(input$source_boundary)
  zenodo <- grepl("10\\.5281/zenodo\\.[0-9]+$",values['source_origin']) || grepl("^https://zenodo\\.org/",values['source_url'])
  if (zenodo && !nzchar(boundary) && grepl("\\.geojson$",values['source_url'],ignore.case=TRUE)) boundary <- values['source_url']
  if (zenodo && (!grepl("^https://[^[:space:]]+$",boundary) || nchar(boundary)>1000))
    return(fail("Zenodo records also need a polygon boundary or tile-index link (GeoJSON, GeoPackage or a zipped Shapefile) - GPS points alone aren't enough to place it on the map."))
  labels <- c("Dataset","Contact email (private)","Description","Dataset DOI / citation","Collection year(s); final year is representative","Aerial laser platform","GeoJSON index or LAS/LAZ access link","Open-data license","Access requirements","Sensor model / location / additional notes","Dataset type (country-wide / national or regional / local)")
  list(valid=TRUE,message="Looking good - ready for the compatibility check.",title=paste("Dataset suggestion:",values[1]),
    body=paste(paste(labels,values,sep=": ",collapse="\n\n"),if(zenodo) paste("Polygon coverage / index (maintainer review required):",boundary) else "",sep="\n\n"))
}

source_submission_ui <- function() shiny::modalDialog(
  title="Share your dataset",size="l",easyClose=FALSE,
  shiny::p("Have aerial LiDAR data other researchers could use? We would love to add it. Tell us a bit about it below, run a quick compatibility check, and send your request to Cesar Alvites for review - it only takes a few minutes."),
  shiny::p("Your point clouds stay right where they are; we only need a link. If your dataset covers a whole survey area, a small GeoJSON index (one footprint and file link per tile) lets people search it by area. Please don't upload point clouds here."),
  shiny::tags$details(shiny::tags$summary("What is a tile index? (advanced)"),
    shiny::p("A tile index is a small map of your files, not a point cloud. Each polygon marks one file's coverage and links to the original LAS/LAZ download, collection dates, licence and citation. The app intersects those polygons with a user's study area to find relevant files."),
    shiny::p("The example contains placeholder geometry and links: replace them with your actual footprints and file information in a GIS or script. Your point clouds stay with their current host. Downloading this example does not upload or register a dataset."),
    shiny::downloadButton("source_index_template", "Download example index (GeoJSON)")),
  shiny::radioButtons("source_scope","What kind of dataset is this? (optional, just helps us sort it)",
    c("Country-wide","National or regional agency","Local or campus survey"),
    selected=character(0),inline=TRUE),
  shiny::textInput("source_name","Dataset name *"),
  shiny::tags$div(class="shiny-input-container form-group",shiny::tags$label(class="control-label",`for`="source_email","Contact email *"),shiny::tags$input(id="source_email",type="email",class="form-control",placeholder="Just so we can follow up with you - never published")),
  shiny::textAreaInput("source_description","Tell us about it in a sentence or two (up to 50 words) *",rows=2),
  shiny::textInput("source_origin","Dataset DOI or Zenodo record ID *",placeholder="e.g. 10.5281/zenodo.3633629, or just the Zenodo record number"),
  shiny::textInput("source_boundary","Zenodo polygon boundary / tile-index link",placeholder="Needed for Zenodo, unless the data link above is already the GeoJSON tile index"),
  shiny::textInput("source_year","Collection year or interval *",placeholder="e.g. 2019 or 2018-2020 - when it was collected, not published"),
  shiny::selectInput("source_platform","Acquisition platform *",c("Choose a platform"="","Aircraft / helicopter ALS","UAV LiDAR","Mixed aerial laser platforms")),
  shiny::textInput("source_url","Tile-index GeoJSON or direct LAS/LAZ link *",placeholder="A public HTTPS link; an index is best for area search"),
  shiny::textInput("source_license_url","Open-data license URL *"),
  shiny::selectInput("source_access","Access requirements *",c("How can people access it?"="","Public: no account or permission"="public","Free registration required"="registration","Owner permission required"="permission")),
  shiny::textAreaInput("source_notes","Anything else worth knowing - sensor, location, quirks? (optional)",rows=2),
  shiny::checkboxInput("source_open_license","I confirm this is aerial laser data under an explicit open-data license that permits reuse.",FALSE),
  source_preflight_ui(),shiny::textOutput("source_form_status"),shiny::uiOutput("source_submission"),
  shiny::helpText("Sending opens a private email draft for you to review and send yourself from your own mail app - nothing goes out automatically. We only use your email to follow up about this dataset; please don't post it in public GitHub issues."),
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
    r$body <- paste(r$body,check()$summary,"Pending maintainer approval - thank you for sharing your data! We'll follow up at the contact address above once it's reviewed.",sep="\n\n");r
  })
  output$source_submission <- shiny::renderUI({
    if(!request()$valid || !isTRUE(check()$ready))return(shiny::actionButton("source_submit_disabled","Send my request",disabled=TRUE))
    r <- proposal();encode <- function(x)utils::URLencode(enc2utf8(x),reserved=TRUE)
    shiny::tagList(shiny::tags$a(id="source_submit",class="btn als-primary",href=paste0("mailto:calvites1990@gmail.com?subject=",encode(r$title),"&body=",encode(r$body)),"Send my request"),
      shiny::downloadButton("source_proposal_file","Save request (.txt)"))
  })
  output$source_proposal_file <- shiny::downloadHandler(filename="als-source-request.txt",content=function(file){r<-proposal();writeLines(enc2utf8(c(r$title,"",r$body)),file,useBytes=TRUE)})
}

source_request <- function(input) {
  ids <- c("source_name","source_email","source_description","source_origin","source_year","source_platform","source_url","source_license_url","source_access","source_notes")
  values <- vapply(ids,function(id)if(is.null(input[[id]]))"" else trimws(input[[id]]),character(1))
  fail <- function(message)list(valid=FALSE,message=message)
  if(any(!nzchar(values[seq_len(9)])))return(fail("Complete the nine required fields; additional notes are optional."))
  if(!grepl("^[^[:space:]@]+@[^[:space:]@]+\\.[^[:space:]@]+$",values['source_email']))return(fail("Enter a valid contact email address."))
  if(length(strsplit(values['source_description'],"[[:space:]]+")[[1]])>50)return(fail("Keep the description to 50 words or fewer."))
  if(!grepl("^(https://(dx\\.)?doi\\.org/)?10\\.[0-9]{4,9}/[^[:space:]]+$",values['source_origin']))return(fail("Enter a dataset DOI, for example 10.5281/zenodo.3633629."))
  if(!grepl("^[0-9]{4}(-[0-9]{4})?$",values['source_year']))return(fail("Use a collection year or interval, for example 2019 or 2018-2020."))
  years <- as.integer(strsplit(values['source_year'],"-",fixed=TRUE)[[1]])
  if(any(years<1900 | years>as.integer(format(Sys.Date(),"%Y"))) || (length(years)==2 && years[2]<years[1]))return(fail("Enter a valid collection-year interval."))
  if(!values['source_platform'] %in% c("Aircraft / helicopter ALS","UAV LiDAR","Mixed aerial laser platforms"))return(fail("Choose an aerial laser platform."))
  if(!all(grepl("^https://[^[:space:]]+$",values[c('source_url','source_license_url')])))return(fail("Data and license links must be HTTPS URLs."))
  if(!isTRUE(input$source_open_license))return(fail("Confirm the open-data license declaration."))
  if(any(nchar(values)>1000))return(fail("Keep each field within 1,000 characters."))
  labels <- c("Dataset","Contact email (private)","Description","Dataset DOI / citation","Collection year(s); final year is representative","Aerial laser platform","LAS/LAZ access link","Open-data license","Access requirements","Sensor model / location / additional notes")
  list(valid=TRUE,message="Metadata ready for compatibility check.",title=paste("Dataset suggestion:",values[1]),
    body=paste(labels,values,sep=": ",collapse="\n\n"))
}

source_submission_ui <- function() shiny::modalDialog(
  title="Submit your dataset",size="l",easyClose=FALSE,
  shiny::p("Complete ten short fields, check compatibility, then submit your request to Cesar Alvites for review."),
  shiny::textInput("source_name","Dataset name *"),
  shiny::tags$div(class="shiny-input-container form-group",shiny::tags$label(class="control-label",`for`="source_email","Contact email *"),shiny::tags$input(id="source_email",type="email",class="form-control",placeholder="For review and acceptance replies; not published")),
  shiny::textAreaInput("source_description","Description (maximum 50 words) *",rows=2),
  shiny::textInput("source_origin","Dataset DOI *",placeholder="10.5281/zenodo.3633629"),
  shiny::textInput("source_year","Collection year or interval *",placeholder="2019 or 2018-2020; not publication year"),
  shiny::selectInput("source_platform","Acquisition platform *",c("Choose a platform"="","Aircraft / helicopter ALS","UAV LiDAR","Mixed aerial laser platforms")),
  shiny::textInput("source_url","LAS/LAZ files or file-index link *",placeholder="Public HTTPS data link"),
  shiny::textInput("source_license_url","Open-data license URL *"),
  shiny::selectInput("source_access","Access requirements *",c("Choose access conditions"="","Public: no account or permission"="public","Free registration required"="registration","Owner permission required"="permission")),
  shiny::textAreaInput("source_notes","Sensor model, location or other relevant information (optional)",rows=2),
  shiny::checkboxInput("source_open_license","These are aerial laser data with an explicit open-data license permitting reuse.",FALSE),
  source_preflight_ui(),shiny::textOutput("source_form_status"),shiny::uiOutput("source_submission"),
  shiny::helpText("Submit opens a private email draft to the maintainer. Review and send it in your mail application. Nothing is sent automatically. The contact address is used for review and acceptance replies; do not post it in public GitHub issues."),
  footer=shiny::modalButton("Close"))

source_submission_server <- function(input,output,session,check) {
  shiny::observeEvent(input$suggest_source,shiny::showModal(source_submission_ui()))
  request <- shiny::reactive(source_request(input))
  output$source_form_status <- shiny::renderText(request()$message)
  proposal <- shiny::reactive({
    r <- request();shiny::req(r$valid,check()$ready)
    r$body <- paste(r$body,check()$summary,"Pending maintainer approval. Please reply to the contact email when reviewed and integrated.",sep="\n\n");r
  })
  output$source_submission <- shiny::renderUI({
    if(!request()$valid || !isTRUE(check()$ready))return(shiny::actionButton("source_submit_disabled","Submit your request",disabled=TRUE))
    r <- proposal();encode <- function(x)utils::URLencode(enc2utf8(x),reserved=TRUE)
    shiny::tagList(shiny::tags$a(id="source_submit",class="btn als-primary",href=paste0("mailto:calvites1990@gmail.com?subject=",encode(r$title),"&body=",encode(r$body)),"Submit your request"),
      shiny::downloadButton("source_proposal_file","Save request (.txt)"))
  })
  output$source_proposal_file <- shiny::downloadHandler(filename="als-source-request.txt",content=function(file){r<-proposal();writeLines(enc2utf8(c(r$title,"",r$body)),file,useBytes=TRUE)})
}

zenodo_submission_ui <- function() shiny::tagList(
  shiny::p("Share your LiDAR with the community through Zenodo. A prepared coverage polygon file linked to the download assets is required. Your files stay on Zenodo. Only approved footprints and download links enter ALS Downloader."),
  shiny::textInput("zenodo_link","1. Zenodo DOI or product link",placeholder="https://zenodo.org/records/... or 10.5281/zenodo...."),
  shiny::actionButton("zenodo_inspect","Read Zenodo metadata"),shiny::textOutput("zenodo_metadata_status"),
  shiny::tags$details(shiny::tags$summary("Retrieved description, citation and licence"),shiny::verbatimTextOutput("zenodo_metadata_details")),
  shiny::selectInput("zenodo_boundary_source","2. Coverage polygons (required)",c("Upload a coverage file"="upload")),
  shiny::conditionalPanel("input.zenodo_boundary_source == 'upload'",
    shiny::fileInput("zenodo_boundary","GeoJSON, GeoPackage or zipped Shapefile (5 MiB maximum)",accept=c(".geojson",".gpkg",".zip"))),
  shiny::helpText("Use actual surveyed coverage with its CRS, not a location point. With several cloud files, include a file_key column matching each Zenodo filename. Multiple polygons for one file are combined; the original file is downloaded once. Use a single-layer GeoPackage."),
  shiny::textInput("zenodo_acquired","3. Acquisition year or interval (blank if unknown)",placeholder="2018, 2016-2018, or 2018-05-01 / 2018-06-30"),
  shiny::selectInput("zenodo_platform","4. LiDAR acquisition platform",c("Choose a platform"="","Aircraft / helicopter ALS"="ALS","UAV LiDAR"="UAV-LiDAR")),
  shiny::textInput("zenodo_email","5. Contact email (optional, private)"),
  shiny::helpText("If notifications are enabled, your proposal summary and optional contact are emailed to the maintainer through this instance's mail provider. They are not included in the public catalogue."),
  shiny::actionButton("zenodo_prepare","Check my proposal"),shiny::textOutput("zenodo_status"),shiny::uiOutput("zenodo_actions"),
  shiny::helpText("No cloud is downloaded or analysed. ZIP assets require downloading the whole archive and local extraction for 3D. Submission is not approval; the maintainer checks coverage, dates, file mapping and terms before publication."))

zenodo_boundary_download <- function(meta,key) {
  keys<-vapply(meta$files,`[[`,"","key")
  if(length(key)!=1 || is.na(key) || !key %in% keys)stop("Choose a listed coverage file.")
  file<-meta$files[[match(key,keys)]]
  if(is.null(file) || file$size>5*1024^2 || !grepl("\\.(geojson|gpkg|zip)$",key,ignore.case=TRUE))stop("Choose a supported coverage file up to 5 MiB.")
  path<-tempfile(fileext=paste0(".",tools::file_ext(key)))
  ok<-FALSE;on.exit(if(!ok)unlink(path))
  url<-paste0("https://zenodo.org/api/records/",meta$id,"/files/",utils::URLencode(key,reserved=TRUE),"/content")
  response<-httr::GET(url,httr::timeout(30),httr::config(followlocation=FALSE,maxfilesize_large=5*1024^2),httr::write_disk(path))
  if(httr::status_code(response)!=200 || file.size(path)>5*1024^2)stop("The small coverage file could not be read. Download it yourself and use Upload a coverage file.")
  ok<-TRUE;path
}

zenodo_submission_server <- function(input,output,session,queue=NULL,reviewer=NULL) {
  meta<-shiny::reactiveVal(NULL); proposal<-shiny::reactiveVal(NULL)
  message<-shiny::reactiveVal("Paste your Zenodo link to begin. Nothing is published automatically.")
  fail<-function(e)message(conditionMessage(e))
  shiny::observeEvent(input$zenodo_link,{meta(NULL);proposal(NULL)},priority=100)
  shiny::observeEvent(list(input$zenodo_boundary,input$zenodo_boundary_source,input$zenodo_acquired,input$zenodo_platform,input$zenodo_email),{
    proposal(NULL);message("Check the proposal after completing or changing the fields.")
  },ignoreInit=TRUE)
  shiny::observeEvent(input$zenodo_inspect,tryCatch({
    m<-shiny::withProgress(message="Reading Zenodo metadata",value=.3,inspect_zenodo(input$zenodo_link));meta(m)
    keys<-vapply(m$files,`[[`,"","key");sizes<-vapply(m$files,`[[`,0,"size")
    small<-keys[grepl("\\.(geojson|gpkg|zip)$",keys,ignore.case=TRUE)&sizes<=5*1024^2]
    shiny::updateSelectInput(session,"zenodo_boundary_source",choices=c("Upload a coverage file"="upload",stats::setNames(small,small)),selected="upload")
    message("Metadata ready. Confirm coverage, acquisition dates and platform; then check your proposal.")
  },error=fail))
  output$zenodo_metadata_status<-shiny::renderText({m<-meta();if(is.null(m))return("No record loaded.");paste(m$title,"|",m$doi,"|",length(m$files),"files | Licence:",m$license)})
  output$zenodo_metadata_details<-shiny::renderText({m<-meta();shiny::req(m);paste(m$citation,m$license_url,gsub("<[^>]*>"," ",m$description),m$acknowledgement,sep="\n\n")})
  shiny::observeEvent(input$zenodo_prepare,tryCatch({
    m<-meta();if(is.null(m))stop("Read the Zenodo metadata first.")
    if(zenodo_record_id(input$zenodo_link)!=m$id)stop("The link changed; read its metadata again.")
    if(identical(input$zenodo_boundary_source,"upload")) {
      if(is.null(input$zenodo_boundary))stop("Upload the coverage polygon file.")
      boundary<-input$zenodo_boundary
    } else {
      boundary<-zenodo_boundary_download(m,input$zenodo_boundary_source)
      on.exit(unlink(boundary),add=TRUE)
    }
    p<-zenodo_build(m,boundary,input$zenodo_acquired,input$zenodo_platform,input$zenodo_email)
    proposal(p);message(paste("Ready for maintainer review:",length(p$index$features),"mapped download assets. This does not verify point-cloud contents or grant approval."))
  },error=fail))
  output$zenodo_status<-shiny::renderText(message())
  output$zenodo_actions<-shiny::renderUI({shiny::req(proposal());shiny::tagList(
    if(!is.null(queue))shiny::actionButton("zenodo_send","Submit for maintainer review") else shiny::helpText("Online submissions are not configured on this instance. Save the proposal and share it privately with the maintainer."),
    shiny::downloadButton("zenodo_proposal_download","Save proposal JSON"))})
  output$zenodo_proposal_download<-shiny::downloadHandler(filename="zenodo-lidar-proposal.json",content=function(file){p<-proposal();shiny::req(p);jsonlite::write_json(p,file,auto_unbox=TRUE,null="null",digits=NA,pretty=TRUE)})
  shiny::observeEvent(input$zenodo_send,tryCatch({
    if(is.null(queue))stop("The review queue is not configured.")
    p<-proposal();if(is.null(p))stop("Check the proposal first.")
    id<-submit_zenodo(p,queue);message(paste("Request saved privately. Reference:",id,"Pending your maintainer's approval; no dataset has been added."))
  },error=fail))
  if(is.null(reviewer))return(invisible(NULL))
  tick<-shiny::reactiveVal(0L)
  refresh<-function(){
    tick(tick()+1L);rows<-zenodo_submissions(queue);rows<-rows[rows$status=="pending",,drop=FALSE]
    shiny::updateSelectInput(session,"zenodo_review_id",choices=stats::setNames(rows$id,paste(rows$title,substr(rows$id,1,8),sep=" | ")))
  }
  open_review<-function(selected=NULL){
    shiny::showModal(shiny::modalDialog(title="Private Zenodo review",size="l",
      shiny::p("Reviewer: ",reviewer,". Proposals remain inactive until you approve them. No point-cloud analysis is performed."),
      shiny::selectInput("zenodo_review_id","Pending proposal",choices=character()),
      shiny::actionButton("zenodo_review_refresh","Refresh queue"),shiny::textOutput("zenodo_queue_status"),
      shiny::conditionalPanel("input.zenodo_review_id && input.zenodo_review_id.length > 0",
      shiny::verbatimTextOutput("zenodo_review_details"),
      leaflet::leafletOutput("zenodo_review_map",height=280),
      shiny::checkboxInput("zenodo_review_confirm","I checked the aerial LiDAR content, polygon/file correspondence, acquisition dates and licence/attribution requirements.",FALSE),
      shiny::textAreaInput("zenodo_review_reason","Private decision note (optional)",rows=2),
      shiny::actionButton("zenodo_review_approve","Approve and add to catalogue"),shiny::actionButton("zenodo_review_reject","Reject")),
      shiny::textOutput("zenodo_review_status"),footer=shiny::modalButton("Close")))
    refresh()
    if(!is.null(selected)) {
      rows<-zenodo_submissions(queue)
      if(selected %in% rows$id[rows$status=="pending"])
        shiny::updateSelectInput(session,"zenodo_review_id",selected=selected)
    }
  }
  shiny::observeEvent(input$zenodo_review_open,open_review())
  shiny::observeEvent(session$clientData$url_search,{
    selected<-shiny::parseQueryString(session$clientData$url_search)$zenodo_review
    if(length(selected)==1L && grepl("^[a-f0-9]{64}$",selected))open_review(selected)
  },once=TRUE)
  shiny::observeEvent(input$zenodo_review_refresh,refresh())
  output$zenodo_queue_status<-shiny::renderText({tick();rows<-zenodo_submissions(queue);n<-sum(rows$status=="pending")
    if(n==0)"No pending proposals. New submissions will appear here for your review." else paste(n,"proposal(s) awaiting your decision.")})
  chosen<-shiny::reactive({tick();shiny::req(input$zenodo_review_id);zenodo_proposal(queue,input$zenodo_review_id)})
  shiny::observeEvent(input$zenodo_review_id,{shiny::updateCheckboxInput(session,"zenodo_review_confirm",value=FALSE);shiny::updateTextAreaInput(session,"zenodo_review_reason",value="")})
  output$zenodo_review_details<-shiny::renderText({p<-chosen();m<-p$metadata;paste(m$title,m$doi,m$citation,
    paste("Licence:",m$license_url),paste("Acquired:",if(nzchar(p$acquired))p$acquired else "Unknown"),paste("Platform:",p$platform),
    paste("Private contact:",p$contact_email),gsub("<[^>]*>"," ",m$description),m$acknowledgement,
    paste(vapply(p$index$features,function(f)paste(f$properties$file_key,"|",round(f$properties$size_bytes/1024^3,3),"GiB"),""),collapse="\n"),sep="\n\n")})
  output$zenodo_review_map<-leaflet::renderLeaflet({p<-chosen();tmp<-tempfile(fileext=".geojson");on.exit(unlink(tmp));jsonlite::write_json(p$index,tmp,auto_unbox=TRUE,null="null",digits=NA);g<-sf::st_read(tmp,quiet=TRUE)
    leaflet::leaflet(g)|>leaflet::addTiles()|>leaflet::addPolygons(color="#176b68",weight=2,fillOpacity=.2,label=~file_key)})
  status<-shiny::reactiveVal("")
  decide<-function(decision)tryCatch({
    review_zenodo_submission(queue,input$zenodo_review_id,decision,reviewer,isTRUE(input$zenodo_review_confirm),input$zenodo_review_reason)
    status(if(decision=="approve")"Approved. The dataset is available on the next AOI search." else "Rejected. No coverage was added.");refresh()
  },error=function(e)status(conditionMessage(e)))
  shiny::observeEvent(input$zenodo_review_approve,decide("approve"))
  shiny::observeEvent(input$zenodo_review_reject,decide("reject"))
  output$zenodo_review_status<-shiny::renderText(status())
  invisible(NULL)
}

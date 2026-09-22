zenodo_submission_ui <- function() shiny::tagList(
  shiny::p("Contribute airborne LiDAR hosted on Zenodo. Add the DOI, identify the point-cloud files and provide their coverage."),
  shiny::textInput("zenodo_link","1. Zenodo DOI or product link",placeholder="https://zenodo.org/records/... or 10.5281/zenodo...."),
  shiny::actionButton("zenodo_inspect","Read Zenodo metadata"),shiny::textOutput("zenodo_metadata_status"),
  shiny::tags$details(shiny::tags$summary("Retrieved description, citation and licence"),shiny::verbatimTextOutput("zenodo_metadata_details")),
  shiny::radioButtons("zenodo_has_boundary","2. Do you have boundary polygons for these point clouds?",
    choices=c("Yes - read a polygon file"="yes","No - declare an approximate square"="no"),selected="yes"),
  shiny::conditionalPanel("input.zenodo_has_boundary == 'yes'",
  shiny::selectInput("zenodo_boundary_source","Coverage polygons",c("Upload coverage polygons"="upload")),
  shiny::conditionalPanel("input.zenodo_boundary_source == 'upload'",
    shiny::fileInput("zenodo_boundary","GeoJSON, GeoPackage or zipped Shapefile (5 MiB maximum)",accept=c(".geojson",".gpkg",".zip"))),
  shiny::helpText("Choose a polygon file from Zenodo or upload one. Use a single-layer GeoPackage or a ZIP containing SHP, SHX, DBF and PRJ."),
  shiny::selectInput("zenodo_id_column", "Column containing the point-cloud tile ID", choices=character()),
  shiny::helpText("Each polygon must have a tile ID. Match each ID to its LAS/LAZ file or ZIP archive on Zenodo."),
  shiny::uiOutput("zenodo_file_mapping")),
  shiny::conditionalPanel("input.zenodo_has_boundary == 'no'",
    shiny::helpText("Click the map to set the centre, or enter WGS84 decimal coordinates. The orange square is a declared search extent, not verified LiDAR coverage."),
    shiny::fluidRow(shiny::column(6,shiny::numericInput("zenodo_longitude","Centre longitude",value=NA,min=-180,max=180)),
      shiny::column(6,shiny::numericInput("zenodo_latitude","Centre latitude",value=NA,min=-85,max=85))),
    shiny::numericInput("zenodo_distance","Centre to each side (metres)",value=1000,min=1,max=10000),
    shiny::helpText("Default: 1,000 m (1 km) to each side gives a 2 x 2 km square. Maximum: 10,000 m (10 km) to each side gives a 20 x 20 km square. For larger or separated areas, upload polygons."),
    leaflet::leafletOutput("zenodo_extent_map",height=260),shiny::textOutput("zenodo_extent_status"),
    shiny::selectInput("zenodo_extent_files","Files or archives within this declared extent",choices=character(),multiple=TRUE),
    shiny::checkboxInput("zenodo_extent_confirm","I declare that this square encloses the selected point clouds. I understand that it will be labelled approximate.",FALSE)),
  shiny::radioButtons("zenodo_year_mode","3. Acquisition years",c("Single year"="single","Multiple years"="multiple"),inline=TRUE),
  shiny::conditionalPanel("input.zenodo_year_mode === 'multiple'",shiny::numericInput("zenodo_year_count","Number of years",2,min=2,max=30,step=1)),
  shiny::uiOutput("zenodo_year_fields"),
  shiny::textInput("zenodo_email","4. Contact email (required, private)"),
  shiny::helpText("Your email is shared with the review service and maintainer; it stays out of the public catalogue."),
  shiny::actionButton("zenodo_prepare","Validate submission"),shiny::textOutput("zenodo_status"),shiny::uiOutput("zenodo_actions"),
  shiny::helpText("Validate the completed form to enable Submit. The ALSdownloadeR team will review your submission."),
  shiny::tags$a(href="https://github.com/Cesarito2021/als_downloader",target="_blank",rel="noopener noreferrer","See details on GitHub"))

zenodo_boundary_download <- function(meta,key) {
  keys<-vapply(meta$files,`[[`,"","key")
  if(length(key)!=1 || is.na(key) || !key %in% keys)stop("Choose a listed coverage file.")
  file<-meta$files[[match(key,keys)]]
  if(is.null(file) || file$size>5*1024^2 || !grepl("\\.(geojson|gpkg|zip)$",key,ignore.case=TRUE))stop("Choose a supported coverage file up to 5 MiB.")
  path<-tempfile(fileext=paste0(".",tools::file_ext(key)))
  ok<-FALSE;on.exit(if(!ok)unlink(path))
  url<-paste0("https://zenodo.org/api/records/",meta$id,"/files/",utils::URLencode(key,reserved=TRUE),"/content")
  response<-httr::GET(url,httr::timeout(30),httr::config(followlocation=FALSE,maxfilesize_large=5*1024^2),httr::write_disk(path))
  if(httr::status_code(response)!=200 || file.size(path)>5*1024^2)stop("The small coverage file could not be read. Download the file from the source and use Upload a coverage file.")
  ok<-TRUE;path
}

zenodo_submission_server <- function(input,output,session,queue=NULL,reviewer=NULL,access=NULL) {
  meta<-shiny::reactiveVal(NULL); proposal<-shiny::reactiveVal(NULL)
  proposal_source<-shiny::reactiveVal(NULL)
  message<-shiny::reactiveVal("A Zenodo DOI or record URL is required. Publication requires approval.")
  fail<-function(e)message(conditionMessage(e))
  shiny::observeEvent(input$zenodo_link,{meta(NULL);proposal(NULL)},priority=100)
  shiny::observeEvent(list(input$zenodo_boundary,input$zenodo_boundary_source,input$zenodo_id_column, selected_years(), mapping_values(),input$zenodo_email,
    input$zenodo_has_boundary,input$zenodo_longitude,input$zenodo_latitude,input$zenodo_distance,input$zenodo_extent_files,input$zenodo_extent_confirm),{
    proposal(NULL);message("Validate the completed form to enable Submit.")
  },ignoreInit=TRUE)
  shiny::observeEvent(list(input$zenodo_has_boundary,input$zenodo_longitude,input$zenodo_latitude,input$zenodo_distance,input$zenodo_extent_files,input$zenodo_link),{
    shiny::updateCheckboxInput(session,"zenodo_extent_confirm",value=FALSE)
  },ignoreInit=TRUE)
  output$zenodo_year_fields<-shiny::renderUI({
    n<-if(identical(input$zenodo_year_mode,"multiple"))input$zenodo_year_count else 1L
    if(is.null(n) || !is.finite(n) || n<1 || n>30)return(NULL)
    shiny::div(style="display:flex;gap:12px;flex-wrap:wrap;",lapply(seq_len(as.integer(n)),function(i)
      shiny::div(style="width:140px",shiny::selectInput(paste0("zenodo_year_",i),paste("Year",i),c("Choose year"="",rev(seq.int(1900L,as.integer(format(Sys.Date(),"%Y"))))),selected=""))))
  })
  selected_years<-shiny::reactive({
    n<-if(identical(input$zenodo_year_mode,"multiple"))input$zenodo_year_count else 1L
    if(is.null(n) || !is.finite(n) || n<1 || n>30)return(character())
    vapply(seq_len(as.integer(n)),function(i){v<-input[[paste0("zenodo_year_",i)]];if(is.null(v))"" else v},"")
  })
  boundary_data<-shiny::reactive({
    if(!identical(input$zenodo_has_boundary,"yes"))return(NULL)
    tryCatch({
      if(identical(input$zenodo_boundary_source,"upload")) {
        if(is.null(input$zenodo_boundary))return(NULL)
        zenodo_boundary(input$zenodo_boundary)
      } else {
        shiny::req(meta(),input$zenodo_boundary_source)
        path<-zenodo_boundary_download(meta(),input$zenodo_boundary_source)
        on.exit(unlink(path));zenodo_boundary(path)
      }
    },error=function(e){message(conditionMessage(e));NULL})
  })
  shiny::observeEvent(boundary_data(),{
    g<-boundary_data();cols<-if(is.null(g))character() else names(sf::st_drop_geometry(g))
    preferred<-intersect(c("file_key","tile_id","id","name"),cols)
    shiny::updateSelectInput(session,"zenodo_id_column",choices=c("Choose ID column"="",stats::setNames(cols,cols)),selected=if(length(preferred))preferred[1] else "")
  },ignoreNULL=FALSE)
  polygon_ids<-shiny::reactive({g<-boundary_data();if(is.null(g) || is.null(input$zenodo_id_column) || !input$zenodo_id_column %in% names(g))return(character());unique(as.character(g[[input$zenodo_id_column]]))})
  output$zenodo_file_mapping<-shiny::renderUI({
    m<-meta();shiny::req(m);ids<-polygon_ids();keys<-vapply(m$files,`[[`,"","key");keys<-keys[grepl("\\.(las|laz|zip)$",keys,ignore.case=TRUE)]
    lapply(seq_along(ids),function(i){
      candidate<-keys[keys==ids[i] | tools::file_path_sans_ext(basename(keys))==ids[i]]
      if(length(keys)==1L)candidate<-keys
      shiny::selectInput(paste0("zenodo_map_",i),paste("Zenodo file for tile ID",ids[i]),c("Choose file"="",stats::setNames(keys,keys)),selected=if(length(candidate)==1L)candidate else "")
    })
  })
  mapping_values<-shiny::reactive({ids<-polygon_ids();stats::setNames(vapply(seq_along(ids),function(i){v<-input[[paste0("zenodo_map_",i)]];if(is.null(v))"" else v},""),ids)})
  output$zenodo_extent_map<-leaflet::renderLeaflet(leaflet::leaflet()|>leaflet::addTiles()|>leaflet::setView(lng=0,lat=15,zoom=2))
  shiny::observeEvent(input$zenodo_extent_map_click,{
    click<-input$zenodo_extent_map_click
    shiny::updateNumericInput(session,"zenodo_longitude",value=round(click$lng,6))
    shiny::updateNumericInput(session,"zenodo_latitude",value=round(click$lat,6))
  })
  square<-shiny::reactive({
    shiny::req(identical(input$zenodo_has_boundary,"no"))
    zenodo_square(input$zenodo_longitude,input$zenodo_latitude,input$zenodo_distance,"preview.laz")
  })
  output$zenodo_extent_status<-shiny::renderText(tryCatch({square();paste("Approximate square:",2*input$zenodo_distance,"x",2*input$zenodo_distance,"metres. Coverage inside is unverified.")},error=function(e)conditionMessage(e)))
  shiny::observeEvent(list(input$zenodo_has_boundary,input$zenodo_longitude,input$zenodo_latitude,input$zenodo_distance),{
    if(!identical(input$zenodo_has_boundary,"no"))return()
    proxy<-leaflet::leafletProxy("zenodo_extent_map",session=session)|>leaflet::clearShapes()|>leaflet::clearMarkers()
    g<-tryCatch(square(),error=function(e)NULL)
    if(is.null(g))return()
    leaflet::addPolygons(proxy,data=g,color="#b76b12",fillOpacity=.18,weight=2,label="Author-declared approximate extent")
    leaflet::addCircleMarkers(proxy,lng=input$zenodo_longitude,lat=input$zenodo_latitude,radius=4)
    leaflet::setView(proxy,lng=input$zenodo_longitude,lat=input$zenodo_latitude,
      zoom=max(2,min(18,floor(log2(40000000/(input$zenodo_distance*6))))))
  },ignoreInit=TRUE)
  shiny::observeEvent(input$zenodo_inspect,tryCatch({
    m<-shiny::withProgress(message="Reading Zenodo metadata",value=.3,inspect_zenodo(input$zenodo_link));meta(m)
    keys<-vapply(m$files,`[[`,"","key");sizes<-vapply(m$files,`[[`,0,"size")
    small<-keys[grepl("\\.(geojson|gpkg|zip)$",keys,ignore.case=TRUE)&sizes<=5*1024^2]
    shiny::updateSelectInput(session,"zenodo_boundary_source",choices=c("Upload coverage polygons"="upload",stats::setNames(small,small)),selected="upload")
    assets<-keys[grepl("\\.(las|laz|zip)$",keys,ignore.case=TRUE)]
    shiny::updateSelectInput(session,"zenodo_extent_files",choices=assets,selected=character())
    message("Metadata ready. Choose coverage, tile IDs and acquisition years; then validate.")
  },error=fail))
  output$zenodo_metadata_status<-shiny::renderText({m<-meta();if(is.null(m))return("No record loaded.");paste(m$title,"|",m$doi,"|",length(m$files),"files | Licence:",m$license)})
  output$zenodo_metadata_details<-shiny::renderText({m<-meta();shiny::req(m);paste(m$citation,m$license_url,gsub("<[^>]*>"," ",m$description),m$acknowledgement,
    paste("Zenodo filenames (use these exact values for file_key):",paste(vapply(m$files,`[[`,"","key"),collapse="\n"),sep="\n"),sep="\n\n")})
  shiny::observeEvent(input$zenodo_prepare,tryCatch({
    m<-meta();if(is.null(m))stop("Read the Zenodo metadata first.")
    if(zenodo_record_id(input$zenodo_link)!=m$id)stop("The link changed; read its metadata again.")
    if(identical(input$zenodo_has_boundary,"no")) {
      if(!isTRUE(input$zenodo_extent_confirm))stop("Confirm that the approximate square encloses the selected files.")
      boundary<-zenodo_square(input$zenodo_longitude,input$zenodo_latitude,input$zenodo_distance,input$zenodo_extent_files)
    } else {
      boundary<-boundary_data();if(is.null(boundary))stop("Read a coverage polygon file first.")
      boundary<-zenodo_id_mapping(boundary,input$zenodo_id_column,vapply(m$files,`[[`,"","key"),mapping_values())
    }
    if(is.null(input$zenodo_email) || !grepl("^[^[:space:]@]+@[^[:space:]@]+\\.[^[:space:]@]+$",input$zenodo_email))stop("Enter a valid private contact email.")
    years<-zenodo_selected_years(input$zenodo_year_mode,input$zenodo_year_count,selected_years())
    p<-zenodo_build(m,boundary,years,"ALS",input$zenodo_email)
    proposal_source(if(identical(input$zenodo_has_boundary,"no")) list(kind="approximate") else
      if(identical(input$zenodo_boundary_source,"upload")) list(kind="upload") else
        list(kind="zenodo",key=input$zenodo_boundary_source,mapping="",id_column=input$zenodo_id_column,id_mapping=mapping_values()))
    proposal(p);message("Validated. You can now submit for ALSdownloadeR team review.")
  },error=fail))
  output$zenodo_status<-shiny::renderText(message())
  output$zenodo_actions<-shiny::renderUI({shiny::req(proposal());
    config<-tryCatch(zenodo_formspree_config(default=is.null(queue)),error=function(e)e)
    shiny::tagList(
    if(inherits(config,"error")) shiny::helpText(conditionMessage(config)) else
      if(!is.null(config)) tryCatch(zenodo_formspree_ui(proposal(),proposal_source(),config),error=function(e)shiny::helpText(conditionMessage(e))) else
      if(!is.null(queue))shiny::actionButton("zenodo_send","Submit for maintainer review") else shiny::helpText("Online submissions are not configured on this instance. Save the proposal and share it privately with the maintainer."),
    shiny::downloadButton("zenodo_proposal_download","Save proposal JSON"))})
  output$zenodo_proposal_download<-shiny::downloadHandler(filename="zenodo-lidar-proposal.json",content=function(file){p<-proposal();shiny::req(p);jsonlite::write_json(p,file,auto_unbox=TRUE,null="null",digits=NA,pretty=TRUE)})
  shiny::observeEvent(input$zenodo_send,tryCatch({
    if(!is.null(zenodo_formspree_config(default=is.null(queue))))stop("Use the Formspree submission button.")
    if(is.null(queue))stop("The review queue is not configured.")
    p<-proposal();if(is.null(p))stop("Validate the submission first.")
    id<-submit_zenodo(p,queue);message(paste("Proposal received. Reference:",id,"| DOI:",p$metadata$doi,"| The ALSdownloadeR team will review your submission."))

  },error=fail))
  if(is.null(reviewer))return(invisible(NULL))
  if(is.null(access)) access <- reviewer_access_controller(queue)
  token <- shiny::reactiveVal(NULL)
  requested <- shiny::reactiveVal(NULL)
  login_message <- shiny::reactiveVal("")
  invitation <- shiny::reactiveVal(NULL)
  authenticated <- shiny::reactive({
    shiny::invalidateLater(30000,session)
    access$valid(token())
  })
  require_reviewer <- function() {
    # Always validate at the action itself, even between expiry timer ticks.
    shiny::req(access$valid(token()))
  }
  show_login <- function() {
    login_message("")
    shiny::showModal(shiny::modalDialog(title="Private proposal review",
      shiny::p("Use the private link delivered to the maintainer mailbox. No app password is required."),
      shiny::actionButton("review_login","Open private review"),shiny::textOutput("review_login_message"),
      footer=shiny::modalButton("Close")))
  }
  output$review_login_message <- shiny::renderText(login_message())
  shiny::observeEvent(input$review_login,{
    result <- access$login(requested(),invitation())
    invitation(NULL)
    if(is.null(result)) {login_message("This private link is unavailable, expired or already used. Access remains locked.");return()}
    token(result)
    open_review(result$id)
  })
  shiny::observeEvent(input$review_logout,{
    token(NULL); requested(NULL); shiny::removeModal(); session$reload()
  })
  shiny::observeEvent(authenticated(),{
    if(!authenticated() && !is.null(token())) {
      token(NULL); shiny::removeModal()
      shiny::showNotification("Private review session ended. A new invitation is needed to reopen an unfinished review.",type="message")
    }
  },ignoreInit=TRUE)
  tick<-shiny::reactiveVal(0L)
  refresh<-function(){
    require_reviewer()
    tick(tick()+1L);rows<-zenodo_submissions(queue);rows<-rows[rows$status=="pending" & rows$id==token()$id,,drop=FALSE]
    shiny::updateSelectInput(session,"zenodo_review_id",choices=stats::setNames(rows$id,paste(rows$title,substr(rows$id,1,8),sep=" | ")))
  }
  open_review<-function(selected=NULL){
    requested(selected)
    if(!access$valid(token())) {show_login();return(invisible(NULL))}
    shiny::showModal(shiny::modalDialog(title="Review submissions",size="l",
      shiny::p("ALSdownloadeR team | Private maintainer panel. Proposals remain inactive until approved. No point-cloud analysis is performed."),
      shiny::actionButton("review_logout","Sign out"),
      shiny::selectInput("zenodo_review_id","Pending proposal",choices=character()),
      shiny::actionButton("zenodo_review_refresh","Refresh queue"),shiny::textOutput("zenodo_queue_status"),
      shiny::conditionalPanel("input.zenodo_review_id && input.zenodo_review_id.length > 0",
      shiny::verbatimTextOutput("zenodo_review_details"),
      leaflet::leafletOutput("zenodo_review_map",height=280),
      shiny::checkboxInput("zenodo_review_confirm","Confirm verification of aerial LiDAR content, coverage method, file correspondence, acquisition dates and licence/attribution requirements. Any approximate extent must retain its label.",FALSE),
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
  shiny::observeEvent(list(session$clientData$url_search,session$clientData$url_hash),{
    selected<-shiny::parseQueryString(session$clientData$url_search)$zenodo_review
    hash <- session$clientData$url_hash
    key <- if(is.character(hash)&&length(hash)==1L) shiny::parseQueryString(sub("^#","",hash))$review_key else NULL
    if(length(selected)==1L && grepl("^[a-f0-9]{64}$",selected) && length(key)==1L && grepl("^[a-f0-9]{64}$",key)) {
      invitation(key); open_review(selected)
      session$sendCustomMessage("als-clear-review-link",list())
    }
  })
  shiny::observeEvent(input$zenodo_review_refresh,refresh())
  output$zenodo_queue_status<-shiny::renderText({shiny::req(authenticated());require_reviewer();tick();rows<-zenodo_submissions(queue);n<-sum(rows$status=="pending" & rows$id==token()$id)
    if(n==0)"No pending proposals. New submissions will appear here for review." else paste(n,"proposal(s) awaiting review.")})
  chosen<-shiny::reactive({shiny::req(authenticated());require_reviewer();tick();shiny::req(identical(input$zenodo_review_id,token()$id));zenodo_proposal(queue,input$zenodo_review_id)})
  shiny::observeEvent(input$zenodo_review_id,{shiny::updateCheckboxInput(session,"zenodo_review_confirm",value=FALSE);shiny::updateTextAreaInput(session,"zenodo_review_reason",value="")})
  output$zenodo_review_details<-shiny::renderText({p<-chosen();m<-p$metadata;paste(m$title,m$doi,m$citation,
    zenodo_coverage_label(p),
    paste("Licence:",m$license_url),paste("Acquired:",if(nzchar(p$acquired))p$acquired else "Unknown"),paste("Platform:",p$platform),
    paste("Private contact:",p$contact_email),gsub("<[^>]*>"," ",m$description),m$acknowledgement,
    paste(vapply(p$index$features,function(f)paste(f$properties$file_key,"|",round(f$properties$size_bytes/1024^3,3),"GiB"),""),collapse="\n"),sep="\n\n")})
  output$zenodo_review_map<-leaflet::renderLeaflet({p<-chosen();tmp<-tempfile(fileext=".geojson");on.exit(unlink(tmp));jsonlite::write_json(p$index,tmp,auto_unbox=TRUE,null="null",digits=NA);g<-sf::st_read(tmp,quiet=TRUE)
    leaflet::leaflet(g)|>leaflet::addTiles()|>leaflet::addPolygons(color="#176b68",weight=2,fillOpacity=.2,label=~file_key)})
  status<-shiny::reactiveVal("")
  decide<-function(decision)tryCatch({
    require_reviewer()
    shiny::req(identical(input$zenodo_review_id,token()$id))
    review_zenodo_submission(queue,input$zenodo_review_id,decision,paste(reviewer,token()$email),isTRUE(input$zenodo_review_confirm),input$zenodo_review_reason)
    status(if(decision=="approve")"Approved. Coverage appears in Explorer within a few seconds; use Find ALS data to retrieve its files." else "Rejected. No coverage was added.");shiny::showNotification(status(),duration=10);token(NULL);shiny::removeModal()
  },error=function(e)status(conditionMessage(e)))
  shiny::observeEvent(input$zenodo_review_approve,decide("approve"))
  shiny::observeEvent(input$zenodo_review_reject,decide("reject"))
  output$zenodo_review_status<-shiny::renderText({shiny::req(authenticated());status()})
  invisible(NULL)
}

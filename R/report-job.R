report_render_job <- function(args) {
  if (!requireNamespace("tinytex", quietly=TRUE) || !isTRUE(tinytex::is_tinytex()))
    stop("PDF reports require TinyTeX. Run tinytex::install_tinytex() in R, then restart the app.")
  report_pandoc()
  path<-do.call(als_report,args)
  if(!file.exists(path) || !identical(readBin(path,"raw",5),charToRaw("%PDF-")))stop("Report generation did not produce a PDF.")
  path
}

report_download_server <- function(input,output,session,tiles,aoi,map) {
  state<-shiny::reactiveValues(job=NULL,directory=NULL,path=NULL,started=NULL)
  status<-function(text)session$sendCustomMessage("als-report-status",text)
  cleanup<-function(){
    if(!is.null(state$job) && state$job$is_alive())state$job$kill_tree()
    state$job<-NULL
    if(!is.null(state$directory))unlink(state$directory,recursive=TRUE)
    state$directory<-NULL;state$path<-NULL
  }
  shiny::observeEvent(input$report_prepare,{
    if(!is.null(state$job)){status("A report is already being prepared.");return()}
    tryCatch({
      cleanup();x<-tiles()
      if(is.null(x) || !nrow(x))stop("Search for tiles first.")
      state$directory<-tempfile("als-report-");dir.create(state$directory)
      # Snapshot uploaded images and the map before launching the worker.
      pictures<-if(is.null(input$report_figures))character() else input$report_figures$datapath
      staged<-if(length(pictures))file.path(state$directory,paste0("figure-",seq_along(pictures),".png")) else character()
      if(length(pictures) && !all(file.copy(pictures,staged)))stop("Could not prepare report images.")
      rgb<-if(isTRUE(input$report_rgb))map() else list(path=character(),credits="")
      if(length(rgb$path)) {
        target<-file.path(state$directory,"map.png")
        if(!file.copy(rgb$path,target))stop("Could not prepare report map.")
        rgb$path<-target
      }
      region<-aoi()
      args<-list(tiles=x,output_dir=state$directory,format="pdf",aoi=region,
        aoi_area_km2=if(is.null(region))NA_real_ else aoi_area(region),figures=staged,
        details=isTRUE(input$report_details),map_image=rgb$path,map_credits=rgb$credits)
      state$started<-Sys.time();status("Preparing PDF | 0 s")
      state$job<-background_job("report_render_job",list(args))
    },error=function(e){cleanup();status(paste("Report failed:",conditionMessage(e)))})
  })
  shiny::observe({
    shiny::invalidateLater(700,session);job<-state$job
    if(is.null(job))return()
    elapsed<-as.numeric(difftime(Sys.time(),state$started,units="secs"))
    if(job$is_alive()){status(sprintf("Preparing PDF | %.0f s",elapsed));return()}
    state$job<-NULL
    tryCatch({state$path<-job$get_result()
      status(sprintf("Report ready | %.0f s. Download sent to your browser.",elapsed))
      session$sendCustomMessage("als-report-download","download_report_pdf")
    },error=function(e){cleanup();status(paste("Report failed:",redact_urls_in_text(conditionMessage(e))))})
  })
  output$download_report_pdf<-shiny::downloadHandler(filename="als-session-report.pdf",contentType="application/pdf",content=function(file){
    shiny::req(state$path,file.exists(state$path))
    if(!file.copy(state$path,file,overwrite=TRUE))stop("Could not send the report.")
  })
  session$onSessionEnded(function()shiny::isolate(cleanup()))
}

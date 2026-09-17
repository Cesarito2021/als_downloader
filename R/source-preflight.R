# Technical preview of contributor-supplied public files; never grants approval.
public_sample_target <- function(url, resolver = function(host) curl::nslookup(host, ipv4 = TRUE)) {
  u <- httr::parse_url(url)
  if (!identical(u$scheme, "https") || is.null(u$hostname) ||
      !grepl("^[A-Za-z0-9.-]+$", u$hostname) ||
      (!is.null(u$port) && u$port != "443") ||
      !is.null(u$username) || !is.null(u$password)) stop("Use a public HTTPS file URL without credentials, on port 443.")
  ips <- resolver(u$hostname)
  public <- function(ip) {
    if (!grepl("^[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+$", ip)) return(FALSE)
    n <- as.integer(strsplit(ip, ".", fixed=TRUE)[[1]])
    if (anyNA(n) || any(n > 255)) return(FALSE)
    !(n[1] %in% c(0,10,127) || n[1] >= 224 ||
      (n[1] == 100 && n[2] >= 64 && n[2] <= 127) ||
      (n[1] == 169 && n[2] == 254) || (n[1] == 172 && n[2] >= 16 && n[2] <= 31) ||
      (n[1] == 192 && (n[2] %in% c(0,168) || (n[2] == 88 && n[3] == 99))) ||
      (n[1] == 198 && (n[2] %in% c(18,19) || (n[2] == 51 && n[3] == 100))) ||
      (n[1] == 203 && n[2] == 0 && n[3] == 113))
  }
  if (!length(ips) || !all(vapply(ips, public, logical(1)))) stop("The test requires a publicly routable IPv4 host.")
  list(host=u$hostname, ip=ips[1])
}

source_preflight <- function(url, directory) {
  progress <- function(n, message) writeLines(c(as.character(n), message), file.path(directory,"stage.txt"))
  progress(10,"Checking public HTTPS access")
  target <- public_sample_target(url)
  # Pin the vetted address and disallow redirects, cookies, authentication and proxies.
  config <- httr::config(resolve=paste0(target$host,":443:",target$ip), followlocation=FALSE, proxy="", netrc=0L)
  handle <- httr::handle(url)
  head <- httr::HEAD(url, config, httr::timeout(20), handle=handle)
  if (httr::status_code(head) != 200L) stop("Direct anonymous access failed. Use the final public file URL; portals, redirects and login pages need manual review.")
  size <- suppressWarnings(as.numeric(httr::headers(head)[["content-length"]]))
  cap <- 50 * 1024^2
  if (length(size) != 1L || !is.finite(size) || size <= 0 || size > cap)
    stop("Access responded, but preview needs a sample LAS/LAZ file with known size up to 50 MB. Provide a smaller representative sample; this does not reject the dataset.")
  progress(35,"Downloading temporary sample (maximum 50 MB)")
  path <- file.path(directory,"sample.laz")
  on.exit(unlink(path),add=TRUE)
  response <- httr::GET(url, config, httr::config(maxfilesize_large=cap), httr::timeout(90), httr::write_disk(path), handle=handle)
  if (httr::status_code(response) != 200L || file.size(path) != size || !valid_las_header(path)) stop("The response is not a complete LAS/LAZ file.")
  progress(70,"Decoding LAS/LAZ sample")
  points <- read_preview(path, 30000L)
  progress(90,"Preparing the 3D preview")
  list(points=unname(as.matrix(points)), origin=unname(attr(points,"origin")),
    summary=sprintf("Technical sample test passed: %s display points; %.2f MB. Public access and decoding checked. Platform, license, coverage and full-dataset integration still require review.",nrow(points),size/1024^2))
}

source_preflight_ui <- function() shiny::tagList(
  shiny::textInput("source_sample_url", "Public LAS/LAZ sample URL for testing", placeholder="Direct HTTPS file up to 50 MB; no private tokens"),
  shiny::selectInput("source_access", "Does access require registration or permission?", c("Choose access conditions"="", "Public: no account or permission"="public", "Free registration required"="registration", "Owner permission required"="permission")),
  shiny::textInput("source_license_url", "Open-data license URL *"),
  shiny::checkboxInput("source_open_license", "I confirm that an explicit open-data license permits reuse of these aerial LiDAR data.",FALSE),
  shiny::actionButton("source_test", "Test connection and preview"),
  shiny::actionButton("source_test_cancel", "Cancel test"),
  shiny::uiOutput("source_test_progress"),
  shiny::tags$canvas(id="als-source-cloud",class="als-point-cloud",style="height:260px",tabindex="0",role="img",`aria-label`="Contributor sample preview: drag to rotate"),
  shiny::helpText("100% means the technical sample test completed, not publication approval. Large files, login-based access and unsupported endpoints can still be submitted for manual review. No fixed approval deadline is promised."))

source_preflight_server <- function(input, output, session, state, mode, hosted_lock) {
  check <- shiny::reactiveValues(job=NULL,directory=NULL,locked=FALSE,percent=0,message="Optional: test a small public aerial LAS/LAZ sample.",summary="Not tested")
  cleanup <- function() {
    if (!is.null(check$job) && check$job$is_alive()) check$job$kill_tree()
    check$job <- NULL
    if (!is.null(check$directory)) unlink(check$directory,recursive=TRUE)
    check$directory <- NULL
    if (isTRUE(check$locked)) unlink(hosted_lock,recursive=TRUE)
    check$locked <- FALSE
    state$source_test_busy <- FALSE
  }
  reset <- function() {cleanup();check$percent <- 0;check$summary <- "Not tested";check$message <- "Test a public sample, or submit for manual review.";session$sendCustomMessage("als-points",list(target="als-source-cloud",points=list(),origin=c(0,0,0)))}
  shiny::observeEvent(list(input$source_sample_url,input$source_url,input$source_platform,input$source_license_url,input$source_open_license,input$source_access), reset(),ignoreNULL=FALSE)
  shiny::observeEvent(input$source_test_cancel,reset())
  shiny::observeEvent(input$source_test, {
    if (is.null(input$source_sample_url) || !nzchar(trimws(input$source_sample_url))) {check$message <- "Provide a direct public LAS/LAZ sample URL.";return()}
    if (!isTRUE(input$source_open_license) || !nzchar(input$source_license_url) || !input$source_platform %in% c("Aircraft / helicopter ALS","UAV LiDAR","Mixed aerial laser platforms")) {check$message <- "Declare an aerial LiDAR platform and an open-data license first.";return()}
    if (isTRUE(state$comparison_busy) || isTRUE(state$source_test_busy) || (!is.null(state$job) && state$job$is_alive()) || (!is.null(state$preview_job) && state$preview_job$is_alive())) {check$message <- "Wait for the current transfer or preview.";return()}
    reset()
    tryCatch({
      if(mode == "hosted") {
        if(!dir.create(hosted_lock,showWarnings=FALSE)) stop("Another hosted transfer is running. Try later.")
        check$locked <- TRUE
      }
      check$directory <- tempfile("als-source-test-");dir.create(check$directory)
      state$source_test_busy <- TRUE;check$percent <- 5;check$message <- "Starting sample test"
      check$job <- callr::r_bg(function(url,directory) alsdownloader:::source_preflight(url,directory),args=list(trimws(input$source_sample_url),check$directory),supervise=TRUE)
    },error=function(e){cleanup();check$message <- conditionMessage(e)})
  })
  shiny::observe({
    shiny::invalidateLater(400,session)
    job <- check$job;if(is.null(job))return()
    if(job$is_alive()) {
      path <- file.path(check$directory,"stage.txt")
      if(file.exists(path)) {x <- tryCatch(readLines(path,warn=FALSE),error=function(e)character());if(length(x)>=2){check$percent <- as.numeric(x[1]);check$message <- x[2]}}
      return()
    }
    tryCatch({
      result <- job$get_result();check$percent <- 100;check$summary <- result$summary;check$message <- result$summary
      session$sendCustomMessage("als-points",list(target="als-source-cloud",points=result$points,origin=result$origin))
    },error=function(e){check$summary <- "Technical test incomplete; manual review required";check$message <- paste("Test incomplete:",conditionMessage(e))})
    cleanup()
  })
  output$source_test_progress <- shiny::renderUI(shiny::tagList(
    shiny::div(class="source-test-track",shiny::span(class="source-test-gator",style=paste0("left:",check$percent,"%"),shiny::HTML("&#128010;")),shiny::span(class="source-test-lake",shiny::HTML("&#127754;"))),
    shiny::tags$progress(value=check$percent,max=100,`aria-label`="Technical test progress"),
    shiny::p(role="status",paste0(check$percent,"% - ",check$message))))
  session$onSessionEnded(function()shiny::isolate(cleanup()))
  shiny::reactive(check$summary)
}

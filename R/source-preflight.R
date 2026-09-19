# Link-only connectivity check; no point-cloud transfer, decoding or plotting.
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

source_preflight <- function(url, directory, head_request = function(url, config, handle) httr::HEAD(url, config, httr::timeout(20), handle=handle),
                             index_request = function(url, path, config, handle) httr::GET(url, config,
                               httr::config(maxfilesize_large = 5 * 1024^2), httr::write_disk(path), httr::timeout(30), handle = handle)) {
  progress <- function(n, message) writeLines(c(as.character(n), message), file.path(directory,"stage.txt"))
  progress(10,"Checking public HTTPS access")
  target <- public_sample_target(url)
  # Pin the vetted address and disallow redirects, cookies, authentication and proxies.
  config <- httr::config(resolve=paste0(target$host,":443:",target$ip), followlocation=FALSE, proxy="", netrc=0L)
  handle <- httr::handle(url)
  head <- head_request(url, config, handle)
  if (httr::status_code(head) != 200L) stop("Direct anonymous access failed. Use the final public file URL; portals, redirects and login pages need manual review.")
  progress(60,"Checking the download-link response")
  headers <- httr::headers(head)
  type <- headers[["content-type"]]
  disposition <- headers[["content-disposition"]]
  path <- httr::parse_url(url)$path
  if (grepl("\\.geojson$", path, ignore.case = TRUE)) {
    size <- suppressWarnings(as.numeric(headers[["content-length"]]))
    if (length(size) != 1L || !is.finite(size) || size <= 0 || size > 5 * 1024^2)
      stop("GeoJSON index requires a known size up to 5 MiB. Split larger indexes by campaign or region.")
    progress(60, "Reading the small tile index; no point clouds")
    file <- file.path(directory, "index.geojson")
    on.exit(unlink(file), add = TRUE)
    response <- index_request(url, file, config, handle)
    if (httr::status_code(response) != 200L || !file.exists(file) || file.size(file) != size)
      stop("Incomplete tile-index response.")
    tiles <- read_tile_index(file)
    progress(90, "Tile footprints and metadata are structurally compatible")
    return(list(summary = paste(nrow(tiles), "tile footprints and metadata passed the index structure check. No point-cloud bytes downloaded or plotted. Individual asset access, license and survey accuracy require maintainer review; not publication approval.")))
  }
  named_file <- grepl("\\.(las|laz)$", path, ignore.case=TRUE) ||
    (!is.null(disposition) && grepl("\\.(las|laz)([\"; ]|$)",disposition,ignore.case=TRUE))
  if (!is.null(type) && grepl("text/html",type,ignore.case=TRUE))
    stop("The link opens a web page. Provide a direct LAS/LAZ link, or submit the portal/index for manual integration review.")
  if (!isTRUE(named_file)) stop("The endpoint responds, but its headers/path do not identify a LAS/LAZ download. Submit for manual review; no file was downloaded.")
  progress(90,"Preparing the connection report")
  list(summary="Connection check complete: public HTTPS endpoint responds and identifies a LAS/LAZ download. No point-cloud bytes downloaded, decoded or plotted. Ready for maintainer review; file contents, license and integration are not verified.")
}

source_preflight_ui <- function() shiny::tagList(
  shiny::helpText("Checks public file access or a small GeoJSON index. No point clouds are downloaded."),
  shiny::actionButton("source_test", "Check compatibility"),
  shiny::actionButton("source_test_cancel", "Cancel test"),
  shiny::uiOutput("source_test_progress"),
  shiny::helpText("A successful check is not approval for publication."))

source_preflight_server <- function(input, output, session, state, mode, hosted_lock) {
  check <- shiny::reactiveValues(job=NULL,directory=NULL,locked=FALSE,percent=0,message="Optional: check the source connection without downloading data.",summary="Not tested",ready=FALSE)
  cleanup <- function() {
    if (!is.null(check$job) && check$job$is_alive()) check$job$kill_tree()
    check$job <- NULL
    if (!is.null(check$directory)) unlink(check$directory,recursive=TRUE)
    check$directory <- NULL
    if (isTRUE(check$locked)) unlink(hosted_lock,recursive=TRUE)
    check$locked <- FALSE
    state$source_test_busy <- FALSE
  }
  reset <- function() {cleanup();check$ready <- FALSE;check$percent <- 0;check$summary <- "Not tested";check$message <- "Check the dataset link, or submit for manual review."}
  shiny::observeEvent(lapply(c("source_email","source_origin","source_boundary","source_year","source_platform","source_url","source_license_url","source_open_license","source_repository_confirm"),function(id)input[[id]]), reset(),ignoreNULL=FALSE)
  shiny::observeEvent(input$source_test_cancel,reset())
  shiny::observeEvent(input$source_test, {
    request <- source_request(input)
    if(!request$valid){check$message <- request$message;return()}
    if(!isTRUE(input$source_repository_confirm)){check$message <- "Confirm stable public repository access.";return()}
    if (isTRUE(state$comparison_busy) || isTRUE(state$source_test_busy) || (!is.null(state$job) && state$job$is_alive()) || (!is.null(state$preview_job) && state$preview_job$is_alive())) {check$message <- "Wait for the current transfer or preview.";return()}
    reset()
    tryCatch({
      if(mode == "hosted") {
        if(!dir.create(hosted_lock,showWarnings=FALSE)) stop("Another hosted transfer is running. Try later.")
        check$locked <- TRUE
      }
      check$directory <- tempfile("als-source-test-");dir.create(check$directory)
      state$source_test_busy <- TRUE;check$percent <- 5;check$message <- "Starting connection check"
      check$job <- background_job("source_preflight", list(trimws(input$source_url), check$directory))
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
      result <- job$get_result();check$ready <- TRUE;check$percent <- 100;check$summary <- result$summary;check$message <- result$summary
    },error=function(e){check$summary <- "Technical test incomplete; manual review required";check$message <- paste("Test incomplete:",conditionMessage(e))})
    cleanup()
  })
  output$source_test_progress <- shiny::renderUI(shiny::tagList(
    shiny::div(class="source-test-track",
      shiny::span(class=paste("source-test-gator",if(check$percent>0 && check$percent<100)"walking" else ""),
        style=paste0("left:",check$percent,"%"),shiny::HTML('<svg viewBox="0 0 120 50" aria-hidden="true"><path d="M5 30 L25 14 L30 26 Q45 10 76 22 L111 22 Q118 24 112 29 L84 35 Q55 42 28 33 Z" fill="#71b57b" stroke="#173e32" stroke-width="2"/><path d="M38 17l5-6 5 5 6-7 5 7 6-4 5 6" fill="#388a61"/><circle cx="80" cy="20" r="6" fill="#71b57b"/><circle cx="82" cy="19" r="2" fill="#101f23"/><path d="M88 29h23" stroke="#173e32" stroke-width="2"/><g class="gator-legs" stroke="#71b57b" stroke-width="6" stroke-linecap="round"><path d="M40 34l-7 10h10M69 35l7 9h10"/></g></svg>')),
      shiny::span(class="source-test-lake",shiny::HTML('<svg viewBox="0 0 44 50" aria-hidden="true"><path d="M12 46V4" stroke="#dbe7e9" stroke-width="3"/><path d="M14 5h25L27 14l12 9H14Z" fill="#e88455"/><ellipse cx="20" cy="47" rx="17" ry="3" fill="#397997"/></svg>'))),
    shiny::tags$progress(value=check$percent,max=100,`aria-label`="Technical test progress"),
    shiny::p(role="status",paste0(check$percent,"% - ",check$message))))
  session$onSessionEnded(function()shiny::isolate(cleanup()))
  shiny::reactive(list(summary=check$summary,ready=check$ready))
}

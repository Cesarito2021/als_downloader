# Administrator-only configuration; never populated from submission fields.
zenodo_mail_config <- function() getOption("alsdownloader.submission_mail", NULL)

zenodo_mail_message <- function(p, config) {
  address <- function(x) is.character(x) && length(x)==1L && !is.na(x) &&
    grepl("^[A-Za-z0-9.!#$%&'*+/=?^_`{|}~-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$",x)
  if(!address(config$from) || !address(config$to)) stop("Configure valid notification sender and recipient addresses.")
  base <- config$review_url
  if(!is.character(base) || length(base)!=1L || is.na(base) ||
     !grepl("^http://(127\\.0\\.0\\.1|localhost)(:[0-9]+)?/$",base))
    stop("Use the localhost reviewer app URL, ending in /.")
  clean <- function(x) substr(gsub("[[:cntrl:]]"," ",paste(x,collapse=" ")),1,500)
  body <- paste("A new LiDAR source is awaiting your review.",
    paste("Title:",clean(p$metadata$title)), paste("DOI:",clean(p$metadata$doi)),
    paste("Contact:",if(nzchar(p$contact_email))clean(p$contact_email) else "Not provided"),
    paste("Acquisition:",if(nzchar(p$acquired))clean(p$acquired) else "Unknown"),
    paste("Platform:",clean(p$platform)), paste("Mapped assets:",length(p$index$features)),
    paste("Coverage:",zenodo_coverage_label(p)),
    paste("Review:",paste0(base,"?zenodo_review=",p$id)),
    "Open ALS Downloader on your PC, review the proposal, then choose Approve and add to catalogue.",
    "Opening this link does not approve or download anything.",sep="\r\n")
  paste0("From: ",config$from,"\r\nTo: ",config$to,
    "\r\nSubject: ALS Downloader - new source ",substr(p$id,1,8),
    "\r\nMIME-Version: 1.0\r\nContent-Type: text/plain; charset=UTF-8",
    "\r\nContent-Transfer-Encoding: base64\r\n\r\n",
    gsub("(.{76})","\\1\r\n",gsub("[\r\n]","",jsonlite::base64_enc(charToRaw(enc2utf8(body))))),"\r\n")
}

zenodo_mail_send <- function(message, config) {
  server <- config$smtp_server
  if(!is.character(server) || length(server)!=1L || is.na(server) ||
     !grepl("^smtps?://[^/@[:space:]]+(:[0-9]+)?/?$",server))
    stop("Configure an SMTP server without embedded credentials.")
  curl::send_mail(mail_from=config$from,mail_rcpt=config$to,message=message,
    smtp_server=server,username=Sys.getenv("ALS_SMTP_USERNAME"),
    password=Sys.getenv("ALS_SMTP_PASSWORD"),use_ssl="force",timeout=20,verbose=FALSE)
  invisible(TRUE)
}

# Called under the per-request submission lock. Resubmission retries a failed
# notification but never sends again after a recorded success.
zenodo_notify <- function(queue,id) {
  config <- zenodo_mail_config()
  if(is.null(config)) return(invisible("disabled"))
  receipt <- file.path(queue,"notifications",paste0(id,".json"))
  if(file.exists(receipt) && identical(jsonlite::fromJSON(receipt)$status,"sent"))
    return(invisible("sent"))
  if(file.exists(file.path(queue,"decisions",paste0(id,".json")))) return(invisible("reviewed"))
  p <- zenodo_proposal(queue,id)
  status <- tryCatch({
    message <- zenodo_mail_message(p,config)
    eml <- file.path(queue,"notifications",paste0(id,".eml"))
    dir.create(dirname(eml),recursive=TRUE,showWarnings=FALSE)
    writeBin(charToRaw(message),eml)
    if(isTRUE(config$preview)) "preview" else {zenodo_mail_send(message,config);"sent"}
  },error=function(e) "failed")
  # Do not store SMTP diagnostics: they can contain credentials or server details.
  zenodo_write(list(id=id,status=status,updated_at=format(Sys.time(),tz="UTC",usetz=TRUE)),receipt)
  invisible(status)
}

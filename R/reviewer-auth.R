# Host-owned allowlist; never populated from contribution fields or browser input.
reviewer_credentials_path <- function() getOption("alsdownloader.reviewer_credentials",
  Sys.getenv("ALS_REVIEWER_CREDENTIALS", ""))

reviewer_credentials <- function(path=reviewer_credentials_path()) {
  tryCatch({
    if(length(path)!=1L || !is.character(path) || !nzchar(path) ||
       !file.exists(path) || file.size(path)>16384) return(NULL)
    x <- readRDS(path)
    if(!identical(x$schema,"als-reviewer-email-v1") || !is.character(x$emails) ||
       !length(x$emails) || length(x$emails)>10L || anyNA(x$emails) ||
       any(!grepl("^[A-Za-z0-9.!#$%&'*+/=?^_`{|}~-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$",x$emails)) ||
       !is.character(x$revision) || length(x$revision)!=1L || is.na(x$revision)) return(NULL)
    x$emails <- unique(tolower(trimws(x$emails)))
    x$fingerprint <- digest::digest(x,algo="sha256")
    x
  },error=function(e)NULL)
}

#' Configure email-link access to local proposal review
#' @param path Private allowlist-file path outside public assets and Git.
#' @param emails Administrator mailboxes allowed to receive review invitations.
#' @return Invisibly, the allowlist-file path. No password is requested or stored.
#'   Set option `alsdownloader.reviewer_credentials` or environment variable
#'   `ALS_REVIEWER_CREDENTIALS` to this path at launch.
#' @details Access requires a single-use private review link delivered to an allowed mailbox by
#'   the administrator-configured mail service. Preview mail cannot authenticate.
#'   Reconfiguration revokes sessions and outstanding invitations. Restrict the file's
#'   parent directory to the host owner (use Windows ACLs where applicable).
#'   The reviewer remains localhost-only; remote HTTPS hosting is separate.
#' @export
configure_reviewer_access <- function(path, emails) {
  if(!requireNamespace("openssl",quietly=TRUE)) stop("Install openssl to configure reviewer access.")
  if(!is.character(emails) || !length(emails) || length(emails)>10L || anyNA(emails))
    stop("Provide the administrator mailboxes.")
  emails <- unique(tolower(trimws(emails)))
  if(any(!grepl("^[A-Za-z0-9.!#$%&'*+/=?^_`{|}~-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$",emails))) stop("Check the email addresses.")
  if(!is.character(path) || length(path)!=1L || is.na(path) || !nzchar(path)) stop("Provide a private allowlist-file path.")
  dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
  x <- list(schema="als-reviewer-email-v1",emails=emails,
    revision=paste(sprintf("%02x",as.integer(openssl::rand_bytes(16))),collapse=""))
  tmp <- tempfile(tmpdir=dirname(path)); on.exit(unlink(tmp),add=TRUE)
  saveRDS(x,tmp); Sys.chmod(tmp,"0600")
  if(!file.copy(tmp,path,overwrite=TRUE)) stop("Could not save the private reviewer allowlist.")
  Sys.chmod(path,"0600")
  invisible(path)
}

# Tokens are bound to one proposal and delivered only by the configured mailer.
reviewer_invitation <- function(queue,id,email,now=as.numeric(Sys.time())) {
  cfg <- reviewer_credentials()
  if(is.null(cfg) || !tolower(email) %in% cfg$emails) stop("Configure the notification recipient as a reviewer.")
  secret <- paste(sprintf("%02x",as.integer(openssl::rand_bytes(32))),collapse="")
  zenodo_write(list(hash=digest::digest(secret,algo="sha256"),email=tolower(email),
    expires=now+30*86400,fingerprint=cfg$fingerprint,used=FALSE),
    file.path(queue,"invitations",paste0(id,".json")))
  secret
}

reviewer_access_controller <- function(queue=NULL,read_config=reviewer_credentials,
  now=function()as.numeric(Sys.time())) {
  read_invitation <- function(id) tryCatch({
    if(is.null(queue)||length(id)!=1L||is.na(id)||!grepl("^[a-f0-9]{64}$",id))return(NULL)
    receipt <- file.path(queue,"notifications",paste0(id,".json"))
    if(!file.exists(receipt)||!identical(jsonlite::fromJSON(receipt)$status,"sent"))return(NULL)
    if(file.exists(file.path(queue,"decisions",paste0(id,".json"))))return(NULL)
    jsonlite::fromJSON(file.path(queue,"invitations",paste0(id,".json")))
  },error=function(e)NULL)
  list(login=function(id,secret) {
    if(!is.character(secret)||length(secret)!=1L||is.na(secret)||!grepl("^[a-f0-9]{64}$",secret))return(NULL)
    x <- read_invitation(id); cfg <- read_config()
    if(is.null(x)||is.null(cfg)||isTRUE(x$used)||now()>=x$expires||
       !identical(x$fingerprint,cfg$fingerprint)||!x$email %in% cfg$emails||
       !identical(x$hash,digest::digest(secret,algo="sha256")))return(NULL)
    # The local reviewer runs in one process; synchronous exchange consumes the link.
    x$used <- TRUE
    zenodo_write(x,file.path(queue,"invitations",paste0(id,".json")))
    list(id=id,email=x$email,expires=now()+1800,fingerprint=cfg$fingerprint)
  },valid=function(token) {
    if(is.null(token)||now()>=token$expires)return(FALSE)
    cfg <- read_config(); x <- read_invitation(token$id)
    !is.null(cfg)&&!is.null(x)&&identical(cfg$fingerprint,token$fingerprint)&&token$email %in% cfg$emails
  })
}

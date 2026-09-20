# Credentials are host configuration, never contribution data or browser inputs.
reviewer_credentials_path <- function() getOption("alsdownloader.reviewer_credentials",
  Sys.getenv("ALS_REVIEWER_CREDENTIALS", ""))

reviewer_credentials <- function(path=reviewer_credentials_path()) {
  tryCatch({
    if(length(path)!=1L || !is.character(path) || !nzchar(path) ||
       !file.exists(path) || file.size(path)>16384) return(NULL)
    x <- readRDS(path)
    if(!identical(x$schema,"als-reviewer-v1") || !is.character(x$emails) ||
       !length(x$emails) || length(x$emails)>10L || anyNA(x$emails) ||
       any(!grepl("^[^[:space:]@]+@[^[:space:]@]+\\.[^[:space:]@]+$",x$emails)) ||
       !is.character(x$hash) || length(x$hash)!=1L || is.na(x$hash) ||
       !startsWith(x$hash,"$7$")) return(NULL)
    x$emails <- unique(tolower(trimws(x$emails)))
    x$fingerprint <- digest::digest(x,algo="sha256")
    x
  },error=function(e)NULL)
}

reviewer_store_credentials <- function(path, emails, password) {
  if(!requireNamespace("sodium",quietly=TRUE)) stop("Install sodium to configure reviewer access.")
  if(!is.character(password) || length(password)!=1L || is.na(password) ||
     nchar(password)<15L || nchar(password)>256L) stop("Use a password of 15 to 256 characters.")
  if(!is.character(emails) || !length(emails) || length(emails)>10L || anyNA(emails))
    stop("Provide the administrator email identifiers.")
  emails <- unique(tolower(trimws(emails)))
  if(any(!grepl("^[^[:space:]@]+@[^[:space:]@]+\\.[^[:space:]@]+$",emails))) stop("Check the email identifiers.")
  if(!is.character(path) || length(path)!=1L || !nzchar(path)) stop("Provide a private credential-file path.")
  dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
  # Explicit configuration may replace an existing password and revokes sessions.
  x <- list(schema="als-reviewer-v1",emails=emails,hash=sodium::password_store(password))
  tmp <- tempfile(tmpdir=dirname(path)); on.exit(unlink(tmp),add=TRUE)
  saveRDS(x,tmp); Sys.chmod(tmp,"0600")
  if(!file.copy(tmp,path,overwrite=TRUE)) stop("Could not save private reviewer credentials.")
  Sys.chmod(path,"0600")
  invisible(path)
}

#' Configure password-protected local proposal review
#' @param path Private credential-file path outside public assets and Git.
#' @param emails Administrator email identifiers allowed to use this password.
#' @return Invisibly, the credential-file path. Stores a salted password hash,
#'   never the plaintext password. Set option `alsdownloader.reviewer_credentials`
#'   or environment variable `ALS_REVIEWER_CREDENTIALS` to this path at launch.
#' @details Prompts twice using askpass. These are local login identifiers, not
#'   verification of mailbox ownership or a connection to Gmail/Outlook. No email
#'   is sent. Reconfiguration revokes existing reviewer sessions. Keep the file
#'   in an owner-restricted directory (Windows requires appropriate directory ACLs).
#'   The reviewer app remains localhost-only; this is not remote hosting setup.
#' @export
configure_reviewer_access <- function(path, emails) {
  if(!requireNamespace("askpass",quietly=TRUE)) stop("Install askpass for private password entry.")
  password <- askpass::askpass("Create an ALS Downloader administrator password (15+ characters; not your email password):")
  if(is.null(password)) stop("Password setup cancelled.")
  confirmation <- askpass::askpass("Repeat the ALS Downloader administrator password:")
  if(!identical(password,confirmation)) stop("Passwords did not match; nothing was saved.")
  reviewer_store_credentials(path,emails,password)
}

reviewer_access_controller <- function(read_config=reviewer_credentials, now=function()as.numeric(Sys.time())) {
  failures <- 0L; blocked_until <- 0
  list(login=function(email,password) {
    if(now()<blocked_until) return(NULL)
    cfg <- read_config()
    if(is.null(cfg) || !requireNamespace("sodium",quietly=TRUE)) return(NULL)
    valid_input <- is.character(email)&&length(email)==1L&&!is.na(email)&&nchar(email)<=254L&&
      is.character(password)&&length(password)==1L&&!is.na(password)&&nchar(password)<=256L
    # Verify even for unknown identifiers, avoiding an account-existence shortcut.
    verified <- valid_input && tryCatch(sodium::password_verify(cfg$hash,password),error=function(e)FALSE)
    if(!verified || !tolower(trimws(email)) %in% cfg$emails) {
      failures <<- failures+1L
      if(failures>=5L) {blocked_until <<- now()+60; failures <<- 0L}
      return(NULL)
    }
    failures <<- 0L
    list(email=tolower(trimws(email)),expires=now()+1800,fingerprint=cfg$fingerprint)
  },valid=function(token) {
    if(is.null(token) || now()>=token$expires) return(FALSE)
    cfg <- read_config()
    !is.null(cfg) && identical(cfg$fingerprint,token$fingerprint) && token$email %in% cfg$emails
  })
}

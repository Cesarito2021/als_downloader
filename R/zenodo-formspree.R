# Public submission endpoint only: never distribute a Formspree management key.
zenodo_formspree_config <- function(default = TRUE) {
  config <- getOption("alsdownloader.formspree", NULL)
  if (identical(config, FALSE)) return(NULL)
  if (is.null(config)) {
    endpoint <- Sys.getenv("ALS_FORMSPREE_ENDPOINT", "")
    if (!nzchar(endpoint)) {
      if (!isTRUE(default)) return(NULL)
      endpoint <- "https://formspree.io/f/xdekaglo"
    }
    config <- list(endpoint = endpoint, attachments = FALSE)
  }
  if (!is.list(config) || !is.character(config$endpoint) ||
      length(config$endpoint) != 1L || is.na(config$endpoint) ||
      !grepl("^https://formspree\\.io/f/[A-Za-z0-9]+$", config$endpoint))
    stop("Configure a public https://formspree.io/f/ endpoint; do not use an API key.")
  list(endpoint = config$endpoint, attachments = isTRUE(config$attachments))
}

zenodo_formspree_fields <- function(p, source, config) {
  fields <- list(subject = paste("[ALS Downloader] Zenodo submission", p$metadata$doi),
    reference = p$id, doi = p$metadata$doi,
    record = paste0("https://zenodo.org/records/", p$metadata$id),
    acquisition = p$acquired, platform = p$platform,
    coverage = zenodo_coverage_label(p),
    files = paste(vapply(p$index$features, function(f) f$properties$file_key, ""), collapse = "\n"),
    review_status = "Pending explicit maintainer approval. This message grants no publication permission.")
  if (nzchar(p$contact_email)) fields$email <- p$contact_email
  approximate <- identical(source$kind, "approximate")
  if (approximate) {
    props <- p$index$features[[1]]$properties
    fields$centre_longitude <- as.character(props$extent_longitude)
    fields$centre_latitude <- as.character(props$extent_latitude)
    fields$centre_to_side_metres <- as.character(props$extent_distance_m)
  } else if (identical(source$kind, "zenodo")) {
    if (!source$key %in% vapply(p$metadata$files, `[[`, "", "key"))
      stop("Coverage must refer to a file in this Zenodo record.")
    fields$boundary <- paste0(fields$record, "/files/", utils::URLencode(source$key, reserved = TRUE))
    if(!is.null(source$id_column)) {
      fields$id_column<-source$id_column
      fields$id_mapping<-as.character(jsonlite::toJSON(as.list(source$id_mapping),auto_unbox=TRUE))
    }
    fields$file_mapping <- if (is.null(source$mapping) || !nzchar(source$mapping))
      "Use the boundary file_key column." else source$mapping
  } else if (!isTRUE(config$attachments)) {
    stop("This submission service accepts Zenodo links. Choose polygons stored in the Zenodo record, or ask the maintainer to enable proposal attachments. Your local proposal can still be saved.")
  }
  fields
}

zenodo_formspree_ui <- function(p, source, config) {
  fields <- zenodo_formspree_fields(p, source, config)
  attachment <- if (isTRUE(config$attachments))
    as.character(jsonlite::toJSON(p, auto_unbox = TRUE, null = "null", digits = NA)) else NULL
  if (!is.null(attachment) && nchar(attachment, type = "bytes") > 8*1024^2)
    stop("The proposal attachment exceeds 8 MiB. Simplify the coverage polygons.")
  shiny::tagList(
    shiny::helpText("Submit sends your dataset information and private email to the ALS Downloader review team through Formspree."),
    shiny::tags$form(class = "als-formspree", action = config$endpoint,
      method = "POST", target = "_blank", rel = "noopener noreferrer",
      enctype = "multipart/form-data",
      lapply(names(fields), function(name) shiny::tags$input(type = "hidden", name = name, value = fields[[name]])),
      shiny::tags$input(type = "text", name = "_gotcha", tabindex = "-1", autocomplete = "off", style = "display:none", `aria-hidden` = "true"),
      if (!is.null(attachment)) shiny::tagList(
        shiny::tags$textarea(class = "als-formspree-proposal", style = "display:none", `aria-hidden` = "true", attachment),
        shiny::tags$input(type = "file", name = "attachment", class = "als-formspree-attachment", style = "display:none", tabindex = "-1")),
      shiny::tags$button(type = "submit", class = "btn btn-default", "Submit for maintainer review"),
      shiny::tags$button(type = "button", class = "btn btn-default als-formspree-retry", style = "display:none", "Retry failed submission"),
      shiny::tags$button(type = "button", class = "btn btn-default als-formspree-verify", style = "display:none", "Continue verification"),
      shiny::tags$p(class = "als-formspree-status", role = "status")))
}

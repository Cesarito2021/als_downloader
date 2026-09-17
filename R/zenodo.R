# This versioned record mixes acquisition methods. Only the reviewed original
# aerial file is eligible; extensions and repository keywords are insufficient.
sila_source <- function(record) {
  if (!identical(as.character(record$id), "3633629")) stop("Unexpected Zenodo record.")
  files <- Filter(function(f) identical(f$key, "merged.las"), record$files)
  if (length(files) != 1L) stop("The reviewed aerial file is unavailable in this record.")
  file <- files[[1]]
  url <- "https://zenodo.org/api/records/3633629/files/merged.las/content"
  if (!identical(file$links$self, url)) stop("The reviewed aerial file link has changed; review required.")
  size <- file$size
  if (length(size) != 1L || !is.numeric(size) || !is.finite(size) || size <= 0)
    stop("Zenodo did not supply a valid file size.")
  if (!identical(record$metadata$license$id, "cc-by-4.0") ||
      !identical(record$metadata$doi, "10.5281/zenodo.3633629"))
    stop("Dataset license or DOI has changed; review required.")
  list(filename = file$key, url = url, size = size, checksum = file$checksum,
    citation = "Puletti, N. (2020). Sila National Park - 3D Point cloud data (Version 1). Zenodo. https://doi.org/10.5281/zenodo.3633629")
}

zenodo_source_ui <- function() {
  shiny::tagList(shiny::h3("Dataset contribution examples"),
    shiny::p("Public aerial LiDAR deposits linked at their original source. Each example retains its DOI and CC BY 4.0 attribution. Files download in full from the provider; no AOI clipping is offered here."),
    shiny::tags$ul(
      shiny::tags$li(shiny::tags$a(href="https://doi.org/10.5281/zenodo.3633629",target="_blank",rel="noopener noreferrer","Sila / Puletti (2020)")," - original aerial LAS; ",shiny::tags$a(href="https://zenodo.org/api/records/3633629/files/merged.las/content","Download")),
      shiny::tags$li(shiny::tags$a(href="https://doi.org/10.5281/zenodo.17492219",target="_blank",rel="noopener noreferrer","Tree-LiMS / Puletti and collaborators")," - UAV LiDAR archive; ",shiny::tags$a(href="https://zenodo.org/api/records/17492219/files/las_singletree_data.zip/content","Download")),
      shiny::tags$li(shiny::tags$a(href="https://doi.org/10.5281/zenodo.7636454",target="_blank",rel="noopener noreferrer","EBA / Ometto and collaborators")," - aircraft LiDAR archive; ",shiny::tags$a(href="https://zenodo.org/api/records/7636454/files/transectos%20para%20p5.zip/content","Download"))),
    shiny::tags$a(href="https://creativecommons.org/licenses/by/4.0/","CC BY 4.0: credit the dataset authors and indicate modifications."))
}

zenodo_source_server <- function(input, output, session) {
  result <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$connect_zenodo, {
    result(NULL)
    result(tryCatch(shiny::withProgress(message = "Connecting to Zenodo", {
      sila_source(request_json("https://zenodo.org/api/records/3633629"))
    }), error = function(e) list(error = "Zenodo connection unavailable or source metadata changed. Retry later or consult the source record; no download link has been enabled.")))
  })
  output$zenodo_access <- shiny::renderUI({
    source <- result()
    if (is.null(source)) return(NULL)
    if (!is.null(source$error)) return(shiny::p(role = "status", source$error))
    shiny::tagList(
      shiny::p(role = "status", sprintf("Connected: %s | %.2f GB (decimal).", source$filename, source$size / 1e9)),
      shiny::p("Downloads the complete source file directly from Zenodo through your browser. No AOI clipping, application storage or in-app preview for this large file. A successful connection verifies metadata, not the full file checksum."),
      shiny::tags$a(id = "zenodo_download", class = "btn als-primary", href = source$url,
        target = "_blank", rel = "noopener noreferrer", "Download original ALS from Zenodo"),
      shiny::p(source$citation),
      shiny::tags$a(href = "https://creativecommons.org/licenses/by/4.0/", target = "_blank", rel = "noopener noreferrer", "Dataset license: CC BY 4.0"),
      shiny::p("Published checksum: ", source$checksum))
  })
}

als_welcome_message <- function() {
  c("##----------------------------------------------------------------##",
    "##                         ALSdownloadeR                           ##",
    "##----------------------------------------------------------------##",
    "An R package with a web-based Shiny app for airborne laser scanning data.",
    "Our mission is to make ALS data more accessible for research.",
    "Discover, visualize and download point clouds from multiple sources.",
    "Access acquisition information and prepare reproducible download scripts.",
    "Developed by C\u00e9sar Alvites at the University of Florida.",
    "Thank you for using ALSdownloadeR.",
    "##---------------------- Suggested citation -----------------------##",
    "Alvites, C. (2026). ALS Downloader: a web-based Shiny application",
    "for the discovery, management, visualization, and download of",
    "airborne laser scanning (ALS) datasets worldwide.",
    "Manuscript in preparation.",
    'For the software reference, use citation("ALSdownloadeR").',
    "Please also cite the original datasets used in your research.",
    "##----------------------------------------------------------------##")
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(als_welcome_message(), collapse = "\n"))
}

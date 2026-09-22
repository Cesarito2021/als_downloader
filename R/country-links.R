country_links_path <- function() getOption("ALSdownloadeR.country_links",
  file.path(tools::R_user_dir("ALSdownloadeR", "config"), "country-links.rds"))

country_link_seed <- function() {
  x <- utils::read.csv(system.file("extdata", "providers.csv", package="ALSdownloadeR"), stringsAsFactors=FALSE)
  x <- x[!x$implemented & x$country_code > 0, ]
  data.frame(country=x$country, country_code=x$country_code, source_id=x$id,
    name=x$name, url=x$info_url, updated_on=x$reviewed_on, notes=x$access,
    stringsAsFactors=FALSE)
}

country_link_store <- function(path) {
  if (!is.character(path) || length(path)!=1L || is.na(path) || !nzchar(path))
    stop("Provide one country-link registry path.", call.=FALSE)
  if (!file.exists(path)) return(list(current=country_link_seed(), history=country_link_seed()[FALSE,]))
  x <- tryCatch(readRDS(path), error=function(e) stop("Cannot read the country-link registry; restore its backup.",call.=FALSE))
  columns <- names(country_link_seed())
  if (!is.list(x) || !all(c("current","history") %in% names(x)) ||
      !all(vapply(x[c("current","history")],function(d) is.data.frame(d) && identical(names(d),columns),logical(1))))
    stop("Invalid country-link registry. The existing file was not changed.",call.=FALSE)
  x
}

#' List persistent country source links
#' @param path Registry file. Defaults to the package's user configuration directory;
#'   override with option `ALSdownloadeR.country_links` for a shared deployment.
#' @param history Return previous revisions instead of current links.
#' @return A data frame of country, country_code, source_id, name, url,
#'   updated_on and notes. The initial records are the packaged external portals.
#' @details Reading does not create a file or contact a website. Country names
#'   follow the bundled world map. These links do not imply national ALS coverage
#'   or enable an in-app download adapter. Restart the app after updating links.
#' @export
#' @examples
#' country_links()
country_links <- function(path=country_links_path(), history=FALSE) {
  if (!is.logical(history) || length(history)!=1L || is.na(history)) stop("history must be TRUE or FALSE.")
  x <- country_link_store(path)
  x[[if(history) "history" else "current"]]
}

#' Add or update an external ALS source link for a country
#' @param country Country name (case-insensitive) or numeric country code from
#'   the bundled world map, for example `"Italy"` or `380`.
#' @param url An HTTPS source portal URL. It is recorded, not fetched or verified.
#' @param updated_on Date supplied by the maintainer, in `YYYY-MM-DD` format.
#'   This is the link review date, not a LiDAR acquisition date.
#' @param source_id Stable source identifier within a country. Use an existing
#'   identifier from country_links() to replace that source's URL.
#' @param name Display name for the portal.
#' @param notes Coverage and access notes. A regional portal is not national coverage.
#' @inheritParams country_links
#' @return The updated current country-link table, invisibly.
#' @details Updates only the matching country/source pair; other entries remain.
#'   Prior revisions are retained in the registry and the previous file is saved
#'   with a `.bak` suffix. A lock rejects simultaneous writers. Registered links
#'   appear in the yellow External Access map layer and source catalogue after
#'   restarting the app, including countries with separate red survey footprints.
#'   No package installation files are modified. Share the registry path with
#'   the app process to use the same links in a hosted installation.
#' @export
#' @examples
#' registry <- tempfile(fileext=".rds")
#' update_country_link("Italy", "https://example.org/lidar", "2026-09-22",
#'   name="Example portal (not a verified source)", path=registry)
#' country_links(path=registry)
#' unlink(c(registry, paste0(registry, ".bak")))
update_country_link <- function(country, url, updated_on, source_id="country_portal",
                                name="ALS source portal", notes="External access; consult source coverage and terms.",
                                path=country_links_path()) {
  scalar <- function(x) is.character(x) && length(x)==1L && !is.na(x) && nzchar(trimws(x))
  if (!scalar(url) || !grepl("^https://[^/?#[:space:]@]+([/?#][^[:space:]]*)?$",url))
    stop("Provide an HTTPS portal URL without embedded credentials.",call.=FALSE)
  if (!scalar(source_id) || !grepl("^[A-Za-z0-9][A-Za-z0-9_.-]*$",source_id)) stop("Use a stable source_id containing letters, numbers, dots, dashes or underscores.")
  if (!scalar(name) || !scalar(notes)) stop("Provide a source name and coverage/access notes.")
  if (!scalar(updated_on) || is.na(parse_als_date(updated_on)) || updated_on!=parse_als_date(updated_on))
    stop("updated_on must be a valid YYYY-MM-DD date.")
  if (length(country)!=1L || is.na(country)) stop("Identify one country.")
  world <- jsonlite::fromJSON(system.file("extdata","world-countries.geojson",package="ALSdownloadeR"))$features
  codes <- as.integer(world$id); countries <- world$properties$name
  found <- which(tolower(countries)==tolower(trimws(as.character(country))) | as.character(codes)==as.character(country))
  if (length(found)!=1L) stop("Country not found in the bundled map; use its country name or numeric code.",call.=FALSE)
  country_link_store(path) # Validate before creating directories or changing anything.
  dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
  lock <- paste0(path,".lock")
  if (!dir.create(lock,showWarnings=FALSE)) stop("Country-link registry is locked by another writer.")
  on.exit(unlink(lock,recursive=TRUE),add=TRUE)
  store <- country_link_store(path)
  row <- data.frame(country=countries[found],country_code=codes[found],source_id=source_id,
    name=name,url=url,updated_on=updated_on,notes=notes,stringsAsFactors=FALSE)
  hit <- which(store$current$country_code==codes[found] & store$current$source_id==source_id)
  if (length(hit)>1L) stop("Duplicate registry key; existing records were not changed.")
  if (length(hit)) {
    old <- store$current[hit,,drop=FALSE]; rownames(old)<-NULL
    if (identical(old,row)) return(invisible(store$current))
    store$history <- rbind(store$history,old)
    store$current[hit,] <- row
  } else store$current <- rbind(store$current,row)
  staged <- tempfile("country-links-",tmpdir=dirname(path))
  on.exit(unlink(staged),add=TRUE)
  saveRDS(store,staged)
  if (file.exists(path) && !file.copy(path,paste0(path,".bak"),overwrite=TRUE)) stop("Could not back up the registry.")
  if (!file.copy(staged,path,overwrite=TRUE)) stop("Could not save the registry; the previous file is in .bak.")
  invisible(store$current)
}

# Overview polygons come only from configured indexes, never country boundaries.
# Simplification is for navigation; search retains the original tile geometry.
coverage_overview <- function(folder = NULL, approved_dir = NULL) {
  paths <- unlist(lapply(Filter(function(x) !is.null(x) && dir.exists(x), list(folder, approved_dir)),
    function(x) list.files(x, "(_TileIndex\\.zip$|\\.tiles\\.geojson$|\\.gpkg$|\\.shp$)", full.names=TRUE, ignore.case=TRUE)))
  items <- lapply(paths, function(path) tryCatch({
    source <- path; tmp <- NULL
    if (grepl("\\.zip$", path, ignore.case=TRUE)) {
      members <- utils::unzip(path, list=TRUE)
      if (sum(members$Length)>500*1024^2 || any(grepl("(^/|^[A-Za-z]:|(^|/)\\.\\.(/|$))",gsub("\\\\","/",members$Name))))
        stop("Unsafe or oversized index archive")
      tmp <- tempfile(); dir.create(tmp); on.exit(unlink(tmp, recursive=TRUE), add=TRUE)
      utils::unzip(path, exdir=tmp)
      source <- list.files(tmp,"\\.(shp|gpkg|geojson)$",recursive=TRUE,full.names=TRUE,ignore.case=TRUE)
      if(length(source)!=1L)stop("Index must contain one geometry file")
    }
    x <- sf::st_read(source,quiet=TRUE)
    if(!nrow(x)||nrow(x)>100000L||is.na(sf::st_crs(x)))return(NULL)
    g <- sf::st_geometry(sf::st_zm(x,drop=TRUE,what="ZM"))
    if(!all(sf::st_geometry_type(g) %in% c("POLYGON","MULTIPOLYGON")))return(NULL)
    # Dissolve in a projected CRS without filling holes or bridging gaps.
    g <- sf::st_make_valid(sf::st_transform(g,3857))
    g <- sf::st_transform(sf::st_simplify(sf::st_union(g),dTolerance=25,preserveTopology=TRUE),4326)
    sf::st_sf(dataset=sub("(_TileIndex\\.zip|\\.tiles\\.geojson|\\.gpkg|\\.shp)$","",basename(path),ignore.case=TRUE),
      geometry=g)
  },error=function(e)NULL))
  items <- Filter(Negate(is.null),items)
  if(!length(items))return(sf::st_sf(dataset=character(),geometry=sf::st_sfc(crs=4326)))
  do.call(rbind,items)
}

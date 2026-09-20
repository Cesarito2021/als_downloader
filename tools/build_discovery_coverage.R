# Maintainer-only: run after the documented index downloads have completed.
# Rscript upstream/tools/build_discovery_coverage.R review/coverage-sources
Sys.unsetenv('LC_ALL')
.libPaths(c(normalizePath('.r-library'),.libPaths()))
folder <- commandArgs(trailingOnly=TRUE)[1]
if(is.na(folder)) stop("Supply a directory containing the complete provider indexes.")
read <- function(name) sf::st_read(file.path(folder,name),quiet=TRUE)
make <- function(x, provider, labels, url, license, credit, dissolve=FALSE, note="Source survey footprints; search confirms current tile availability.") {
  cat(provider,nrow(x),'input footprints\n')
  x <- sf::st_zm(x,drop=TRUE,what="ZM")
  g <- sf::st_make_valid(sf::st_transform(sf::st_geometry(x),3857))
  if(any(sf::st_geometry_type(g)=="GEOMETRYCOLLECTION")) {
    g <- sf::st_sfc(lapply(g,function(part) {
      if(!inherits(part,"GEOMETRYCOLLECTION")) return(part)
      polygons <- sf::st_collection_extract(sf::st_sfc(part,crs=3857),"POLYGON")
      if(!length(polygons)) return(sf::st_multipolygon())
      sf::st_union(polygons)[[1]]
    }),crs=3857)
  }
  if(dissolve) {
    groups <- split(seq_along(g),labels)
    g <- sf::st_sfc(lapply(groups,function(ids)sf::st_union(g[ids])[[1]]),crs=3857)
    labels <- names(groups)
  }
  # 50 m generalization, preserving topology and disjoint regions; no bbox fills.
  g <- sf::st_transform(sf::st_simplify(g,dTolerance=50,preserveTopology=TRUE),4326)
  g <- sf::st_wrap_dateline(g,options=c("WRAPDATELINE=YES","DATELINEOFFSET=180"),quiet=TRUE)
  bad <- which(!sf::st_is_valid(g))
  if(length(bad)) {
    # Snap only residual spherical ring crossings at <= 0.0001 degrees
    # (about 11 m), below this display overview's simplification tolerance.
    g[bad] <- sf::st_set_precision(sf::st_make_valid(sf::st_set_precision(g[bad],1e4)),0)
  }
  if(any(!sf::st_is_valid(g))) stop("Invalid display footprint after repair: ",provider)
  keep <- !sf::st_is_empty(g)
  sf::st_sf(dataset=as.character(labels[keep]),provider=provider,info_url=url,citation=credit,
    license_url=license,reviewed_on="2026-09-19",coverage_note=note,geometry=g[keep])
}
us <- read('usgs.geojson')
ca <- read('canada.geojson')
nl <- read('ahn6-complete.geojson')
fr <- read('france-complete.geojson')
ch <- read('swiss-complete.geojson')
items <- list(
  make(us,'usgs3dep',us$name,'https://registry.opendata.aws/usgs-lidar/',
    'https://www.usgs.gov/information-policies-and-instructions/copyrights-and-credits',
    'USGS 3DEP; public EPT survey footprints maintained by Hobu. COPC availability in Planetary Computer is verified when searching.',
    note='USGS survey coverage from the public EPT index. The app searches the Planetary Computer COPC catalogue; availability may differ.'),
  make(ca,'canelevation',ca$project,'https://open.canada.ca/data/en/dataset/7069387e-9986-4297-9f55-0288e9676947',
    'https://open.canada.ca/en/open-government-licence-canada',
    'Natural Resources Canada, CanElevation. Contains information licensed under the Open Government Licence - Canada.'),
  make(nl,'ahn6',rep('AHN6',nrow(nl)),'https://www.ahn.nl/dataroom',
    'https://creativecommons.org/licenses/by/4.0/','AHN6; available point-cloud tile index. Generalized for display.',TRUE),
  make(fr,'ignfr',ifelse(is.na(fr$survey)|!nzchar(fr$survey),'IGN LiDAR HD',fr$survey),
    'https://api.stac.teledetection.fr/collections/lidarhd','https://www.data.gouv.fr/pages/legal/licences/etalab-2.0',
    paste('IGN LiDAR HD; index maintained by UMR TETIS / INRAE. Index retrieved 2026-09-19. Source edition dates:',
      paste(range(fr$updated,na.rm=TRUE),collapse=' to '),'. Generalized footprints for display.'),TRUE),
  make(ch,'swisstopo',rep('swissSURFACE3D',nrow(ch)),
    'https://www.swisstopo.admin.ch/en/height-model-swisssurface3d',
    'https://www.swisstopo.admin.ch/en/terms-of-use-free-geodata-and-geoservices',
    'Federal Office of Topography swisstopo; swissSURFACE3D catalogue tile footprints. Generalized for display.',TRUE)
)
result <- do.call(rbind,items)
saveRDS(result,'upstream/inst/extdata/discovery-coverage.rds',compress='xz')
cat(nrow(result),'overview polygons;',file.info('upstream/inst/extdata/discovery-coverage.rds')$size,'bytes\n')

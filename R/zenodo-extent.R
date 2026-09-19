# A declared search extent, never a computed point-cloud footprint.
zenodo_square <- function(longitude,latitude,distance_m,file_keys) {
  scalar<-function(x)is.numeric(x)&&length(x)==1L&&!is.na(x)&&is.finite(x)
  if(!scalar(longitude)||abs(longitude)>180||!scalar(latitude)||abs(latitude)>85)
    stop("Enter longitude (-180 to 180) and latitude (-85 to 85) in decimal degrees.")
  if(!scalar(distance_m)||distance_m<1||distance_m>50000)
    stop("Distance from centre to each side must be 1 to 50,000 metres. For larger areas, provide coverage polygons.")
  if(!is.character(file_keys)||!length(file_keys)||anyNA(file_keys)||any(!nzchar(file_keys)))
    stop("Select the point-cloud files or archives described by this approximate square.")
  crs<-sprintf("+proj=aeqd +lat_0=%.12f +lon_0=%.12f +datum=WGS84 +units=m +no_defs",latitude,longitude)
  r<-distance_m
  ring<-matrix(c(-r,-r,r,-r,r,r,-r,r,-r,-r),ncol=2,byrow=TRUE)
  geom<-sf::st_sfc(sf::st_polygon(list(ring)),crs=crs)
  geom<-sf::st_segmentize(geom,dfMaxLength=max(1,r/10))
  geom<-sf::st_wrap_dateline(sf::st_transform(geom,4326),options=c("WRAPDATELINE=YES","DATELINEOFFSET=180"),quiet=TRUE)
  keys<-unique(file_keys)
  sf::st_sf(file_key=keys,coverage_method="author_approximate_square",
    extent_longitude=longitude,extent_latitude=latitude,extent_distance_m=distance_m,
    geometry=rep(geom,length(keys)))
}

zenodo_coverage_label <- function(p) {
  approximate<-any(vapply(p$index$features,function(f)identical(f$properties$coverage_method,"author_approximate_square"),logical(1)))
  if(approximate) "AUTHOR-DECLARED APPROXIMATE SQUARE: may include areas without LiDAR points. Not a surveyed footprint."
  else "Contributor-provided polygons: coverage/file correspondence requires review."
}

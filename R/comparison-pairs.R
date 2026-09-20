# A pair needs separate acquisition periods and essentially complete coverage
# of the smaller provider tile (1% tolerance for independently digitized edges).
# Spatial intersection is indexed; no dense n-by-n geometry matrix is built.
comparison_tile_pairs <- function(tiles, aoi, minimum = 0.99) {
  empty <- data.frame(a=integer(), b=integer(), fraction=numeric())
  if (is.null(tiles) || nrow(tiles)<2L || is.null(aoi)) return(empty)
  start <- suppressWarnings(as.Date(tiles$acquired_start))
  end <- suppressWarnings(as.Date(tiles$acquired_end))
  valid <- which(!is.na(start) & !is.na(end) & start<=end)
  if(length(valid)<2L) return(empty)
  g <- sf::st_transform(sf::st_make_valid(tiles[valid,]),6933)
  area <- as.numeric(sf::st_area(g))
  roi <- sf::st_union(sf::st_geometry(sf::st_transform(aoi,6933)))
  candidates <- sf::st_intersects(g)
  urls <- redact_url(tiles$url)
  result <- list()
  for(i in seq_along(valid)) {
    js <- candidates[[i]]
    js <- js[js>i]
    for(j in js) {
      ia<-valid[i]; ib<-valid[j]
      if (!(end[ia]<start[ib] || end[ib]<start[ia]) || identical(urls[ia],urls[ib])) next
      denominator<-min(area[i],area[j])
      if(!is.finite(denominator) || denominator<=0) next
      shared<-suppressWarnings(sf::st_intersection(sf::st_geometry(g[i,]),sf::st_geometry(g[j,])))
      fraction<-sum(as.numeric(sf::st_area(shared)))/denominator
      if(!is.finite(fraction) || fraction+1e-8<minimum) next
      inside<-suppressWarnings(sf::st_intersection(shared,roi))
      if(!length(inside) || sum(as.numeric(sf::st_area(inside)))<=0) next
      if(start[ia]>start[ib]) {tmp<-ia;ia<-ib;ib<-tmp}
      result[[length(result)+1L]]<-data.frame(a=ia,b=ib,fraction=min(1,fraction))
    }
  }
  if(length(result)) do.call(rbind,result) else empty
}

comparison_tile_choices <- function(tiles, rows) {
  if(!length(rows)) return(character())
  stats::setNames(as.character(rows), paste(tiles$filename[rows], tiles$acquired_start[rows], tiles$dataset[rows],sep=" | "))
}

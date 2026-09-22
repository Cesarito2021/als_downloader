zenodo_fixture <- function(keys="survey.laz") {
  zenodo_metadata(list(id=12345,metadata=list(title="Fictitious aerial survey",doi="10.5281/zenodo.12345",
    access_right="open",publication_date="2024-06-01",creators=list(list(name="Example Author")),
    license=list(id="cc-by-4.0"),description="Synthetic test only",notes="Example credits"),
    files=lapply(keys,function(key)list(key=key,size=1000,checksum="md5:example"))))
}
zenodo_shape <- function() sf::st_sf(geometry=sf::st_as_sfc(sf::st_bbox(c(xmin=1,ymin=1,xmax=1.01,ymax=1.01),crs=4326)))

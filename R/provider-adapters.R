# Provider names identify protocols/products, rather than assuming a country
# has one universal catalogue. Shared STAC transport stays in search_europe().
find_als_ahn <- function(aoi,max_items) search_europe(aoi,"ahn6",max_items)
find_als_ign_france <- function(aoi,max_items) search_europe(aoi,"ignfr",max_items)
find_als_swisstopo <- function(aoi,max_items) search_europe(aoi,"swisstopo",max_items)
find_als_canelevation <- function(aoi,tile_index_dir,max_items) search_canelevation(aoi,tile_index_dir,max_items)
find_als_opentopography <- function(aoi,tile_index_dir,max_items) search_ot(aoi,tile_index_dir,max_items)

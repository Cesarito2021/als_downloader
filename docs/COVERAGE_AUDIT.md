# Coverage eligibility audit

This document records the initial coverage exclusions and the subsequent
France/Spain restoration. The catalogue has since expanded to 18 entries
following the [supplied-document review](DOCUMENT_SOURCE_REVIEW.md). Spain
(PNOA) and Australia (AUS11_Victor) were later removed on 18 September 2026;
see [ACTIVE_SOURCES.md](ACTIVE_SOURCES.md) for the current 36-entry catalogue.

Reviewed 17 September 2026. Initial scope: all 10 previously active catalogue entries. Subsequently restored France and Spain as official-portal access, following the user's clarified scope.
Retain research deposits with verified polygon coverage/index. Also expose official national portals documenting spatial downloads; this does not claim a locally verified index or an implemented adapter. Spatial evidence includes
STAC geometry and equivalent official services. A point Shapefile, LAS filename,
country label or successful download alone is insufficient. No new polygons
were inferred from point-cloud coordinates for this review.

## Earlier review: nine entries, eight source/products and one index service

| Entry | Spatial evidence | Integration status |
|---|---|---|
| USGS 3DEP | [STAC catalogue](https://planetarycomputer.microsoft.com/dataset/3dep-lidar-copc): item geometry linked to point-cloud assets; existing AOI adapter and prior live search evidence. | AOI search implemented. |
| OpenTopography | [Official tile-index workflow](https://opentopography.org/node/3598). Service entry, not an additional survey. | Requires locally supplied indexes. |
| AUS11_Victor | Local `AUS11_Victor_TileIndex.zip` contains SHP, SHX, DBF and PRJ. Previously exercised by the tile-index adapter. | Retained. |
| BR17_SaoPaulo | Local `BR17_SaoPaulo_TileIndex.zip` contains SHP, SHX, DBF and PRJ. Previously exercised by the tile-index adapter. | Retained. |
| Auckland_2013 | Local `Auckland_2013_TileIndex.zip` contains SHP, SHX, DBF and PRJ. Previously exercised by the tile-index adapter. | Retained. |
| IGN LiDAR HD, France | [Official delivery specification](https://geoservices.ign.fr/sites/default/files/2024-09/DL_LiDAR_HD_1-0.pdf) documents spatial selection and downloads by kilometre tile. | Official portal access; in-app adapter pending. |
| PNOA LiDAR, Spain | [Official product page](https://pnoa.ign.es/pnoa-lidar/productos-a-descarga) describes LAZ tiles by campaign (2 km, with exceptions, or 1 km for the third coverage). | Official portal access; in-app adapter pending. |
| CanElevation | [Official record](https://open.canada.ca/data/en/dataset/7069387e-9986-4297-9f55-0288e9676947) explicitly lists Projects Vector Data SHP and Tiles Vector Data SHP. | Coverage resources confirmed; AOI adapter remains pending. |
| swissSURFACE3D | [STAC item](https://data.geo.admin.ch/api/stac/v0.9/collections/ch.swisstopo.swisssurface3d/items/swisssurface3d_2015_2494-1140) has a Polygon and its LAS ZIP asset. Inspected local copy of the official response. | Polygon-to-asset association confirmed; AOI adapter remains pending. |

Retention does not claim continuous country coverage, that every asset was
tested, or that all nine entries have an implemented AOI download workflow.

## Excluded from active catalogue, country shading and download examples

| Record | Findings | Decision |
|---|---|---|
| [Sila / Puletti](https://doi.org/10.5281/zenodo.3633629) | Full API file listing includes `GPSpoints_EPSG_32633.shp` and companions. Its binary Shapefile header reports geometry type 1 (Point), not polygon coverage. No separate polygon index listed. | Pending provider polygon coverage/index. |
| [Tree-LiMS](https://doi.org/10.5281/zenodo.17492219) | Four top-level files: two Rdata objects, an R script and a LAS ZIP. The complete ZIP directory contains 113 entries and no SHP/GPKG/GeoJSON/KML/JSON files. | No verified polygon index; excluded. Rdata objects were not executed or treated as coverage. |
| [EBA](https://doi.org/10.5281/zenodo.7636454) | Eleven ZIP files. All eleven central directories, including ZIP64, were read completely; no SHP/GPKG/GeoJSON/KML/JSON coverage files found. | No verified polygon index; excluded. |

The audit read API file metadata, 100 bytes of the Sila Shapefile header,
and bounded ZIP directory ranges. It did not download or decode the clouds.
[Machine-readable archive evidence](coverage-archive-evidence.json).
No verified index in this review does not prove that an index cannot exist
elsewhere or be supplied later. Original attribution and DOI links are kept
here for traceability; these records are not offered as active sources.

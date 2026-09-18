# Active source review

Follow-up [European live checks](../inst/sources/VALIDATION.md): French spatial
metadata and official COPC headers now pass; Spain's anonymous download-init
returned 403, so PNOA LiDAR was removed from the active catalogue on 18
September 2026 pending a working transfer check. France has no new in-app
adapter. Luxembourg and Wallonia were additionally tested but are not new
active catalogue entries.

OpenTopography AUS11_Victor (Australia) was also removed on 18 September
2026: the provider supplies no reuse licence for the dataset, so access
cannot be presented as guaranteed even though the technical adapter and a
sample download passed.

Reviewed 18 September 2026. **16 active catalogue entries**, including one general index service. Entries are source/products, not counts of individual tiles or completed national adapters. The [document review](DOCUMENT_SOURCE_REVIEW.md) records the expanded scope.

The historical inventory contains 46 rows, including repeated sources, discovery pages and excluded products. It is retained only as an audit archive; it does not populate the app. Of 40 distinct page links rechecked, 38 responded successfully. HTTP reachability alone was not sufficient for retention.

Nine representative file endpoints were checked using at most 1,024 bytes each (LAS/LAZ or ZIP signatures); three were Zenodo examples, now excluded by the coverage requirement. This is an access check, not a full-file checksum or a guarantee that every tile works. Existing decoded sample evidence remains in [SOURCE_AUDIT.md](SOURCE_AUDIT.md). See [current endpoint results](release-access-checks.json).

| Active entry | Official source | App access |
|---|---|---|
| USGS 3DEP | [Source](https://planetarycomputer.microsoft.com/dataset/3dep-lidar-copc) | Public catalog; assets signed at download time |
| OpenTopography | [Source](https://opentopography.org/node/3598) | Local TileIndex archives; license and citation vary by dataset |
| OpenTopography BR17_SaoPaulo | [Source](https://opentopography.org/node/3598) | One sample validated; requires supplied TileIndex and dataset terms review |
| OpenTopography Auckland_2013 | [Source](https://opentopography.org/node/3598) | One sample validated; requires supplied TileIndex and dataset terms review |
| CanElevation | [Source](https://open.canada.ca/data/en/dataset/7069387e-9986-4297-9f55-0288e9676947) | Public COPC header verified; Open Government Licence Canada; adapter pending |
| swissSURFACE3D | [Source](https://www.swisstopo.admin.ch/en/height-model-swisssurface3d) | Native STAC AOI search and original LAS ZIP/COPC download; archived ZIP preview requires local extraction; acquisition dates unknown unless separately verified |
| IGN LiDAR HD | [Source](https://cartes.gouv.fr/rechercher-une-donnee/dataset/IGNF_NUAGES-DE-POINTS-LIDAR-HD) | Official portal: spatial tile selection and original point-cloud downloads; in-app AOI adapter pending |
| AHN6 point clouds | [Source](https://www.ahn.nl/dataroom) | AHN6 native OGC footprint search and original LAZ download; other AHN versions not integrated; collection dates not inferred from filenames |
| Kartverket laser projects | [Source](https://www.geonorge.no/kartdata/datasett-i-geonorge/hoydedata/) | Official project coverage and spatial downloads at Hoydedata; select laser point clouds; in-app adapter pending |
| NLS laser scanning 0.5 p | [Source](https://www.maanmittauslaitos.fi/laserkeilaus-ja-ilmakuvaus) | Open 0.5 p cloud downloads through Mapsite; acquisition coverage GeoPackages; paid 5 p product excluded; in-app adapter pending |
| GUGiK airborne laser clouds | [Source](https://www.geoportal.gov.pl/en/data/lidar-measurements-lidar/) | Official spatial download layers and LIDAR indexes; in-app adapter pending |
| Estonian airborne laser clouds | [Source](https://geoportaal.maaamet.ee/eng/spatial-data/elevation-data/download-elevation-data-p664.html) | Official elevation download service: select aerial laser points and survey year; in-app adapter pending |
| GeoSN Saxony laser clouds | [Source](https://www.geodaten.sachsen.de/downloadbereich-digitale-hoehenmodelle-4851.html) | Saxony only: original LSC LAZ delivered in 2 km ZIP tiles with acquisition information; select LSC not raster models; in-app adapter pending |
| NEON discrete return LiDAR | [Source](https://www.neonscience.org/resources/learning-hub/tutorials/neon-discrete-point-clouds) | DP1.30003.001: 1 km LAZ tiles and spatial metadata; user account/API token required for download; in-app adapter pending |
| ORNL Brazilian forest surveys | [Source](https://daac.ornl.gov/CMS/guides/LiDAR_Forest_Inventory_Brazil.html) | Original LAZ with CSV/KMZ tile inventory; Earthdata access workflow required; authenticated transfer and in-app adapter pending |
| LINZ aerial LiDAR collections | [Source](https://www.linz.govt.nz/products-services/data/types-linz-data/elevation-data/provincial-growth-fund-lidar-programme) | Official programme includes source point clouds and regional coverage; follow point-cloud access channels; in-app adapter pending |

## Removed from the visible catalog

The remaining entries below were excluded by the earlier access review. Several official portals have since been restored as official portal access, with their in-app adapters still pending. Removal does not assert data unavailability.

- [PNOA LiDAR (Spain)](https://pnoa.ign.es/pnoa-lidar/productos-a-descarga): official terms and tile polygon verified, but the anonymous download-init check returned HTTP 403; removed 18 September 2026 pending a working transfer check.
- [OpenTopography AUS11_Victor (Australia)](https://portal.opentopography.org/datasetMetadata?otCollectionID=OT.062013.28354.1): technical adapter and a sample download passed, but the provider supplies no reuse licence; removed 18 September 2026 as access could not be presented as guaranteed.
- [Laserdata Skog](https://www2.lantmateriet.se/en/geodata/our-products/product-list/laser-data-download-forest/): provider changed access conditions in 2026; current terms and authorized delivery require review (see DOCUMENT_SOURCE_REVIEW.md).
- [Scottish Public Sector LiDAR](https://registry.opendata.aws/scottish-lidar/): Public AWS bucket; OGL v3 except phase-2 LAZ non-commercial terms; adapter pending
- [ForestScan Lope - UAV laser scanning subset](https://doi.org/10.5285/88a8620229014e0ebacf0606b302112d): Research collection; aerial UAV-LS subset only; TLS excluded; point sample and adapter pending
- [ELVIS](https://elevation.fsdf.org.au/): Portal ordering workflow; adapter pending

## Coverage requirement

The three previously visible Zenodo examples are no longer active or shown on the map. Their records lack a verified polygon coverage/index. Sila includes GPS points, which do not meet this requirement. See the [coverage audit](COVERAGE_AUDIT.md) for record-level evidence and the retained sources.

[European connector evidence and limits](EUROPE_INTEGRATION.md).

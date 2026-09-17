# Active source review

Reviewed 17 September 2026. **10 active catalog entries**: one general OpenTopography index entry and nine source/product entries. These are not ten individual survey tiles or ten national download adapters.

The historical inventory contains 46 rows, including repeated sources, discovery pages and excluded products. It is retained only as an audit archive; it does not populate the app. Of 40 distinct page links rechecked, 38 responded successfully. HTTP reachability alone was not sufficient for retention.

Nine representative file endpoints were checked using at most 1,024 bytes each (LAS/LAZ or ZIP signatures); three are Zenodo contribution examples. This is an access check, not a full-file checksum or a guarantee that every tile works. Existing decoded sample evidence remains in [SOURCE_AUDIT.md](SOURCE_AUDIT.md). See [current endpoint results](release-access-checks.json).

| Active entry | Official source | App access |
|---|---|---|
| USGS 3DEP | [Provider](https://planetarycomputer.microsoft.com/dataset/3dep-lidar-copc) | Public catalog; assets signed at download time |
| OpenTopography | [Provider](https://opentopography.org/node/3598) | Local TileIndex archives; license and citation vary by dataset |
| OpenTopography AUS11_Victor | [Provider](https://opentopography.org/node/3598) | One sample validated; requires supplied TileIndex and dataset terms review |
| OpenTopography BR17_SaoPaulo | [Provider](https://opentopography.org/node/3598) | One sample validated; requires supplied TileIndex and dataset terms review |
| OpenTopography Auckland_2013 | [Provider](https://opentopography.org/node/3598) | One sample validated; requires supplied TileIndex and dataset terms review |
| CanElevation | [Provider](https://open.canada.ca/data/en/dataset/7069387e-9986-4297-9f55-0288e9676947) | Public COPC header verified; Open Government Licence Canada; adapter pending |
| swissSURFACE3D | [Provider](https://www.swisstopo.admin.ch/en/height-model-swisssurface3d) | Public STAC sample decoded (930 points); attribution applies; adapter pending |
| Zenodo research: Sila ALS | [Provider](https://zenodo.org/records/3633629) | Contribution example; CC BY 4.0; original aerial merged.las endpoint checked; no AOI adapter |
| Zenodo research: Tree-LiMS | [Provider](https://zenodo.org/records/17492219) | Contribution example; CC BY 4.0; aerial LAS ZIP endpoint checked; no AOI adapter |
| Zenodo research: EBA L1A - Ometto and collaborators | [Provider](https://zenodo.org/records/7636454) | Contribution example; CC BY 4.0; aerial survey ZIP endpoint checked; no AOI adapter |

## Removed from the visible catalog

The following ten entries had no verified end-to-end file access in this review; removal does not assert that their data are unavailable.

- [NEON AOP](https://data.neonscience.org/data-products/DP1.30003.001): Account and download token required; attribution applies
- [IGN LiDAR HD](https://cartes.gouv.fr/rechercher-une-donnee/dataset/IGNF_NUAGES-DE-POINTS-LIDAR-HD): COPC product; confirm current catalog terms
- [PNOA LiDAR](https://pnoa.ign.es/pnoa-lidar/productos-a-descarga): Producer attribution required; download adapter pending
- [Laserdata Skog](https://www2.lantmateriet.se/en/geodata/our-products/product-list/laser-data-download-forest/): CC0 product; delivery workflow pending
- [NLS 0.5 p](https://www.maanmittauslaitos.fi/en/maps-and-spatial-data/datasets-and-interfaces/product-descriptions/laser-scanning-data): Open data with attribution; 5 p is a separate licensed product
- [Scottish Public Sector LiDAR](https://registry.opendata.aws/scottish-lidar/): Public AWS bucket; OGL v3 except phase-2 LAZ non-commercial terms; adapter pending
- [ORNL Amazon ALS](https://doi.org/10.3334/ORNLDAAC/1644): Earthdata access and dataset citation must be verified
- [ForestScan Lope - UAV laser scanning subset](https://doi.org/10.5285/88a8620229014e0ebacf0606b302112d): Research collection; aerial UAV-LS subset only; TLS excluded; point sample and adapter pending
- [LINZ](https://www.linz.govt.nz/products-services/data/types-linz-data/elevation-data/access-elevation-data): Open point clouds; collection adapter pending
- [ELVIS](https://elevation.fsdf.org.au/): Portal ordering workflow; adapter pending

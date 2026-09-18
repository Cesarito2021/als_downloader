# Review of the supplied source material

17 September 2026. This review goes beyond the previous active catalogue.
Inputs: `Global_ALS_LiDAR_Cloud_Access_Table.docx`, the three
`ICESat2_ALS_*2026-09-02.xlsx` workbooks, `ALS_EU/readme.docx`,
`ALS_EU/readme_EXCEL.xlsx`, and the supplied JRC 2021 European LiDAR report.
Public dataset identifiers referenced in the supplied research paper were
also considered; the manuscript itself is not redistributed.

The [46-row candidate audit](document-candidate-review.csv) preserves repeated
entries and earlier endpoint findings rather than treating each row as a
different dataset. The [JRC section screening](JRC_SECTION_REVIEW.md) records
28 country sections. Its 2021 descriptions are historical leads, not current
access guarantees. The ancillary application-design files and processing
instructions are not additional data repositories.

## Existing file organization and access

| Source from the material | Native organization / spatial discovery | Current access and next integration requirement |
|---|---|---|
| USGS 3DEP | STAC item polygons and COPC assets | Existing in-app adapter. |
| OpenTopography | Shapefile tile indexes with file URLs | Existing adapter with supplied indexes; review acquisition method per dataset. |
| Canada | Project and tile Shapefiles, COPC files | Official portal retained; convert native index and resolve stable asset URLs before enabling a connector. |
| Switzerland | STAC Polygon linked to LAS ZIP | Native AOI search and original ZIP download implemented; extract locally for preview. |
| France | Spatial selection and kilometre COPC/LAZ tiles | Official portal retained; current index/asset discovery must be implemented. |
| Spain | LAZ tiles by coverage campaign | Official portal retained; campaign-specific indexes and downloads need a connector. |
| [AHN](https://www.ahn.nl/dataroom) | LAZ sheets; official WFS/OGC index includes download links | AHN6 native index and LAZ download implemented; other versions remain portal access. |
| [Norway](https://www.geonorge.no/kartdata/datasett-i-geonorge/hoydedata/) | Laser projects with spatial selection at Hoydedata | Added official access. Select laser point clouds, not derived elevation grids. |
| [Finland](https://www.maanmittauslaitos.fi/laserkeilaus-ja-ilmakuvaus) | Acquisition coverage GeoPackages and Mapsite downloads | Added open 0.5 p access. Paid 5 p product is excluded. |
| [Poland](https://www.geoportal.gov.pl/en/data/lidar-measurements-lidar/) | ALS download layers and spatial indexes | Added official access. Native index-to-file connector pending. |
| [Estonia](https://geoportaal.maaamet.ee/eng/spatial-data/elevation-data/download-elevation-data-p664.html) | Survey-year laser point downloads and spatial selection | Added official access. Select the aerial point product. |
| [Saxony, Germany](https://www.geodaten.sachsen.de/downloadbereich-digitale-hoehenmodelle-4851.html) | LSC LAZ in 2 km ZIP tiles with acquisition information | Added state-level access. Does not establish coverage across Germany. |
| [NEON](https://www.neonscience.org/resources/learning-hub/tutorials/neon-discrete-point-clouds) | Discrete-return 1 km tiles; tile-boundary metadata | Added official access. Downloads need the user's account/token; no credential sharing. |
| [ORNL Brazil](https://daac.ornl.gov/CMS/guides/LiDAR_Forest_Inventory_Brazil.html) | Original LAZ plus `cms_brazil_lidar_tile_inventory.csv` and KMZ | Added official access. Use the inventory rather than requiring a new Shapefile. Earthdata workflow and authenticated transfer remain untested. |
| [LINZ](https://www.linz.govt.nz/products-services/data/types-linz-data/elevation-data/provincial-growth-fund-lidar-programme) | Regional surveys with source point clouds and coverage | Added official access. Existing Auckland OpenTopography sample is not a connector for all LINZ surveys. |

Of these newly restored sources, AHN6 now has an AOI adapter. Switzerland also
has a native adapter. Other restored sources remain official access links. Their access requirements appear next to the country links.
The in-app provider selector includes only implemented adapters.

## Research sources and unresolved leads

| Lead | Finding / reason not automatically activated |
|---|---|
| Paracou / ForestScan | Prior review verified a public LAZ directory; file-to-footprint mapping remains unresolved. Keep as an integration candidate. |
| Panama 2023 / Smithsonian | Prior review verified classified/unclassified LAZ directories. Need footprint mapping and source-specific terms review. |
| ORNL Indonesia, DOI 10.3334/ORNLDAAC/1518 | Airborne product identified. Current guide redirect failed in this pass; Earthdata file/index workflow not established. |
| ORNL Mozambique, DOI 10.3334/ORNLDAAC/1521 | Public dataset identifier recovered from the supplied material; promising survey/tile candidate. Current guide redirect failed; fresh file/index access remains pending. |
| Brazil DOI 10.48432/EAOMKQ and 10.48432/SJM2W0 | Additional repository leads from the supplied paper; not assumed to be duplicates of ORNL or Zenodo. File organization/access unverified. |
| Australia DOI 10.4225/25/56CA73A346C6A | Additional paper lead; no current tile endpoint verified. |
| ELVIS | Official discovery/order portal previously reviewed. Automated transfer and selected point-cloud delivery remain untested. |
| Scotland | Public AWS archive, but phase-2 LAZ has non-commercial terms. A connector must select eligible campaigns, not approve the entire bucket. |
| Sweden | The [2026 provider update](https://www.lantmateriet.se/sv/geodata/vara-produkter/Produktnyheter/Geografisk-information/uppdatering-angaende-tillhandahallandet-av-laserdata-nedladdning-skog/) changes access conditions and describes COPC/API delivery. Previous blanket CC0/anonymous assumptions are not retained. |
| Gabon / Malaysia ForestScan | Mixed terrestrial and aerial collections; resolve the exact aerial subset and spatial index before activation. |
| Kruger / CEDA | Registration and file-level access remain untested. |
| GlobALS / GEDI calibration pages | Discovery leads, not direct LAS/LAZ download evidence. |
| ORNL 2481 and 1681 | Supplied identifiers resolve to derived products; not raw point-cloud sources for this app. |
| China / Taiwan OpenTopography examples | Reviewed examples are photogrammetric, excluded. Restricted China data in the supplied paper are also excluded. |
| Other countries in the JRC report | Historical section screening only. DEM/DSM availability is not proof of open LAS/LAZ. Current cloud endpoints must be verified before adding them. |

## Zenodo remains a contribution route

The repository is not excluded. A contributor may supply the numeric Zenodo
record ID (or DOI), a polygon boundary or tile index, and the existing data
links. The form now requires a boundary link for a Zenodo submission unless
its data link already points to the GeoJSON tile index. It does not upload
large clouds or automatically publish a record.

A campaign boundary locates the study. Selecting individual files requires
one footprint-to-file association per tile; a boundary alone cannot establish
that association. GeoPackage and zipped polygon Shapefiles are accepted for
manual review/conversion; the implemented common index reader accepts GeoJSON.
Boundary accuracy, open license and record/file association remain maintainer
checks. Existing Sila/Tree-LiMS/EBA examples remain inactive pending suitable
coverage. Changing a boundary link resets the compatibility check.

[European connector evidence and limits](EUROPE_INTEGRATION.md).

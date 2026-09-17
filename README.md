# ALS Downloader

An R package and Shiny application to discover and download airborne laser scanning point clouds. Draw or upload a study area, find source tiles, download a selection, and inspect a bounded 3D preview.

**Development preview 0.1.0.9000 — not submitted to CRAN.** Working adapters: USGS 3DEP through Microsoft Planetary Computer, and OpenTopography through local TileIndex archives. Other catalog entries are candidates awaiting integration.

Developed within [OpenForest4D](https://openforest4d.org). Maintainer: Cesar Ivan Alvites Diaz, **calvites1990@gmail.com**. Secondary contact: **c.alvitesdiaz@ufl.edu**. Software: **GPL-3**. Datasets retain their own licenses.

## Install and launch

Requires R >= 4.1 and the spatial dependencies of `sf`. Installation is explicit; the app never installs packages at startup.

```r
install.packages("remotes")
remotes::install_github("Cesarito2021/als_downloader", ref = "codex/shiny-r-package")
install.packages("lidR") # optional LAS/LAZ preview
alsdownloader::launch_app()
```

From a clone, use `remotes::install_local(".")` and then `alsdownloader::launch_app()`. The root `app.R` is also a Shiny entry point after installation. The previous application is preserved under `legacy/` for comparison and is not loaded by the package.

For OpenTopography, obtain [provider tile indexes](https://opentopography.org/node/3598) and configure a folder of `*_TileIndex.zip` archives. Indexes and point clouds are not bundled.

```r
alsdownloader::launch_app(tile_index_dir = "C:/data/TileIndex_all")
```

## Use the app or R functions

1. Navigate the map. Country shading indicates catalog candidates, not continuous survey coverage.
2. Draw a polygon/rectangle or upload GeoJSON, GeoPackage, FlatGeobuf, or a zipped Shapefile with companion files. Inputs need a CRS; multilayer GeoPackages need a layer selection.
3. Search an implemented provider and inspect tile footprints and dates. Unknown dates remain visible. Incomplete searches fail explicitly.
4. Select rows and an output folder. Downloads produce `manifest.csv`, `CITATIONS.txt`, and checksum sidecars. Restarting verifies completed files before skipping them; interrupted tiles restart in full.
5. Open **3D preview**, upload LAS/LAZ, and rotate the bounded sample. Colors represent source elevation, not canopy height. Check source units and vertical datum.

```r
library(alsdownloader)
aoi <- read_aoi("study-area.gpkg", layer = "boundary")
aoi_area(aoi) # square kilometres, overlaps counted once
tiles <- find_tiles(aoi, provider = "usgs3dep")
result <- download_tiles(tiles, "selected-tiles", workers = 2)
points <- read_preview(result$path[1], max_points = 50000)
```

## Workers, hosting, and devices

Local mode recommends `min(10, max(1, available cores - 4))` workers, adjustable up to the machine allowance. Effective concurrency also respects tile count and the configured provider ceiling, initially two. Raise `provider_limit` after checking applicable service terms. `future.apply::future_lapply()` handles transfers inside a background process, keeping Shiny responsive during downloads and previews.

Hosted mode uses one download worker, at most 10 tiles / 500 MB per batch with known sizes, and 200 MB uploads. These are application controls, not universal Shiny limits. Large transfers belong in local mode.

```r
alsdownloader::launch_app(mode = "hosted", tile_index_dir = "/srv/als/indexes")
```

The repository entry point accepts `ALS_MODE` and `ALS_TILE_INDEX_DIR`. Multiple server processes must share `ALS_HOST_LOCK_DIR`; administrators must configure storage quotas and cleanup. This is not a distributed scheduler. Tile searches are currently synchronous and large index collections can take time.

The layout adapts to desktop, tablet, and phone widths. Touch rotation and collapsible controls are included. Physical mobile-browser acceptance testing and hosted deployment remain pending. Workers run on the hosting computer, not the phone.

## Countries and sample validation

Each passed row resolved an AOI, downloaded one representative tile, decoded it with `lidR`, and verified checksum restart on 2026-09-16. **A sample pass is not national coverage validation.** Exact URLs, sizes, checksums and point counts: [validation.csv](docs/validation.csv).

| Country / region | Dataset / source | Status |
|---|---|---|
| United States | USGS 3DEP, Utah Statewide South 2020 | Passed: 1 tile, 376,166 points |
| Australia | OpenTopography AUS11_Victor | Passed: 1 tile, 3,145,273 points; ELVIS pending |
| Brazil | OpenTopography BR17_SaoPaulo | Passed: 1 tile, 4,564,257 points; ORNL/Zenodo/EMBRAPA pending |
| New Zealand | OpenTopography Auckland_2013 | Passed: 1 tile, 1,772,379 points; direct LINZ pending |
| Taiwan | OpenTopography TW18_Carr | Passed: 1 tile, 474,929 points |
| United States | NEON AOP | Candidate; account/token integration pending |
| Canada | CanElevation | Candidate; native adapter and sample pending |
| France | IGN LiDAR HD | Candidate; native adapter and sample pending |
| Spain | PNOA LiDAR | Candidate; native adapter and attribution review pending |
| Sweden | Laserdata Skog | Existing local LAS preview checked; remote download pending |
| Finland | NLS | Candidate; distinguish open 0.5 p from licensed 5 p |
| Switzerland / Liechtenstein | swisstopo / supplied inventory | Candidate; access and adapter pending |
| United Kingdom / Scotland | Scottish Public Sector LiDAR | Candidate; campaign license and adapter pending |
| Gabon | ForestScan / AfriSAR | Candidate; distinguish point clouds from waveform products |
| French Guiana | ForestScan, Paracou | Candidate; geometry and remote sample pending |
| Malaysia | ForestScan / NERC-ARF, Sabah | Candidate; access and sample pending |
| Indonesia | Kalimantan research data | Candidate; access and sample pending |
| Panama | Research data in supplied inventory | Candidate; access and sample pending |
| China / Ningxia | Research data in supplied inventory | Candidate; access and sample pending |
| South Africa | Research data in supplied inventory | Candidate; access and sample pending |
| Netherlands, Norway, Estonia, Poland | National datasets in supplied inventory | Candidates; access and adapters pending |
| Central Africa | Regional research collections | Candidate region; individual country/file coverage not established |

The [candidate inventory](docs/dataset-candidates.csv) retains 45 supplied records, including overlapping and regional records. Their descriptions are not verified current terms. `provider_catalog()` returns curated provider links and implementation flags, not an exhaustive global inventory.

OpenTopography uses URLs embedded in supplied indexes, not area-processing requests. There is no assumed universal 5 km limit: restrictions depend on endpoint and dataset. Check each landing page and the [citation guidance](https://opentopography.org/citations). Unknown licenses and missing DOIs remain unresolved; `CITATIONS.txt` is a provenance starting point, not necessarily a publication-ready citation.

The optional backdrop uses [Esri World Hillshade](https://developers.arcgis.com/javascript/latest/sample-code/layers-custom-blendlayer/). Bundled country outlines remain available without that service. Background maps are context, not ALS coverage.

## Suggest a dataset

[Suggest a dataset](https://github.com/Cesarito2021/als_downloader/issues/new?title=Dataset%20suggestion): include country/site, provider, landing page or DOI, format, coverage/index, dates, license, required citation, and credential requirements. Never include keys or passwords. A structured [suggestion form](.github/ISSUE_TEMPLATE/suggest-dataset.yml) becomes available in GitHub's issue chooser after merge.

Suggestions are reviewed before inclusion. Enabling a source requires documented terms, reliable AOI-to-tile discovery, and one or two decoded sample downloads per supported country. A catalog entry alone does not enable downloads.

## Development and release gate

Includes documented functions, offline tests, an offline vignette, and Windows/macOS/Linux check CI. Local Windows validation uses `R CMD check --as-cran --no-manual`; it does not establish CRAN acceptance. See [the release checklist](docs/RELEASE_CHECKLIST.md). No workflow submits to CRAN.

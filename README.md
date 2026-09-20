# ALS Downloader

![ALS Downloader — airborne LiDAR discovery and visualization](docs/images/banner.png)

[![License: GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Version](https://img.shields.io/badge/version-0.1.1-blue.svg)](NEWS.md)
[![Install in R](https://img.shields.io/badge/R-install-276DC3?logo=r&logoColor=white)](#get-started)
[![R-universe version](https://cesarito2021.r-universe.dev/alsdownloader/badges/version)](https://cesarito2021.r-universe.dev/alsdownloader)
[![GitHub source ZIP](https://img.shields.io/badge/GitHub-source_ZIP-181717?logo=github&logoColor=white)](https://github.com/Cesarito2021/als_downloader/archive/refs/heads/main.zip)

**Discover, visualize and download airborne laser scanning data from an international catalogue.**
ALS Downloader combines R functions with a map-based Shiny application: define an **area of interest (AOI)**, find available tiles, inspect point clouds and download original files from their providers. Coverage depends on the source and configured indexes; the catalogue does not imply complete global or national coverage.

[Get started](#get-started) · [Application](#application) · [Outputs](#outputs) · [Examples](#four-source-examples) · [Catalogue](#als-catalogue) · [Contribute](#contribute-als-data)

## Get started

Install the latest published version from **[R-universe](https://cesarito2021.r-universe.dev/alsdownloader)** (recommended). Version **0.1.1** is prepared for final user review; the package has **not yet been submitted to CRAN**.

```r
install.packages("alsdownloader", repos = c(
  "https://cesarito2021.r-universe.dev",
  "https://cloud.r-project.org"
))
install.packages("lidR")
alsdownloader::launch_app()
```

Alternatively, install the development source from GitHub:

```r
install.packages(c("remotes", "lidR"))
remotes::install_github("Cesarito2021/als_downloader")
alsdownloader::launch_app()
```

Requires R ≥ 4.1. `lidR` enables point-cloud reading. PDF reports also require `rmarkdown`, Pandoc and a LaTeX installation such as TinyTeX. The GitHub ZIP contains software source, not LiDAR files. [Installation and workflow guide](vignettes/als-workflow.Rmd).

The R package is named **`alsdownloader`**. The repository remains `als_downloader` to preserve existing links.

## Coverage in numbers

ALS Downloader connects users to **USGS 3DEP, OpenTopography, CanElevation,
AHN6, swissSURFACE3D and IGN LiDAR HD**, with additional national portals in its
catalogue. **The verified OpenTopography integration alone provides access to
428 airborne-LiDAR collections, over 1.05 million unique point-cloud files and
42.6 TB of provider-hosted data.** The app discovers and downloads original files;
it does not store or redistribute these point clouds.

| OpenTopography snapshot · 20 September 2026 | Verified count |
|---|---:|
| Integrated airborne-LiDAR collections / survey footprints | 428 |
| Tile-footprint records in their indexes | 1,056,156 |
| Unique file objects, after removing shared-file duplicates | 1,056,011 |
| Original-file storage, from public object sizes | 42.598 TB (38.743 TiB) |

These figures exclude external-only collections and federated 3DEP. They are
not a combined global total or unique land area: different surveys can overlap.
Other provider inventories remain separate to avoid double counting.
[Methods and source inventories](docs/COVERAGE_STATISTICS.md) ·
[Per-collection statistics (CSV)](inst/extdata/opentopography-collection-statistics.csv).

**Core-aware parallel downloading** supports local workflows, bounded by available
CPU cores, selected jobs and provider limits (two simultaneous transfers by
default; hosted mode uses one). Downloaded LAS/LAZ files can then be processed
with tools such as `lidR`; the app provides sampled visualization and comparison.

## Application

Explorer opens with two access masks: **red survey footprints** for in-app
discovery and **yellow country outlines** linking to external providers.
The welcome globe illustrates access by country. [Coverage sources and limits](inst/sources/DISCOVERY_COVERAGE.md).

![Welcome page](docs/images/current/welcome.png)

The screenshots are captured at double resolution. Click any image to read it at full size.

<details>
<summary>Interface text at a closer scale</summary>

![Application title and Zenodo submission control](docs/images/current/header-detail.png)

</details>

| View | Purpose |
|---|---|
| **Explore** | Draw or upload an AOI, filter acquisition dates, inspect footprints and select tiles. |
| **3D view** | Inspect a source tile or local LAS/LAZ, using source classification, intensity or elevation colours. |
| **3D comparison** | Visually compare compatible sources or a source with a local cloud in a shared window. |

Downloads preserve original tiles. The viewer uses sampled points and may download files temporarily. Comparison is a visual aid, not an automated change estimate; compatible coordinate and elevation references are required. [Comparison guide](docs/TEMPORAL_COMPARISON.md).

<details>
<summary>View the application screens</summary>

**Coverage overview** — survey footprints and external access links.
![Coverage masks](docs/images/current/coverage.png)


**Point-cloud visualization**
![3D view](docs/images/current/3d-view.png)

**Comparison controls** — no comparison result is claimed in this capture.
![3D comparison](docs/images/current/comparison.png)

**About the project**
![About](docs/images/current/about.png)

**Source catalogue**
![Catalogue](docs/images/current/catalogue.png)

**Submit ALS data - Zenodo**
![Contribution form](docs/images/current/contribute.png)

</details>

## Outputs

* **Point clouds:** original LAS/LAZ files or provider ZIP archives.
* **Figures (PNG):** AOI and tile maps, point-cloud views, and available comparison profiles and distributions.
* **Metadata and scripts:** tile metadata (CSV) and an R script to download the selected files.
* **Download report (PDF):** a concise summary of the AOI, selected tiles, acquisition dates, known download volume (GiB), available figures and source credits.

**Review the PDF before downloading** to plan storage and document the selection.
Unknown file sizes are flagged; allow additional space for archive extraction
and temporary visualization files.

## Four source examples

Four verified sites shown as **top-down point clouds**. Point colours use **Viridis by source elevation (Z)**, not canopy
height or change. Click an image for the full-size view.
[Source credits and figure details](docs/README_FIGURES.md).

### 1. USA · Utah

USGS 3DEP · [Source catalogue](https://planetarycomputer.microsoft.com/dataset/3dep-lidar-copc).

![Utah point cloud from above](docs/images/current/United_States-cloud.png)

### 2. Brazil · São Paulo

Municipality of São Paulo, SMDU/M3DC · [OpenTopography dataset](https://doi.org/10.5069/G9NV9GD1).

![São Paulo point cloud from above](docs/images/current/Brazil-cloud.png)

### 3. Canada · Athabasca

Natural Resources Canada · [CanElevation](https://open.canada.ca/data/en/dataset/7069387e-9986-4297-9f55-0288e9676947), Open Government Licence – Canada.

![Athabasca point cloud from above](docs/images/current/Canada-cloud.png)

### 4. Netherlands · Groningen

AHN6 · [AHN](https://www.ahn.nl/dataroom), [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).

![Groningen point cloud from above](docs/images/current/Netherlands-cloud.png)

These are sampled visualizations of individual sites. Unknown coordinate units remain unverified;
no height normalization or change analysis is applied.

## ALS catalogue

[![USGS 3DEP data source](https://img.shields.io/badge/Data_source-USGS_3DEP-006747)](https://www.usgs.gov/3d-elevation-program)
[![OpenTopography data source](https://img.shields.io/badge/Data_source-OpenTopography-9B5526)](https://opentopography.org)

<a href="https://opentopography.org"><img src="docs/images/providers/opentopography.png" alt="OpenTopography — data and resources" width="180"></a>

Provider badges link to the official data sources. The OpenTopography logo acknowledges its data and resources, under its [media kit guidelines](https://opentopography.org/media); it does not imply endorsement. USGS 3DEP is identified with a text badge because the [official USGS logo has restricted use](https://www.usgs.gov/media/images/usgs-visual-identity-usgs-logo-black-tm).

| Integrated source | Access |
|---|---|
| **USGS 3DEP**, USA | Planetary Computer spatial catalogue and COPC files |
| **OpenTopography**, international | Audited hosted ALS catalogue; original tile indexes load automatically for the AOI |
| **CanElevation**, Canada | Official spatial tile service and original point clouds |
| **AHN6**, Netherlands | Spatial index and LAZ files |
| **swissSURFACE3D**, Switzerland | Spatial catalogue and LAS archives |
| **IGN LiDAR HD**, France | Spatial catalogue and COPC files |
| **Approved community sources** | Reviewed boundaries linked to provider-hosted files |

Additional catalogue entries link to providers' own portals. OpenTopography explicitly documents the [Tile Index download workflow](https://opentopography.org/node/3598) used by this adapter. Each source retains its own access conditions.

The package includes a small search index. Exact OpenTopography overview outlines
load on demand from a versioned, checksum-verified auxiliary catalogue (about
39 MB), cached for the R session. Tile searches always use the provider's original
polygons. [Incremental updates and catalogue versions](docs/OPENTOPOGRAPHY_ACCESS.md).

[Full source catalogue](inst/sources/SOURCES.md) · [Regional verification](docs/REGIONAL_VERIFICATION.md)

## Contribute ALS data

**Submit ALS data - Zenodo** accepts published Zenodo records. Supply the record link, coverage and acquisition information; Zenodo metadata provide authors, DOI, licence and available files. Polygons should link to the corresponding point-cloud assets. Declared approximate coverage remains labelled approximate.

The ALS Downloader team reviews submissions before publication. Inclusion requires explicit approval. Contributing a link does not transfer ownership or upload the point clouds to ALS Downloader.

Contributors need no account; their contact email is optional. An optional
[Formspree form](docs/FORMSPREE_SETUP.md) receives proposals from local or hosted
apps and notifies the maintainer. It requires a configured endpoint and is subject
to service quotas. Receipt is not approval or automatic catalogue publication.
The separate local queue supports [private email review invitations](docs/REVIEWER_ACCESS.md)
when its mail service is configured.

For Zenodo, select a supplied boundary and its matching cloud file, or upload
coverage polygons. Approved coverage updates in Explorer automatically.
[A real ALS and Shapefile integration test](docs/ZENODO_LIVE_CHECKS.md) documents
what was verified and the limits of automatic metadata extraction.

[Contribution guide](docs/CONTRIBUTING_DATA.md) · [Contact-data handling](docs/CONTRIBUTOR_PRIVACY.md)

## Author and citation

Developed and maintained by **Cesar Ivan Alvites Diaz (Cesar Alvites)**, [University of Florida](https://cesarito2021.github.io/). [Contact](mailto:calvites1990@gmail.com).

Use `citation("alsdownloader")` to cite the software. Cite each dataset's producer and DOI separately; exported metadata and reports preserve available source credits.

## Acknowledgement

Developed within [OpenForest4D](https://openforest4d.org), funded by NSF awards **2409885, 2409886 and 2409887**.

## Licences and credits

Software: **GPL-3**. Dataset rights remain with their respective rights holders. Access through this application grants no additional permission and implies no provider endorsement. Follow each dataset's licence, citation requirements and service conditions.

Leaflet and its R interface retain their software licences; Natural Earth supplies public-domain globe outlines. Explorer uses OpenStreetMap cartography with visible attribution. Exported maps retain the copyright URL. Browser caching and the [tile service policy](https://operations.osmfoundation.org/policies/tiles/) apply; no offline basemap downloads are provided.

[Third-party notices](inst/NOTICE) · [Data and figure licensing](inst/sources/LICENSING.md) · [Access and intellectual-property review](inst/sources/USE_REVIEW.md) · [CRAN readiness](docs/CRAN_READINESS.md)

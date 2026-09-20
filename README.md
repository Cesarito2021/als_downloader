![ALS Downloader — airborne LiDAR discovery and visualization](docs/images/banner.png)

[![License: GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Version](https://img.shields.io/badge/version-0.1.3-blue.svg)](NEWS.md)
[![Install in R](https://img.shields.io/badge/R-install-276DC3?logo=r&logoColor=white)](#get-started)
[![R-universe version](https://cesarito2021.r-universe.dev/alsdownloader/badges/version)](https://cesarito2021.r-universe.dev/alsdownloader)
[![GitHub release downloads](https://img.shields.io/github/downloads/Cesarito2021/als_downloader/total?label=release%20downloads)](https://github.com/Cesarito2021/als_downloader/releases)
[![GitHub source ZIP](https://img.shields.io/badge/GitHub-source_ZIP-181717?logo=github&logoColor=white)](https://github.com/Cesarito2021/als_downloader/archive/refs/heads/main.zip)

### ALS Downloader: a web-based Shiny application for the discovery, management, visualization, and download of airborne laser scanning (ALS) datasets worldwide

**Developed by Cesar Alvites**<br>
School of Forest, Fisheries, and Geomatics Sciences, University of Florida

ALS Downloader links a user-defined area of interest (AOI) to available airborne LiDAR surveys. Users can identify and select tiles, download the original point cloud files in parallel, inspect point clouds interactively, and compare overlapping acquisitions. All data remain hosted by the originating providers. The catalogue combines direct data access with links to national portals, and spatial coverage varies by source.

## User interface

Start with the globe, then open **Explore** to find tiles. **3D view** inspects a
selected tile or local file; **3D comparison** brings overlapping clouds together.

![ALS Downloader: globe, access routes and entry points](docs/images/workflow/01-globe.gif)

**A.** The globe shows access routes: red indicates in-app ALS data access and yellow indicates external portals; survey coverage varies within each country. **B.** Open map launches the dataset explorer. **C.** Submit ALS data opens the Zenodo contribution form. **D.** About the project presents the application and its purpose. **E.** GitHub opens the source-code repository.

## Get started

Install the latest published version from **[R-universe](https://cesarito2021.r-universe.dev/alsdownloader)** (recommended). Version **0.1.3** is prepared for final user review; the package has **not yet been submitted to CRAN**.

```r
install.packages("alsdownloader", repos = c(
  "https://cesarito2021.r-universe.dev",
  "https://cloud.r-project.org"
))
install.packages("lidR")
alsdownloader::launch_app()
```

The browser is the interface; R runs the application on your computer. To start explicitly in local mode:

```r
alsdownloader::launch_app(mode = "local")
```

Local mode saves files to your chosen folder and supports parallel transfers. The effective worker count is limited by available cores (reserving four when possible), selected tiles and the configured transfer limit (two by default). A public server running in `hosted` mode uses one worker. Where a provider permits more connections, configure the limit when launching, for example `launch_app(mode = "local", provider_limit = 4L)`.

Alternatively, install the development source from GitHub:

```r
install.packages(c("remotes", "lidR"))
remotes::install_github("Cesarito2021/als_downloader")
alsdownloader::launch_app()
```

Requires R ≥ 4.1. `lidR` enables point-cloud reading. PDF reports also require `rmarkdown`, Pandoc and a LaTeX installation such as TinyTeX. The GitHub ZIP contains software source, not LiDAR files. [Installation and workflow guide](vignettes/als-workflow.Rmd).

The R package is named **`alsdownloader`**. The repository remains `als_downloader` to preserve existing links.

## ALS Data Catalog

| Integrated source | Access |
|---|---|
| **USGS 3DEP**, USA | Planetary Computer spatial catalogue and COPC files |
| **OpenTopography**, international | Audited hosted ALS catalogue; original tile indexes load automatically for the AOI |
| **CanElevation**, Canada | Official spatial tile service and original point clouds |
| **AHN6**, Netherlands | Spatial index and LAZ files |
| **swissSURFACE3D**, Switzerland | Spatial catalogue and LAS archives |
| **IGN LiDAR HD**, France | Spatial catalogue and COPC files |
| **Approved community sources** | Reviewed boundaries linked to provider-hosted files |

Other countries link to their official download portals. [Full source catalogue](inst/sources/SOURCES.md).

## ALS Downloader Stats

The audited OpenTopography portion of ALS Downloader connects users to over **1.05 million files (42.6 TB)** hosted by the original providers.

| OpenTopography snapshot · 20 September 2026 | Verified count |
|---|---:|
| Integrated airborne-LiDAR collections / survey footprints | 428 |
| Tile-footprint records in their indexes | 1,056,156 |
| Unique file objects, after removing shared-file duplicates | 1,056,011 |
| Original-file storage, from public object sizes | 42.598 TB (38.743 TiB) |

Counts cover the audited OpenTopography collections, excluding federated 3DEP and external-only entries. [Inventory and methods](docs/COVERAGE_STATISTICS.md).

## Input data

- **Study area:** draw a polygon or rectangle, or upload an AOI file.
- **Acquisition period:** choose dates, then select tiles by year and campaign or directly in the table.
- **Download folder and workers:** choose where to save files and how many transfers to run in parallel.
- **Local point cloud (optional):** open a LAS/LAZ file for 3D viewing or comparison.

## Outputs

* **Point clouds:** original LAS/LAZ files or provider ZIP archives.
* **Figures (PNG):** AOI and tile maps, point-cloud views, and available comparison profiles and distributions.
* **Metadata and scripts:** tile metadata (CSV) and an R script to download the selected files.
* **Download report (PDF):** a concise summary of the AOI, selected tiles, acquisition dates, reported download size (MB or GB), available figures and source credits.

**Recommendation:** review the PDF before downloading to check the selected tiles and reported storage requirements. File sizes may be unavailable from some providers.

## Point-cloud examples

Four source datasets viewed from above and coloured by elevation. Click an image to enlarge it. [Figure credits](docs/README_FIGURES.md).

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

## Share your ALS data

Share an ALS dataset already published on **Zenodo** through **Submit ALS data - Zenodo**:

1. Enter the Zenodo record or DOI and supply its coverage polygons.
2. Match the polygon IDs to the point-cloud files, choose acquisition years and provide a private contact email.
3. Validate and submit. The team reviews the entry before it appears in the catalogue.

Data remain on Zenodo. Catalogue entries retain the dataset DOI, authors and licence so users can find and cite your work. We encourage citation of both the dataset and its associated paper; users are responsible for respecting the source licence and attribution requirements.

## Author and contact

**César Alvites** · University of Florida · [calvites1990@gmail.com](mailto:calvites1990@gmail.com)

## Related publications

- Alvites, C., Santopuoli, G., Maesano, M., Chirici, G., Moresi, F. V., Tognetti, R., Marchetti, M., & Lasserre, B. (2021). [Unsupervised algorithms to detect single trees in a mixed-species and multilayered Mediterranean forest using LiDAR data](https://flore.unifi.it/handle/2158/1259181). *Canadian Journal of Forest Research, 51*(12), 1766–1780.
- Alvites, C., Marchetti, M., Lasserre, B., & Santopuoli, G. (2022). [LiDAR as a tool for assessing timber assortments: A systematic literature review](https://doi.org/10.3390/rs14184466). *Remote Sensing, 14*(18), 4466.
- Alvites, C. (2026). *ALS Downloader: a web-based Shiny application for the discovery, management, visualization, and download of airborne laser scanning (ALS) datasets worldwide*. **Manuscript in preparation.**

## Acknowledgements

Developed at the **University of Florida** within [OpenForest4D](https://openforest4d.org),
funded by the **U.S. National Science Foundation** (awards **2409885, 2409886 and 2409887**).

<table>
<tr>
<td align="center" width="20%"><img src="docs/images/acknowledgements/openforest4d.png" alt="OpenForest4D" width="115"></td>
<td align="center" width="20%"><img src="docs/images/acknowledgements/nsf.jpg" alt="U.S. National Science Foundation" width="130"></td>
<td align="center" width="40%"><img src="docs/images/acknowledgements/university-of-florida.jpg" alt="University of Florida" width="280"></td>
<td align="center" width="20%"><img src="docs/images/acknowledgements/silva-lab.png" alt="Silva Lab" width="120"></td>
</tr>
</table>

## Licensing and credits

Software: **GPL-3**. Source datasets retain their own licences and citation requirements. Maps credit OpenStreetMap and Natural Earth; acknowledgement logos belong to their respective organizations. [Third-party notices](inst/NOTICE).

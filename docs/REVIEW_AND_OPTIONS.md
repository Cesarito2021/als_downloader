# ALS Downloader review and development options

Historical design notes from the first development review. For current source identity, access and implementation status,
use [SOURCE_AUDIT.md](SOURCE_AUDIT.md) and the [README](../README.md). The Taiwan/China ALS assumptions below were corrected:
the named OpenTopography datasets are photogrammetric. The next map stage is described in [MAP_ROADMAP.md](MAP_ROADMAP.md).

Review date: 17 September 2026. Baseline: GitHub commit `25a7e2b`.

## Recommendation

Develop **ALS Explorer**, a map-first Shiny application with a shared provider catalog, reliable tile downloads and an optional point-cloud preview. Retain the current OpenTopography TileIndex and USGS 3DEP discovery workflows after correcting their failure cases. Offer both hosted and local execution, with the same discovery, study-area, metadata and preview interface. Hosted downloads use one download worker; local downloads use a configurable worker pool.

Build this in stages. A polished map alone will not make the application dependable: accurate coverage, resumable downloads, trustworthy acquisition dates and correct citations are the key scientific improvements.

This document records the initial baseline review. The subsequent package preview is described in the current README and RELEASE_CHECKLIST.md; those files supersede implementation-status statements in this historical review.

## Material reviewed and verification boundary

- Inventoried 1,595 files, including 52 R scripts, 990 ZIP archives, 99 LAZ files and one LAS file.
- Extracted text and hyperlinks from four Word documents, four workbooks, eight PDFs and one presentation. This is content extraction, not a complete visual or scientific peer review of every document.
- Audited all 52 R scripts for parsing, function definitions, fixed paths and global state. Inspected the download application and the reusable biomass visualization sections in detail.
- The desktop `ot_pc_app/app.R` and `base.R` have identical SHA-256 hashes to GitHub. `lidar_app` is a separate earlier 3DEP-oriented version.
- Parsed 50 scripts successfully. Two biomass prototype scripts fail parsing: `biomass_app/app2/pipeline2_gte_100.R:169` and `biomass_app/app2/pipelines1_pipelines2.R:839`. Both contain `las <- lidR::readLAS(chunk) if` on one line. Parsing success does not establish runtime or scientific correctness.
- Preserved 45 candidate records from the three global-candidate workbooks, with source workbook, sheet and row. These overlap; they are not 45 unique working integrations. The master workbook alone contains 22 rows and explicitly omits several candidates retained in the other files.
- Read a reproducible 16,000-point sample from the local Swedish LAS file (7,635,604 source points) without loading the full point array into memory. No remote point-cloud download was tested.
- Installed R executables are available, but `sf`, `shiny`, `lidR`, `rstac`, `rgl` and the other application packages were not available on the tested default R 4.4 / 4.6 library paths. The current Shiny app has therefore not been runtime-tested in this review.

## Three development options

| Option | User experience | Technical scope | Tradeoff |
|---|---|---|---|
| A — Reliable Downloader | Familiar workflow, clear metadata and a better map | Correct existing discovery/download bugs; resumable manifest; hosted/local modes; English interface | Smallest change, but limited global exploration |
| B — Global ALS Explorer — recommended | World → region → survey → tile; draw or upload AOI; inspect data; download with citations | A plus provider adapters, real coverage footprints, lazy tile loading, bounded 3D preview | Best balance of scientific utility, visual quality and maintainability |
| C — Research Workspace | B plus DTM/DSM/CHM, profiles, biomass and later ICESat-2 comparisons | Separate processing jobs, additional validation and scientific metadata | Valuable long-term direction; substantially larger testing and resource requirements |

Preserve the existing ICESat-2 and biomass methodologies while implementing B. Their presence in the supplied material does not make a scientific pipeline rewrite part of this first redesign.

## Proposed experience

1. **Explore.** Show a graphite world basemap, restrained terrain relief, subtle boundaries and teal coverage accents. Continents are navigation shortcuts, not mandatory steps. Users may search or upload an AOI immediately.
2. **Find real coverage.** At world scale show provider/project summaries. At regional scale show simplified survey footprints. At local scale show intersecting tiles, loaded only for the visible map extent. Display acquisition dates, density where supplied, classification, format, vertical datum and last catalog update.
3. **Define the study area.** Offer both `Draw study area` and `Upload study area` as equal choices. Accept a zipped Shapefile, GeoPackage or GeoJSON; validate projection and polygons and let users select layers/features. Keep the uploaded and normalized geometry for reproducibility. A bare `.shp` is insufficient without its companion files.
4. **Review a download plan.** Show selected datasets, overlapping years, tile count, estimated bytes when known, whole-tile versus AOI-clipped output, required credentials and citations. Show unknown sizes as unknown. Do not add tile areas together and call the result coverage; compute the union intersected with the AOI.
5. **Inspect in 3D.** Start with a small preview, then refine on demand. Use a black background with viridis elevation, optional class coloring and a gray terrain surface only when a compatible DTM exists. Show sample size, source point count, color quantity, units and vertical exaggeration.
6. **Download.** Use a resumable job manifest with per-tile progress, cancel/resume, retry of failed tiles and a final completion summary. Export metadata and citations with the files.

Map states should distinguish `Verified coverage`, `Catalog candidate`, `Authentication required`, `No intersecting data` and `Provider unavailable`. A failed catalog request is not evidence that no data exists. Country coloring must never imply nationwide contiguous coverage.

The interactive concept supplied with this review demonstrates country/region navigation and a real local point sample. It has no live tile coverage, hillshade raster, AOI upload/drawing or remote download integration. Its Swedish sample is not a canopy-height product.

## Hosted and local execution

The user clarified that hosted users should retain all features, including downloads, with serial download execution. Do not carry forward the old README's discovery-only promise into the redesigned product.

| Setting | Hosted | Local |
|---|---|---|
| Download concurrency | One download worker | Adjustable |
| Initial local recommendation | Not applicable | `max(1, min(10, available_cores - 4))` |
| Local upper bound | Not applicable | `max(1, available_cores - 4)` |
| More than ten workers | Not applicable | Advanced choice within the local upper bound |
| Additional limits | Provider limits and host-wide capacity | Provider limits, disk and network capacity |
| Output delivery | Browser downloads / appropriate temporary storage | User-selected local output directory |

Examples: 8 available cores → 4 recommended workers; 16 → 10; 32 → 10 recommended and up to 28 selectable. Machines with four cores or fewer fall back to one worker; reserving four is impossible there. Count available logical cores honestly and prefer a quota-aware detector in deployment.

The **effective** concurrency is the minimum of the user's selection, deployment allowance, provider limit and number of remaining tiles. Ten is a suggested default cap, not a universal service limit. More workers do not guarantee faster transfers once bandwidth, disk throughput or provider throttling dominates.

Distinguish a hosting worker (an R process serving sessions) from a download worker (a concurrent transfer task). Shiny can run background work; hosting policy and available resources determine concurrency. See [Posit instance and worker documentation](https://docs.posit.co/shinyapps.io/guide/applications/) and [Shiny nonblocking operations](https://shiny.posit.co/r/articles/improve/nonblocking/).

For hosted deployment, use one shared download slot per instance, or a centrally enforced queue if multiple instances exist; one worker per browser session would otherwise multiply total concurrency. Queue ownership must remain private to each user. Large hosted outputs require a delivery/storage design, expiry, disk limits and recovery after instance restarts. A server-side filesystem path is not a download to the user's computer.

Suggested English interface text:

> Hosted mode uses one download worker. Run ALS Downloader locally to configure parallel downloads. Local mode recommends leaving four CPU cores available and using up to ten download workers initially. Transfer concurrency may be reduced to respect provider limits.

`R/download_policy.R` implements the numeric planning rule. Validate it with `Rscript tests/test_download_policy.R` from the repository directory. Deployment detection, the queue, the UI and resumable execution remain to be integrated.

### Keep future_lapply for tile transfers

The user's existing `future.apply::future_lapply()` approach is appropriate for independent tile downloads and should be retained initially. The transfer function must return a structured result for every tile, including failure, rather than aborting the whole batch. Keep worker configuration outside reactive callbacks and avoid accidentally creating nested worker pools when adding a background job controller. `future_lapply()` waits for its results; running it directly in a Shiny observer does not by itself keep that session responsive. Use an explicitly budgeted background job architecture and pass plain job inputs into it.

Benchmark scheduling with realistic tile sizes; a few large tiles can unbalance fixed batches. Do not assume a particular chunk setting is optimal before measuring. Consult the [future.apply reference](https://future.apply.futureverse.org/reference/future_lapply.html). For the core detector, prefer [parallelly::availableCores()](https://parallelly.futureverse.org/reference/availableCores.html), which accounts for resource constraints, over blindly trusting the machine's total CPU count.

## Code findings to address first

References below are relative to the unchanged baseline app unless stated otherwise.

| Priority | Evidence | Required change |
|---|---|---|
| High | `app.R:308` passes only the upload temporary path; `base.R:73` derives format from that path | Pass the Shiny upload object and use the original filename to determine format. Temporary paths may lack the extension. Validate archive members before extraction and select the intended vector layer. |
| High | `base.R:120` / `:211` default the catalog bbox to California; `app.R:316` calls the lower-level search without an AOI bbox | Query metadata for the actual AOI. Do not silently skip every tile whose CRS is missing from an unrelated regional lookup. |
| High | `base.R:507` constructs bbox values from already named elements | Remove inherited names: `c(minX = unname(bb['xmin']), ...)`; otherwise `minX.xmin` does not match later `bbox['minX']` lookups. |
| High | Both downloaders validate HEAD but do not check the final GET before labeling the tile downloaded | Validate GET status, expected size if supplied and LAS header/readability; write `.part` and atomically finalize. HEAD success does not guarantee GET success. Do not require HEAD support if the provider does not offer it. |
| High | Existing downloads are skipped by filename alone | Only skip verified completed manifest entries. Partial/error files must be retried. Use provider + dataset + asset ID to prevent filename collisions between collections. |
| High for hosting | `app.R:274` writes the user's API key with `Sys.setenv`; UI defaults read the environment | Keep user credentials in session-scoped state. Never populate a public password input from a shared service key. Redact signed URLs and key-bearing error messages in logs. |
| High for global use | `base.R:55` computes area in EPSG:5070; global `sf_use_s2(FALSE)` | Use geodesic area or a suitable equal-area/local projection. Add antimeridian and high-latitude cases; keep geometry-engine changes scoped. |
| Medium | `stac_post_search_all()` catches pagination errors and silently breaks | Return explicit incomplete-search status, retry transient failures and validate pagination semantics against the pinned STAC client version. |
| Medium | Year heuristics restrict 2014–2026 and discovery may drop missing years | Prefer acquisition intervals from metadata. Preserve unknown dates; distinguish acquisition, publication and processing time. Avoid excluding old legitimate surveys. |
| Medium | `aoi_to_geojson_list()` serializes an `sf` object | Verify that STAC `intersects` receives a Geometry, not a FeatureCollection; add a request-schema test. |
| Medium | Search functions discard dataset geometry and most metadata | Return a normalized catalog object with geometry, dataset ID, asset ID, date interval, source host, license and citation. Retain canonical URLs and sign assets immediately before transfer. |
| Medium | `future_lapply()` is called synchronously inside observers and process-global plans change per click | Move jobs out of the reactive execution path and configure workers centrally. Use session-scoped task state, cancellation and duplicate-submission protection. |
| Medium | `biomass_app/app/app.R:24` sources a fixed `D:/...` path | Resolve packaged modules relative to the app; remove executable example calls from reusable function files. |
| Medium | Six versions of `export_lidar_html` share a name in the visualization scratch file | Extract one tested implementation. Voxel decimation currently does not impose a global maximum; catalog automerge can still accumulate too many points. |

## Reuse plan

| Material | Decision |
|---|---|
| `ot_pc_app` / GitHub | Keep workflow and relevant parsing logic; modify discovery, metadata, download validation and session isolation |
| `lidar_app` | Reference implementation; avoid maintaining a second divergent 3DEP stack |
| `ot_pc_app_codes` and `09_app/app*` variants | Preserve as history; select behavior deliberately rather than sourcing multiple versions |
| `10_opentopography_app/01_scripts` | Reuse TileIndex acquisition ideas; make indexes refreshable and cacheable instead of requiring a separate bulk folder installation |
| `biomass_app/app/00_base2.R` | Extract the viewer; retain scientific functions separately pending their own validation |
| Biomass models, crown metrics and ICESat-2 scripts | Keep methodology unchanged in this phase; no implied scientific validation |
| `ALS_EU` data and country notes | Use as local fixtures and provider leads; do not publish large data or sample assets without confirming their individual terms |
| JRC report and manuscript drafts | Background, product requirements and scientific context; not current proof of access conditions |

## Provider access findings

These are documentation findings, not tested download adapters. Authentication, data license, service quotas and scientific citation are separate fields. Do not infer nationality restrictions from a country's data portal.

| Provider/product | Finding and implementation consequence | Evidence |
|---|---|---|
| OpenTopography hosted point clouds | TileIndex discovery and programmatic tile retrieval are documented pathways. Resolve per-dataset license and citation. | [Official TileIndex tutorial](https://opentopography.org/node/3598), [citation policy](https://opentopography.org/citations) |
| OpenTopography APIs | Limits differ by API/product and account. Keys belong to individual users and must not be shared through a public service. | [Developer documentation](https://opentopography.org/developers) |
| USGS 3DEP via Planetary Computer | Existing code uses `3dep-lidar-copc`; retain as first adapter, but live search, pagination, signing and sample reading still need testing. The originator and delivery host must both be recorded. | Existing `base.R`; [USGS program](https://www.usgs.gov/3d-elevation-program) |
| NEON | Current guidance requires login/token for downloads from June 30, 2026 and announces attribution for newly downloaded data. The workbook's “token recommended” is outdated. Capture the product/release citation. | [NEON current guidance](https://www.neonscience.org/impact/observatory-blog/getting-started-what-new-users-should-know-about-neon-data) |
| Canada CanElevation | Official catalog lists LAZ/COPC, project/tile GPKG indexes and Open Government Licence – Canada. Strong first global addition. | [NRCan dataset](https://open.canada.ca/data/en/dataset/7069387e-9986-4297-9f55-0288e9676947) |
| France IGN LiDAR HD | COPC delivery documented. Former geoservices link now redirects to cartes.gouv.fr. Revalidate discovery and license metadata at the new catalog. | [Catalog](https://cartes.gouv.fr/rechercher-une-donnee/dataset/IGNF_NUAGES-DE-POINTS-LIDAR-HD), [delivery specification](https://geoservices.ign.fr/sites/default/files/2024-09/DL_LiDAR_HD_1-0.pdf) |
| Spain PNOA | LAZ delivery and attribution conditions are documented. Account requirements and automated endpoint remain untested; no evidence here establishes a Spanish-nationality requirement. Use official producer attribution, retaining required original wording where applicable. | [Products](https://pnoa.ign.es/pnoa-lidar/productos-a-descarga), [IGN license](https://www.ign.es/resources/licencia/Condiciones_licenciaUso_IGN.pdf) |
| Finland NLS | Separate 0.5 p open data from licensed 5 p data. A single “Finland is open” flag is incorrect. | [Publication rights](https://www.maanmittauslaitos.fi/en/about-nls/organisation/publications-and-brochures/publication-rights) |
| Sweden Laserdata Skog | Product page states CC0; delivery/account integration still needs testing. | [Official product](https://www2.lantmateriet.se/en/geodata/our-products/product-list/laser-data-download-forest/) |
| Switzerland swissSURFACE3D | Standard geodata requires attribution; format depends on acquisition/release. Sample-data restrictions must not be confused with standard data terms. | [Product](https://www.swisstopo.admin.ch/en/height-model-swisssurface3d), [attribution](https://www.swisstopo.admin.ch/en/source-reference-ogd-swisstopo) |
| Scotland | Public AWS resource documented; retain campaign-specific licensing and distinguish LAS/LAZ from raster derivatives. | [Dataset registry](https://registry.opendata.aws/scottish-lidar/) |
| New Zealand LINZ | Official pages document free point-cloud download/streaming. Determine exact collection endpoint and attribution before activation. | [Elevation access](https://www.linz.govt.nz/products-services/data/types-linz-data/elevation-data/access-elevation-data) |

### Remaining candidates

Keep the following as discoverable candidates with download disabled until validated: Brazil ORNL and Zenodo; ForestScan Paracou/Lopé/Sabah; Panama; Indonesia; Norway; Netherlands AHN; Estonia; Poland; Taiwan/China OpenTopography surveys; South African CEDA collections; Australia ELVIS and additional OpenTopography collections. The supplied ORNL Brazil URL did not return readable content in this review, so its current authentication/terms were not independently confirmed.

Central African reference metrics, AfriSAR waveform products and raw discrete-return ALS need different product types. GlobALS and the GEDI calibration/validation network are discovery leads rather than confirmed public download services. Large ZIP archives are not equivalent to spatially addressable COPC. Determine whether an archive can be selectively accessed before promising AOI-only bandwidth.

Prioritize integrations by both engineering feasibility and geographic value: 3DEP/OpenTopography stabilization → Canada/France/NEON → Spain/Switzerland/Scotland → Brazil and ForestScan tropical sites → remaining regional adapters. This preserves early progress without neglecting Southern Hemisphere coverage.

## OpenTopography limit wording

Do not claim a universal five-kilometre limit or claim that dividing an AOI automatically satisfies all provider conditions. A 5 × 5 km study box is **25 km²**. Study area, tile footprint, API request area, point count and daily quota are different quantities. The published portal limits include point-count limits; raster APIs have their own area limits.

Proposed English copy:

> ALS Downloader identifies source tiles intersecting your study area and retrieves them through provider-supported access methods. Large areas may require multiple transfers. Each transfer and the overall request schedule follow the applicable provider limits. Dataset licenses, attribution requirements and source citations are included with the download manifest.

Only ship that statement once the corresponding controls and manifest exist. The app may plan AOIs larger than 5,000 km², but successful download depends on actual coverage, provider terms, transfer volume and deployment resources. Tile retrieval and local processing should have separately configurable chunk sizes.

## Architecture and acceptance criteria

Suggested modules: `R/aoi.R`, `R/catalog.R`, `R/providers/*.R`, `R/download_policy.R`, `R/download_queue.R`, `R/preview.R`, `R/citations.R`, plus Shiny modules for map, study area, dataset details and jobs. Use a pinned dependency environment and a small local launcher. Keep credentials and large point files out of Git.

Each provider adapter should expose catalog discovery, AOI search, asset resolution, access metadata, preview capability and transfer rules. Registry records should include `provider_id`, `dataset_id`, `asset_id`, geometry/coverage quality, acquisition interval, format, CRS/vertical datum, density, classification, license URL, attribution text, DOI, authentication method, endpoint, quotas, validation status and `verified_at`.

Enable an adapter only after a real AOI resolves real point-cloud assets, a sample can be read, CRS and acquisition metadata are understood, and the download failure path is tested. Test empty coverage separately from authentication, timeout and incomplete pagination.

For downloads, test HTTP failures after a successful HEAD, unsupported HEAD, expired signed URLs, interrupted transfers, duplicate filenames, corrupted existing files and resume after restart. Host mode must not accept arbitrary output paths or user-supplied remote download URLs.

For 3D, clip/sample before materializing large clouds. COPC/EPT range access should be preferred when supported by the actual provider and client. Plain LAZ does not automatically provide efficient spatial random access. Start around 100,000 preview points as a tunable engineering target, benchmark on modest hardware, enforce a total cap across all tiles and progressively refine when appropriate. Use a local coordinate origin to retain rendering precision. Decimation affects visualization only, not the downloaded scientific data or analysis inputs.

Export `manifest.csv`, a machine-readable metadata file, `CITATIONS.bib`/plain text, the AOI and processing settings. Cite source datasets and the application separately. Do not invent a DOI for ALS Downloader; assign one through a release process later if desired.

## Completed checks

- Baseline hashes match the desktop source.
- Full R script parse inventory saved; 50 pass and two prototype failures documented.
- New pure-R worker-policy tests pass, including small machines, unknown core count, hosted restrictions, user overrides, provider caps and empty queues.
- Interface concept tested in Edge: world geometry loads, country/region selection updates details, the 3D control updates the view, and mobile width has no horizontal overflow. Desktop map and cloud renders were visually inspected.
- Full Shiny integration, provider download validation, terrain basemap service selection and deployment testing remain future implementation work.

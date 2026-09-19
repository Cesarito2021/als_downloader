# Access, reuse and acknowledgements review

Reviewed 19 September 2026 against the sources below and the implemented routes.
This records evidence and remaining questions, not blanket legal approval.
Acknowledging a source does not replace its access rules or licence obligations.
External catalogue links are references, not approved in-app download routes.

## Software and cartography

* **Leaflet:** BSD-2-Clause; **leaflet for R:** MIT. These permit integration with
  the GPL-3 application while retaining dependency notices. The application now
  acknowledges them in About and NOTICE. The libraries' original notices remain
  in their distributions. See [Leaflet licence](https://github.com/Leaflet/Leaflet/blob/v1.3.1/LICENSE)
  and [R package](https://github.com/rstudio/leaflet/tree/v2.2.2).
* **Natural Earth / World Atlas:** geographical data are public domain; World
  Atlas processing code has an ISC notice already retained in NOTICE. No
  institutional endorsement is implied. [Natural Earth terms](https://www.naturalearthdata.com/about/terms-of-use/).
* **Esri World Imagery:** the [service item](https://www.arcgis.com/home/item.html?id=10df2279f9684e4a9f6a7f08febac2a9)
  links to the Esri Master License Agreement and an [account-dependent use summary](https://goto.arcgis.com/termsofuse/viewsummary).
  That summary permits qualifying screenshots with attribution, subject to its
  conditions, and requires Esri software or an ArcGIS Online subscription. It
  prohibits systematic tile harvesting and tile redistribution. A screenshot is
  distinct from an offline tile archive. **Pending:** confirm the maintainer's
  institutional entitlement and whether it covers this application and published
  screenshots. Current item credits: Esri, Vantor, Earthstar Geographics, and the
  GIS User Community; retain applicable location-specific credits too.
  **Terrain:** review its separate item terms before public deployment; the
  World Imagery finding does not automatically cover another service.
  [Basemap citation guidance](https://support.esri.com/en-us/knowledge-base/what-is-the-correct-way-to-cite-an-arcgis-online-basema-000012040).

## Integrated data routes

| Source | Finding and acknowledgement | Remaining limits |
|---|---|---|
| USGS 3DEP / Microsoft Planetary Computer | Retain USGS, survey provenance and Microsoft Planetary Computer as the access service. The app obtains temporary SAS links via Microsoft's documented signing endpoint; it does not contain a shared personal API key. [SAS documentation](https://planetarycomputer.microsoft.com/docs/concepts/sas/), [USGS copyright guidance](https://www.usgs.gov/faqs/are-usgs-reportspublications-copyrighted). | The [Microsoft supplemental terms](https://planetarycomputer.microsoft.com/terms), read in the JavaScript-rendered page, restrict token transfer/sharing, permit expiration and rate limits, and provide no availability guarantee. They describe previews as intended for evaluation/testing, while separately stating production use is not prohibited. Check third-party survey notices and the hosted service's token model against these terms; public-domain data do not waive service conditions. |
| OpenTopography | Its [official Tile Index tutorial](https://opentopography.org/node/3598) explicitly documents spatial selection and programmatic LAZ downloads from supplied indexes. This matches the adapter's access pattern; it does not use OT's keyed DEM APIs. Cite the producer, distributor and dataset DOI under the [citation policy](https://www.opentopography.org/citations). | No blanket 5 km restriction on this index-download workflow was found in that tutorial. Limits for a processing API must not be assumed to apply to a different endpoint. Follow dataset licences and current [service conditions](https://opentopography.org/usageterms), including any applicable quotas; the tutorial is not unlimited service capacity or a waiver for arbitrary redistribution. |
| Sao Paulo / BR17_SaoPaulo | Added the PMSP dataset citation, DOI **10.5069/G9NV9GD1**, municipal contributors and AWS hosting credit to tile metadata. [Dataset record](https://portal.opentopography.org/datasetMetadata?otCollectionID=OT.062020.31983.1). | The record lists GNU GPLv3. Retain that designation; a citation alone does not discharge applicable redistribution or derivative obligations. The 2024 publication year is distinct from the 2017 acquisition year. |
| Auckland_2013 | Retain Auckland Council ownership, OpenTopography and DOI **10.5069/G9KW5CZ5**. Figure credits now use the provider's derivative-work ownership wording; raw-file metadata retains its copy wording. [Dataset record](https://portal.opentopography.org/datasetMetadata?otCollectionID=OT.062016.2193.1). | The acknowledgement specifies CC BY 3.0 NZ although the separate licence field is empty. This discrepancy is retained in the review; new datasets cannot inherit this finding. |
| swissSURFACE3D | Existing credit includes **Federal Office of Topography swisstopo**, an accepted mandatory source attribution. [OGD conditions](https://www.swisstopo.admin.ch/en/terms-of-use-free-geodata-and-geoservices). | Reuse, processing and distribution are permitted under the conditions; excessive service use may be restricted. ZIP extraction does not change the licence. |
| CanElevation | Added the standard OGL Canada attribution to the existing Natural Resources Canada credit and product link. [OGL Canada](https://open.canada.ca/en/open-government-licence-canada). | Preserve any additional dataset-specific notices and exclusions; no endorsement or rights over official symbols are granted. |
| IGN LiDAR HD | Retain IGN, the dataset link, Licence Ouverte 2.0 and UMR TETIS / INRAE for the discovery catalogue. [Licence](https://www.data.gouv.fr/pages/legal/licences/etalab-2.0). | Source and information-update date are attribution requirements. The app does not yet preserve a verified data-update date for every tile; this is pending. Do not substitute the acquisition date or an unverified catalogue timestamp. |
| AHN6 | Preserve AHN and the CC BY 4.0 link recorded in the adapter; identify display modifications. [AHN6 release](https://www.ahn.nl/eerste-deel-ahn-6-beschikbaar), [official access routes](https://www.ahn.nl/dataroom). | The dataroom explicitly provides point-cloud downloads and OGC index links. No inference that older AHN licences apply to AHN6; service limits still apply. |
| Zenodo contributions | Metadata retain the depositor's authors, DOI and supported explicit licence. Approval is required before registration. | Evaluate each record, asset, boundary and access condition separately; Zenodo hosting is not a universal licence. No new deposit was approved in this review. |

## API keys and funding

USGS 3DEP has documented public access routes; NASA Earthdata registration is
not a requirement of this app's Planetary Computer route. The [USGS AWS registry](https://registry.opendata.aws/usgs-lidar/)
identifies its public EPT bucket as account-free and US Government Public Domain,
while its raw LAZ bucket uses requester-pays access. These are different services
from the app's current COPC route. Another R package's existence is not the
licence evidence; the provider's documentation is.

OpenTopography prohibits sharing personal API keys or embedding one in a service
that bypasses its individual-key rule. Commercial API integration requires its
Enterprise arrangement. If keyed OT APIs are introduced, implement the permitted
credential model and the API acknowledgement from its current terms; this review
does not add an unused API acknowledgement or claim a partnership.

The software's funding credit remains **OpenForest4D, NSF awards 2409885, 2409886
and 2409887**. OpenTopography's facility funding is separate and must never be
presented as funding received by ALS Downloader.

Source credits and licence links travel in exported tile metadata and report
citations. Point-cloud figures retain source notices and display transformations.
The licensing download includes this review so unresolved items remain visible.
No provider was contacted, no key was purchased, and no new agreement was accepted
on behalf of the maintainer during this review.

## Remaining intellectual-property inventory

| Product or component | Treatment / evidence | Status |
|---|---|---|
| Original R and interface code | GPL-3; Cesar Ivan Alvites Diaz is the declared author and maintainer in DESCRIPTION and CITATION. No new paper coauthors are added to the software. Git author labels and paid AI assistance do not, by themselves, establish ownership of every line. Preserve genuine third-party notices and provenance. | Authorship metadata checked; no exhaustive historical code-provenance or institutional ownership determination. |
| R dependencies and browser libraries | Their licences remain attached to their distributions; see [dependency inventory](DEPENDENCIES.md) and [NOTICE](../NOTICE). Leaflet's permission does not license Esri imagery. The bundled html2canvas distribution retains its MIT notice. | Notices inspected; verify the exact release dependency bundle during release checks. |
| Icons, banner and portrait | Icons are supplied through Shiny's dependencies, not copied provider logos. The portrait was obtained from the author's own website with his authorization. Banner history (68cb457, 0a9ead9, 20ea191) records a procedurally generated illustration, not survey points or borrowed provider branding; SVG source is retained. | Repository provenance reviewed. The synthetic banner is decorative and is not represented as scientific evidence. |
| Catalogue and provider names | Descriptive source names and official links identify access; they do not imply endorsement. A descriptive package name is not a trademark clearance. | No provider branding rights claimed. |
| Submitted boundaries and metadata | Require contributors to have rights to share the boundaries and links. Record-specific licences apply to Zenodo and other repositories. Maintainer approval does not relicense a contribution. | Verify each accepted proposal; do not infer rights from public access alone. |
| Exported figures, tables and PDFs | Preserve producer credits, licence links and display-transformation notes. Imagery and LiDAR may have different rights and dates. An export does not automatically inherit the software's GPL-3 licence. | Esri entitlement and IGN update-date attribution remain pending as above. |
| Temporary cloud copies / hosted downloads | The viewer may download files temporarily; hosted transfers can relay files. This is more than linking to a portal. Apply the source licence and service conditions to this actual behaviour. | No claim that the application never stores data or that all publicly reachable data are freely redistributable. |

Dataset rights remain with their respective rights holders, which may be an
agency, institution, company or individual, rather than a country as such.
For a CRAN release, software distribution and notices must be reviewed separately
from the terms for operating a public Shiny service. This review identifies
specific evidence and open items; it does not certify universal legal clearance.
The [CRAN Repository Policy](https://cran.r-project.org/web/packages/policies.html)
requires unambiguous ownership and preserved third-party authorship/licence
notices, including components downloaded during use. It also requires suitable
source availability. The banner SVG is now included at `inst/report/banner.svg`,
even though the documentation gallery is excluded from the package archive.

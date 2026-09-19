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
* **Esri World Imagery and terrain:** retain Esri and contributor attribution on
  the map and exported images. These are licensed services, not public-domain
  imagery provided by Leaflet. Esri distinguishes qualifying noncommercial uses
  from commercial uses and reserves service-specific conditions. **Pending before
  public hosting:** verify the intended deployment and screenshot/export use
  against the actual service's item terms and any applicable account agreement.
  A working anonymous tile URL is not enough to resolve that question.
  [Service terms](https://www.esri.com/en-us/legal/terms/web-site-service),
  [basemap citation guidance](https://support.esri.com/en-us/knowledge-base/what-is-the-correct-way-to-cite-an-arcgis-online-basema-000012040).

## Integrated data routes

| Source | Finding and acknowledgement | Remaining limits |
|---|---|---|
| USGS 3DEP / Microsoft Planetary Computer | Retain USGS, survey provenance and Microsoft Planetary Computer as the access service. The app obtains temporary SAS links via Microsoft's documented signing endpoint; it does not contain a shared personal API key. [SAS documentation](https://planetarycomputer.microsoft.com/docs/concepts/sas/), [USGS copyright guidance](https://www.usgs.gov/faqs/are-usgs-reportspublications-copyrighted). | Third-party survey notices still need checking. The [full Microsoft service terms](https://planetarycomputer.microsoft.com/terms) were not fully retrievable in this review; deployment-specific token/service compliance remains to be closed. |
| OpenTopography | Cite both the dataset producer and distributor, including the dataset DOI. [Citation policy](https://www.opentopography.org/citations), [service terms](https://opentopography.org/usageterms). The adapter reads configured TileIndex files and their download URLs; it does not use OT's keyed DEM APIs. | Account requirements apply to some services. Do not assume direct URL reachability authorizes a hosted download relay. Confirm that intended use for each bulk route before public hosting; no approval has been requested or received. |
| Sao Paulo / BR17_SaoPaulo | Added the PMSP dataset citation, DOI **10.5069/G9NV9GD1**, municipal contributors and AWS hosting credit to tile metadata. [Dataset record](https://portal.opentopography.org/datasetMetadata?otCollectionID=OT.062020.31983.1). | The record lists GNU GPLv3. Retain that designation; a citation alone does not discharge applicable redistribution or derivative obligations. The 2024 publication year is distinct from the 2017 acquisition year. |
| Auckland_2013 | Retain Auckland Council ownership, OpenTopography and DOI **10.5069/G9KW5CZ5**. Figure credits now use the provider's derivative-work ownership wording; raw-file metadata retains its copy wording. [Dataset record](https://portal.opentopography.org/datasetMetadata?otCollectionID=OT.062016.2193.1). | The acknowledgement specifies CC BY 3.0 NZ although the separate licence field is empty. This discrepancy is retained in the review; new datasets cannot inherit this finding. |
| swissSURFACE3D | Existing credit includes **Federal Office of Topography swisstopo**, an accepted mandatory source attribution. [OGD conditions](https://www.swisstopo.admin.ch/en/terms-of-use-free-geodata-and-geoservices). | Reuse, processing and distribution are permitted under the conditions; excessive service use may be restricted. ZIP extraction does not change the licence. |
| CanElevation | Added the standard OGL Canada attribution to the existing Natural Resources Canada credit and product link. [OGL Canada](https://open.canada.ca/en/open-government-licence-canada). | Preserve any additional dataset-specific notices and exclusions; no endorsement or rights over official symbols are granted. |
| IGN LiDAR HD | Retain IGN, the dataset link, Licence Ouverte 2.0 and UMR TETIS / INRAE for the discovery catalogue. [Licence](https://www.data.gouv.fr/pages/legal/licences/etalab-2.0). | Source and information-update date are attribution requirements. The app does not yet preserve a verified data-update date for every tile; this is pending. Do not substitute the acquisition date or an unverified catalogue timestamp. |
| AHN6 | Preserve AHN and the CC BY 4.0 link recorded in the adapter; identify display modifications. [AHN6 release](https://www.ahn.nl/eerste-deel-ahn-6-beschikbaar), [official access routes](https://www.ahn.nl/dataroom). | The dataroom explicitly provides point-cloud downloads and OGC index links. No inference that older AHN licences apply to AHN6; service limits still apply. |
| Zenodo contributions | Metadata retain the depositor's authors, DOI and supported explicit licence. Approval is required before registration. | Evaluate each record, asset, boundary and access condition separately; Zenodo hosting is not a universal licence. No new deposit was approved in this review. |

## API keys and funding

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

# European airborne LiDAR access review

Follow-up: [live access checks](../inst/sources/VALIDATION.md) now establish
French public spatial-catalogue-to-file access, Luxembourg index/ZIP access,
and Wallonia index access. Spain's download-init returned 403. These findings
supersede the corresponding pending access checks below; connectors remain pending.

Screened 17 September 2026. This is a discovery and access review, **not a claim
that every country has a working connector**. It combines the supplied JRC
report and document inventory with current provider documentation. Only AHN6
and swissSURFACE3D have the native European adapters and live checks described
in [EUROPE_INTEGRATION.md](EUROPE_INTEGRATION.md).

## Admission criteria

Use original airborne or UAV **laser** point clouds, an explicit open licence,
provider tile polygons or a documented study boundary, and an official mapping
from each downloadable file to its footprint. Preserve the provider's files,
grid, attribution and collection dates. A terrain raster, point representation
of a terrain model, map image, or 3D viewer alone does not satisfy these criteria.

Automatic downloads must work anonymously through an authorised public route.
Do not bypass accounts, API keys, identity forms, CAPTCHA, payment, permissions,
rate limits or provider selection limits. Do not split selections or rotate
sessions to evade limits. Stop at access failures; a public URL alone is not
evidence of an open licence. Account-dependent products can have an explanatory
official portal link, but cannot be labelled as anonymous in-app downloads.

An open licence and anonymous access are separate requirements. In particular,
an agency can publish open data through an authenticated service. No credentials
were supplied and no restricted download was attempted during this screening.

## Sources with documented clouds or actionable leads

`Implemented` means an existing local adapter, not publication on GitHub.
`Candidate` means documented cloud delivery; API, licensing or footprint checks
listed below remain necessary before enabling automatic download.

| Country / region | Provider evidence and original organization | Access / integration decision |
|---|---|---|
| Netherlands | [AHN dataroom](https://www.ahn.nl/dataroom): AHN6 1 km LAZ tiles and OGC feature index. | **Implemented AHN6**; anonymous index and sample LASF range response checked. Other generations not covered by this adapter. |
| Switzerland | [swisstopo STAC](https://docs.geo.admin.ch/download-data/stac-api/overview.html): swissSURFACE3D item polygons and original LAS ZIP assets. | **Implemented**; anonymous queries and sample downloads checked. [Attribution required](https://www.swisstopo.admin.ch/en/terms-of-use-free-geodata-and-geoservices). |
| Spain | [IGN PNOA products](https://pnoa.ign.es/pnoa-lidar/productos-a-descarga): LAZ; first coverage 2 x 2 km, second 2 x 2 km with 1 x 1 km exceptions, third 1 x 1 km. | **Candidate, priority**. [CNIG download manual](https://centrodedescargas.cnig.es/CentroDescargas/txtInfoDesc/ManualDescargaAutomatica_ES.pdf) limits unregistered downloads to 20 products. Native index-to-file connector pending. Never evade this cap. |
| France | [IGN LiDAR HD](https://geoservices.ign.fr/lidarhd), [delivery specification](https://geoservices.ign.fr/sites/default/files/2024-09/DL_LiDAR_HD_1-0.pdf): 1 km point-cloud tiles, LAZ/COPC. | **Candidate, priority**. Catalogue link already present; native polygon/asset query and anonymous sample transfer still to implement/test. |
| Luxembourg | [ACT LiDAR 2019](https://data.public.lu/en/datasets/lidar-2019-releve-3d-du-territoire-luxembourgeois/): official GeoJSON and Shapefile tile indices, LAZ grouped in ZIPs; CC0. | **Candidate, priority**. Public resource links documented. Inspect official index properties and map them to assets before integration; do not use the optional API-key bulk script. |
| Belgium — Wallonia | [SPW point-cloud grid](https://geoportail.wallonie.be/catalogue/7341def8-8ea4-4031-9b2a-4a31f1954d05.html): 500 m tiles, LAZ names/sizes, first and last flight dates; CC BY 4.0. | **Candidate, priority**. Public [ArcGIS index](https://geoservices.wallonie.be/arcgis/rest/services/RELIEF/LIDAR_MAILLES/MapServer). Live index parsing and direct LAZ delivery remain untested here; custom emailed delivery is not an instant connector. |
| Belgium — Flanders | [Digitaal Vlaanderen DHMV II](https://www.vlaanderen.be/digitaal-vlaanderen/onze-diensten-en-platformen/basiskaart-vlaanderen-grb/dhmv-ii-brondata): original airborne LAZ, 500 m flight-strip tiles; links to EODaS Open LiDAR. | **Candidate**. Provider explicitly describes public download and integration in other applications. Verify tile endpoint, licence and sample access before enabling. Not a Belgium-wide dataset. |
| Germany — Saxony | [GeoSN LSC download](https://www.geodaten.sachsen.de/downloadbereich-digitale-hoehenmodelle-4851.html): 2 km LAZ tile delivery in ZIPs and acquisition metadata. | **Candidate**. Regional source already catalogued. Native index/ZIP connector pending; no nationwide German coverage claim. Other states need separate product-level review. |
| Norway | [Geonorge height data](https://www.geonorge.no/kartdata/datasett-i-geonorge/hoydedata/): project-based laser point clouds through Hoydedata. | **Candidate**. Verify public project footprints, export route and licence individually; do not infer that every partner product is anonymous. |
| Poland | [GUGiK LiDAR measurements](https://www.geoportal.gov.pl/en/data/lidar-measurements-lidar/), [WFS services](https://www.geoportal.gov.pl/pl/usluga/uslugi-pobierania-wfs/): downloadable laser measurements and spatial indices. | **Candidate**. Resolve WFS feature-to-cloud links, dates and anonymous sample transfer before an adapter. |
| Estonia | [Land Board downloads](https://geoportaal.maaamet.ee/eng/spatial-data/elevation-data/download-elevation-data-p664.html): elevation products include laser point-cloud downloads. | **Candidate**. Retain original cloud files; check current index-to-LAZ mapping, licence and transfer separately from raster products. |
| Slovenia | [GURS announcement, June 2026](https://www.gov.si/novice/2026-06-03-lidarski-podatki-za-celotno-slovenijo-dostopni-v-pregledovalniku-3d-podatkov/): 2023–2025 surveys, 1 km tiles, capture dates and technical reports; GKOT classified clouds in LAZ. | **Candidate, priority**. Public viewer/download without registration; **maximum ten sheets per download**. Select GKOT, not DMR/DMP terrain/surface products even when also LAZ. Verify licence and API before integration. |
| Slovakia | [GKU ALS provision](https://www.gku.sk/geoportal-en/zbgis/als/provision-als-products/?forceBrowserDetector=blind): classified point clouds via MAPKA, alongside terrain/surface rasters. | **Candidate with export cap**. Current provider description allows cloud sections up to **4 km2**. Whole-country raster ZIP access is not whole-country cloud access. Auth/licence/endpoint checks pending; no bulk workaround. |
| Portugal | [DGT catalogue](https://cdd.dgterritorio.gov.pt/dgt-fe/catalogos): airborne LiDAR collection includes point clouds as well as MDT/MDS. | **Candidate**. Interactive basket; [download list expires after 24 hours](https://cdd.dgterritorio.gov.pt/dgt-fe/downloads?language=en). Native boundary/LAZ mapping and current request limits/licence need verification. Do not substitute terrain rasters. |
| United Kingdom — England | [Environment Agency National LiDAR Programme](https://www.data.gov.uk/dataset/f0db0249-f17b-4036-9e65-309148c97ce4/national-lidar-programme): 5 km LAZ tiles, survey dates, WFS and downloadable Shapefile/GPKG/GeoJSON indices. | **Candidate, priority**. OGL, no public-access constraint documented. [Index service](https://environment.data.gov.uk/KB6uNVj5ZcJr7jUP/ArcGIS/rest/services/National_LIDAR_Programme_Catalogues/FeatureServer/0); asset resolution and sample transfer pending. |
| United Kingdom — Scotland | [Scottish Public Sector LiDAR](https://registry.opendata.aws/scottish-lidar/): original data organised by survey phase. | **Mixed licences**. General OGL does not cover the phase-2 LAZ noncommercial exception. Only individually confirmed open phases can qualify. Wales and Northern Ireland require their own cloud/footprint checks. |
| Finland | [NLS file updating service](https://www.maanmittauslaitos.fi/en/e-services/open-data-file-download-service/open-data-file-updating-service-interface): file feeds including LAZ; personal API key. | **No authenticated connector under current scope**. Keep the open 0.5-point product distinct from restricted/paid higher-density products. An alternative anonymous documented route would need separate verification. |
| Denmark | [DHM point-cloud file service](https://datafordeler.dk/dataoversigt/danmarks-hoejdemodel-dhm/dhm-fildownload-punktsky/), [Dataforsyningen API documentation](https://docs.dataforsyningen.dk/). | **Authentication-dependent route identified**. Token-dependent APIs are not enabled; public viewing does not establish anonymous point-cloud download. |
| Sweden | [Lantmateriet laser-data access update](https://www.lantmateriet.se/sv/geodata/vara-produkter/Produktnyheter/Geografisk-information/uppdatering-angaende-tillhandahallandet-av-laserdata-nedladdning-skog/). | **Account/acceptance conditions**. Do not add an anonymous connector based on historical open-data assumptions. |
| Lithuania | [National Land Service access instructions, March 2026](https://nzt.lrv.lt/lt/naujienos/kaip-gauti-valstybinius-erdvinius-duomenis-CkKP/): LiDAR available through geoportal.lt. | **Registration required for described ordering route**; no automatic anonymous integration. Large orders can require a signed request. |
| Latvia | [LGIA airborne LAS product](https://www.lgia.gov.lv/en/Digit%C4%81lais%20virsmas%20modelis), [open-data licence](https://www.lgia.gov.lv/lv/atvertie-dati): classified ALS with download and acquisition-time links. | **Candidate, unresolved route**. Open product documented; embedded download/index not tested. Separate request forms exist, but they do not prove this open product requires registration. |
| Austria — Tirol | [Official laser-data page](https://www.tirol.gv.at/sicherheit/geoinformation/geodaten-tiris/laserscandaten): free derived products; original LAS/LAZ supplied by request with processing charges. | **Do not enable this cloud route**. Other Austrian states remain unresolved; this is not a national prohibition. |
| Italy — Tuscany / Emilia-Romagna | [Tuscany catalogue](https://dati.toscana.it/dataset/lidar); [Emilia-Romagna survey](https://geoportale.regione.emilia-romagna.it/approfondimenti/rer23_24) and [coverage service notice](https://geoportale.regione.emilia-romagna.it/in-primo-piano/rer23-24-aggiornamento-lug-25). | **Unresolved original-cloud delivery**. Surveys and footprint layers exist, but public raster delivery is not sufficient evidence of anonymously downloadable LAZ with an open licence. Review regions individually. |
| Andorra | [Government airborne survey](https://www.cartografia.ad/index.php?Itemid=830&id=245&option=com_content&view=article): 2018/2025 LAZ, 3.5 km sheets and flight dates. | [Download portal](https://www.ideandorra.ad/geodades/index.jsp?pog=lidar2018laz) requests mandatory name/email/country/activity. **Do not automate past this identification form**. The separate road/mobile LiDAR viewer is out of scope. |
| Liechtenstein | [Government geoportal](https://service.geo.llv.li/): 2018/2024 laser viewers and a download area. | **Unresolved**. Viewer existence does not establish original LAS/LAZ downloads, licence or anonymous access. Protected geoshop is not an allowed route. |
| Ireland | [GSI Phase-2 index](https://gsi.geodata.gov.ie/server/rest/services/Lidar/IE_GSI_LiDAR_Coverage_GSI_Phase2_IE26_ITM/MapServer): 2 km footprints, acquisition metadata, but described links download **rasters**. | **Cloud route unresolved**. [OPW open-data release](https://www.gov.ie/en/office-of-public-works/press-releases/opw-releases-lidar-captured-as-part-of-flood-risk-management-projects-as-open-data/) is CC BY 4.0; establish original clouds for each product rather than assuming all GSI tiles contain LAS/LAZ. |

## Remaining countries: screened, not approved for integration

These are explicit research gaps, **not conclusions that no ALS exists**.
Broad searches with no qualifying result do not prove absence. No country in
this table should be shaded as downloadable ALS solely from this review.

| Country | Screening result / evidence |
|---|---|
| Albania | [ASIG ATOM instructions](https://geoportal.asig.gov.al/en/node/3258) describe downloads for registered users. No qualifying anonymous ALS tile route established. |
| Belarus | Country-specific search yielded no verified primary-source open ALS tile/asset chain. |
| Bosnia and Herzegovina | [RGURS national LiDAR project](https://rgurs.org/stranica/lidar) confirms acquisition project; no verified anonymous licensed original-cloud tile delivery. |
| Bulgaria | [National INSPIRE portal](https://inspire.egov.bg/) identified; no qualifying original-cloud index/asset chain established. |
| Croatia | [DGU access information](https://dgu.gov.hr/kako-doci-do-prostornih-podataka/6764) and [LiDAR request category](https://dgu.gov.hr/proizvodi-i-usluge/podnesite-zahtjev-91/91) found; anonymous cloud tile delivery remains unverified. Orthophoto labelled LiDAR is not a point cloud. |
| Cyprus | [COASTLINE research dataset](https://zenodo.org/records/21628561) describes aerial/UAV laser clouds; no qualifying file-to-boundary mapping established in this screening. Do not activate a Zenodo record without the required boundary and licence check. |
| Czechia | [CUZK open-data policy](https://cuzk.gov.cz/Uvod/Produkty-a-sluzby/Otevrena-data/Otevrena-data-zakladni-informace.aspx) includes elevation products, but [DMR5G specification](https://geoportal.cuzk.gov.cz/Dokumenty/TECHNICKA_ZPRAVA_DMR_5G.pdf) describes terrain XYZ delivery. Full original airborne cloud route unverified. |
| Greece | Same [COASTLINE lead](https://zenodo.org/records/21628561); current official national cloud tiles not established. Research record not admitted without boundary/licence checks. |
| Hungary | [Lechner product contact](https://lechnerkozpont.hu/oldal/kapcsolat) describes ordered elevation data; no verified anonymous original ALS tile delivery. |
| Iceland | [National spatial-data infrastructure](https://www.natt.is/en/resources/spatial-data-infrastructure) found; no qualifying ALS tile/asset chain established. Search results for backpack/terrestrial laser scans do not qualify. |
| Kosovo | Search did not establish an official open ALS tile/asset chain. EuroDEM is a terrain raster and does not qualify. |
| Malta | [Official 2018 DSM](https://portal.data.gov.mt/dataset/digital-surface-model-2018) derives from airborne LiDAR; original cloud download/index not verified. |
| Moldova | Search did not establish a primary-source open ALS tile/asset chain. |
| Monaco | Search did not establish a primary-source open ALS tile/asset chain. |
| Montenegro | Search did not establish a primary-source open ALS tile/asset chain. |
| North Macedonia | [AREC strategic plan](https://www.katastar.gov.mk/wp-content/uploads/en/documents/Strategic_plan/Strategic_work_plan_AREC_2020_2022_final.pdf) mentions a LiDAR project; no current qualifying anonymous tile delivery established. |
| Romania | [ANCPI LAKI-II description](https://geoportal.ancpi.ro/portal/sharing/rest/content/items/4c820fcf2111417cb062acdcf7f6fef1/data) describes a LiDAR-derived terrain model; raw clouds cannot be inferred from that download. |
| Russia | Search did not establish a primary-source open ALS tile/asset chain. |
| San Marino | [Government cadastral report](https://www.gov.sm/pub1/GovSM/dam/jcr%3A6e2f35fe-0300-402a-b645-668f4f741a6a/Prima%20Statistica%20Catastale%20-%20Catasto%20Fabbricati.pdf) mentions a 2023 LiDAR flight, but no open tile/asset chain was established. [La Guiata](https://openheritage3d.org/project.php?id=ywq4-dj36) is terrestrial LiDAR/aerial photogrammetry and is excluded. |
| Serbia | Search found mobile/photogrammetric examples and regional projects, but no qualifying national anonymous ALS tile chain. |
| Ukraine | Search did not establish a primary-source open ALS tile/asset chain. |
| Vatican City | Targeted search did not establish a qualifying primary-source ALS tile chain. Regional terrain-model references do not establish original clouds. |

Expanded geographical scope was also screened for **Armenia, Azerbaijan,
Georgia, Kazakhstan and Turkiye**. No qualifying anonymous open ALS tile chain
was established. These are unresolved, not unavailable. Overseas territories,
dependencies and every regional or university repository have not been audited.
The screening is deliberately broader than the EU and is not a political
definition of Europe.

## Practical integration order

1. Spain and France: retain their native tiles; resolve official spatial
   indices to file URLs and enforce documented download limits.
2. Luxembourg, Belgium, England and Slovenia: strong provider documentation
   for actual clouds and spatial organization; complete licence/index/asset tests.
3. Poland, Estonia, Norway, Portugal, Slovakia, Latvia and German regional
   sources: resolve provider-specific delivery and conditions before enabling.
4. Keep unresolved or gated products out of automatic downloads. Reassess when
   a provider publishes an appropriate anonymous route; never infer availability
   from a country's boundary or from the existence of a LiDAR survey.

No new adapter or new active provider was enabled by this document. The review
does not certify uptime, every tile, or permissions for all products of an agency.

## Checked and deferred: Norway, Poland, Estonia, Germany (18 September 2026)

After building live-query adapters for France (STAC, confirmed file download)
and a local-index adapter for Canada (confirmed public S3 bucket file
download), these four were reviewed as the next candidates and found to lack
comparable evidence - **no adapter was built for any of them**:

- **Poland (GUGiK)**: `docs/dataset-candidates.csv` records the official page
  as reachable (HTTP 200, after an initial TLS certificate-chain failure) and
  documents WMS GetFeatureInfo download URLs and WFS indexes by vertical
  datum, but explicitly notes "**File untested**" - no anonymous file
  download was ever confirmed.
- **Norway (Kartverket/Hoydedata), Estonia (Maa-amet), Germany/Saxony
  (GeoSN)**: only page-level reachability is recorded (`docs/link-checks.csv`
  for Estonia); none has a confirmed anonymous LAS/LAZ file download.

Building a local-index adapter (the pattern used for CanElevation) does not
strictly require a live-tested file download, since the app only reads a
tile index the user already downloaded from the official portal themselves -
but it does require knowing the index's actual field schema and file-host
pattern well enough to write correct, non-speculative code, and none of
these four have that documented yet. Revisit once a real file
download (host, response code, LAS signature) is confirmed for one of them,
the way `docs/file-access-checks.csv` already does for Canada and Switzerland.

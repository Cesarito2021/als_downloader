# Source and access review

Reviewed on **17 September 2026**. All 45 supplied records were considered, including repeated entries.
Original country labels, descriptions and URLs are preserved for traceability in
[dataset-candidates.csv](dataset-candidates.csv); use the added review columns as the current assessment.
Repeated records do not establish additional coverage. No new national download adapter was enabled by this review.

## What was checked

* **HTTP reachability:** bounded GET requests, redirects and titles, with TLS verification enabled. Final sweep: 48 URLs, 43 HTTP 200 responses, two HTTP 403 responses, two NEON certificate failures and one EMBRAPA timeout. Results in [link-checks.csv](link-checks.csv).
* **Identity and scope:** official product pages, dataset metadata, DOI registries and provider documentation. A homepage or DOI resolving successfully does not prove a point-cloud download.
* **File access:** Canadian COPC header read anonymously; Swiss ZIP downloaded and 930 LAS points decoded using `lidR`. Results and SHA-256 in [file-access-checks.csv](file-access-checks.csv).
* **Earlier integrated samples:** USGS, Australia, Brazil and New Zealand ALS files were decoded on 16 September. Taiwan was also decoded but is photogrammetry. See [validation.csv](validation.csv).

HTTP failures remain explicit. Initial requests failed certificate verification for Sweden, Finland, Poland and NEON. Sweden, Finland and Poland subsequently returned HTTP 200 with verification enabled; two NEON website pages still failed. The NEON product portal returned HTTP 200 and its official tutorial was reviewed through web search. Certificates were not bypassed.
The MDPI publisher request returned 403; Crossref confirmed the GlobALS publication identity.
No account was created and no credentials were supplied to third-party data services. Authenticated downloads remain untested.

## Corrections that affect scientific interpretation

| Supplied record | Finding | Consequence |
|---|---|---|
| East Helanshan, China | Provider identifies satellite photogrammetry. | Exclude from ALS coverage; LAZ is a file format, not an acquisition method. |
| TW18_Carr, Taiwan | Provider identifies UAV structure-from-motion. | Retain the transport test, but withdraw the ALS sample claim. |
| AfriSAR DOI 1681 | Aboveground biomass GeoTIFF maps. | Related research, not a raw LAS/LAZ source. |
| ORNL DOI 2481 | GEDI/ALS-derived metrics, GeoTIFF, CSV and KML products. | Regional bounds do not verify point clouds in every named country. |
| ForestScan collection | Mix of TLS, UAV-LS, ALS and census data; not all types at all sites. | Confirm site and acquisition method before claiming ALS availability. |
| Switzerland + Liechtenstein | Swiss product and Swiss sample verified. | Liechtenstein scope remains unconfirmed. |

## Country and dataset findings

The table groups exact duplicate submissions; country/site variants remain explicit.

| Country / region | Reviewed source | Product / status | Finding and access limitation |
|---|---|---|---|
| Brazil (Mato Grosso, Amazonas, Pará) | [Source](https://zenodo.org/records/7636454) | ALS point clouds in ZIP; Metadata and file listing verified | EBA transects; record credits Ometto et al.; CC BY 4.0. Large archives listed; full download not tested. |
| Brazil (multiple biomes) | [Source](https://www.embrapa.br/en/busca-de-noticias/-/noticia/15706279/web-system-offers-lidar-data-on-brazilian-biomes) | Discovery/news; Landing page only | News page is reachable but displays an election-period content restriction. This is not a file endpoint; use the ORNL Brazil record for documented data. Final recheck timed out; initial HTTP 200 content inspection only. |
| French Guiana | [Source](https://catalogue.ceda.ac.uk/uuid/1d554ff41c104491ac3661c6f6f52aab/) | ALS LAZ; Public file directory verified | Paracou November 2019; CC BY 4.0; public LAZ directory reachable. Full point-stream decoding not tested. |
| Gabon | [Source](https://doi.org/10.5285/88a8620229014e0ebacf0606b302112d) | Mixed TLS/UAV-LS/ALS collection; Collection verified; ALS claim narrowed | Lopé archive lists TLS and UAV-LS folders. The collection DOI does not establish a conventional airborne ALS file for every site. |
| Malaysia (Sabah, Borneo) | [Source](https://doi.org/10.5285/88a8620229014e0ebacf0606b302112d) | Mixed TLS/UAV-LS/ALS collection; Collection verified; site-specific access unresolved | ForestScan covers Paracou, Lopé and Kabili-Sepilok; inspected Malaysia SEP-11 is a scanning-project directory. This DOI does not verify Danum Valley or SAFE campaign ALS. |
| Indonesia (Kalimantan, Borneo) | [Source](https://doi.org/10.3334/ORNLDAAC/1518) | ALS point clouds; Metadata verified; authenticated file access untested | CMR collection CMS_LiDAR_Indonesia_1518 confirms 2014 airborne surveys. Earthdata download workflow required; do not substitute derived raster product 1540. |
| Panama | [Source](https://doi.org/10.60635/C3F593) | ALS LAZ plus derived products; Public file directory verified | DataCite title matches 2023 Central Panama surveys; public BCI directory contains classified and unclassified LAZ folders. Dataset-specific reuse terms still need review. |
| Australia | [Source](https://portal.opentopography.org/datasets) | Mixed topographic catalog; Catalog reachable; representative ALS sample passed | AUS11_Victor representative ALS tile decoded on 2026-09-16; this does not validate every Australian catalog entry or ELVIS ordering. |
| Australia | [Source](https://elevation.fsdf.org.au/) | Elevation discovery portal; Portal verified; ordering untested | ELVIS is a discovery/order portal with multiple producers and products; choose point clouds and check individual terms. |
| South Africa | [Source](https://data-search.nerc.ac.uk/geonetwork/srv/api/records/a2e82c7f92dc4f389a7fb7e4e6629c9e) | ALS and aerial imagery; Metadata verified; registration required | Kruger rivers May 2012; metadata requires a registered CEDA account, OGL v3 and citation. Correct producer is Southern Mapping Geospatial; file download untested. |
| Global (discovery layer) | [Source](https://doi.org/10.3390/rs12111877) | Discovery literature; Publication identity verified | Crossref title confirms GlobALS. Publisher HTTP 403 in direct check; this is a providers database paper, not downloadable nationwide ALS. |
| Global (discovery layer) | [Source](https://gedi.umd.edu/science/calibration-validation/) | Discovery network; Landing page verified | Calibration/validation collaborator network; not a uniform open ALS archive or coverage index. |
| Canada | [Source](https://open.canada.ca/data/en/dataset/7069387e-9986-4297-9f55-0288e9676947) | ALS COPC LAZ; Public file header verified | CanElevation national product metadata, tile/project index links and public bucket confirmed. A 65536-byte range is LAS 1.4 point format 6; no full tile decoding. Open Government Licence Canada. |
| Central Africa | [Source](https://doi.org/10.3334/ORNLDAAC/2481) | GEDI/ALS-derived metrics and maps; Excluded from raw ALS download sources | Published formats are GeoTIFF, CSV and KML. ALS reference metrics are not evidence of LAS/LAZ availability in each country within the regional map extent. |
| Gabon | [Source](https://doi.org/10.3334/ORNLDAAC/1681) | Aboveground biomass GeoTIFF; Excluded from raw ALS download sources | AfriSAR biomass maps for Gabon, not the raw airborne point-cloud dataset; retain as related research only. |
| United Kingdom – Scotland | [Source](https://registry.opendata.aws/scottish-lidar/) | ALS LAZ and DTM/DSM; Metadata and public bucket verified | AWS bucket listing accessible without account. OGL v3 except phase-2 LAZ, which uses a non-commercial government licence. File decoding untested. |
| Switzerland + Liechtenstein | [Source](https://www.swisstopo.admin.ch/en/height-model-swisssurface3d) | ALS LAS/COPC; Public sample decoded | Swiss STAC asset downloaded: ZIP 11225 bytes, 930 LAS points decoded with lidR. Country scope confirmed for Switzerland; supplied Liechtenstein claim not independently established. |
| Netherlands | [Source](https://www.ahn.nl/dataroom) | ALS LAZ; Product and native grid documented | AHN official dataroom lists LAZ downloads and tile indexes; AHN2-5 sheets differ from AHN6 1 km grid. Native adapter and file download untested. |
| Sweden | [Source](https://www2.lantmateriet.se/en/geodata/our-products/product-list/laser-data-download-forest/) | ALS LAZ; Official metadata verified; final HTTP check passed | Official product: 1-2 points/m2, CC0, Geotorget delivery. Web-indexed source read; initial request failed certificate-chain validation; final verified-TLS request returned HTTP 200. No TLS bypass; remote file untested. |
| Estonia | [Source](https://geoportaal.maaamet.ee/eng/Spatial-Data-p58.html) | ALS elevation points; Official download guidance verified | Official portal links elevation-point downloads. Official FAQ documents per-sheet LAZ URLs; sample download and adapter untested. |
| Poland | [Source](https://www.geoportal.gov.pl/en/data/lidar-measurements-lidar/) | ALS LAS/LAZ; Official access workflow verified; final HTTP check passed | Official page documents WMS GetFeatureInfo download URLs and WFS indexes by vertical datum; initial root request failed certificate-chain validation; final verified-TLS request returned HTTP 200. File untested. |
| China – Ningxia | [Source](https://portal.opentopography.org/lidarDataset?opentopoID=OTLAS.062016.32648.1) | Photogrammetric point cloud; Excluded from ALS coverage | East Helanshan is Pleiades photogrammetry; LAZ format does not imply laser acquisition. |
| Taiwan | [Source](https://portal.opentopography.org/lidarDataset?opentopoID=OTLAS.062023.32651.1) | UAV photogrammetric point cloud; Excluded from ALS coverage; transport sample passed | TW18_Carr is UAV structure-from-motion, not airborne laser scanning. Previous decoded sample remains valid only as a file transport test. |
| USA | [Source](https://planetarycomputer.microsoft.com/dataset/3dep-lidar-copc) | ALS COPC; Previously decoded sample; live search rechecked | USGS sample decoded 2026-09-16; live AOI query rechecked 2026-09-17. Adapter uses Planetary Computer signing; national completeness not asserted. |
| USA | [Source](https://data.neonscience.org/data-products/DP1.30003.001) | Discrete-return ALS LAZ; Official product verified; token required | NEON tutorial confirms download tokens required since June 2026. Product differs from waveform and raster data. Direct website TLS check failed; authenticated download untested. |
| Brazil | [Source](https://doi.org/10.3334/ORNLDAAC/1644) | ALS point clouds; Metadata verified; authenticated file access untested | Brazil forest research sites 2008-2018; Earthdata collection resolves. Guide URL redirects to PDF; full authenticated download untested. |
| Brazil | [Source](https://zenodo.org/records/7636454) | ALS point clouds in ZIP; Metadata and file listing verified | EBA transects; record credits Ometto et al.; CC BY 4.0. Large archives listed; full download not tested. |
| France | [Source](https://cartes.gouv.fr/rechercher-une-donnee/dataset/IGNF_NUAGES-DE-POINTS-LIDAR-HD) | ALS COPC; Current catalog redirect verified | Old IGN link redirects to cartes.gouv.fr. Product-specific catalog retained; file download and AOI adapter untested. |
| Spain | [Source](https://pnoa.ign.es/pnoa-lidar/productos-a-descarga) | ALS point clouds and derived models; Official product downloads page verified | Select point clouds through CNIG and distinguish coverage campaigns, availability and attribution; native adapter and file download untested. |
| Norway | [Source](https://www.kartverket.no/api-og-data/terrengdata) | ALS LAZ/ZLAS and other elevation data; Official access workflow verified | Kartverket documents original point clouds and Høydedata downloads/API; some partner datasets need login. Select airborne laser surveys explicitly. |
| Finland | [Source](https://www.maanmittauslaitos.fi/en/maps-and-spatial-data/datasets-and-interfaces/product-descriptions/laser-scanning-data) | ALS point clouds; Official product verified; final HTTP check passed | Open 0.5 p product is distinct from 5 p licensed data. Official indexed metadata read; initial request failed certificate-chain validation; final verified-TLS request returned HTTP 200. File access untested. |
| Central Africa (DRC, Congo, Gabon, Cameroon, Eq. Guinea, CAR, Nigeria, S. Sudan, Uganda, Rwanda, Burundi, Tanzania, Angola) | [Source](https://doi.org/10.3334/ORNLDAAC/2481) | GEDI/ALS-derived metrics and maps; Excluded from raw ALS download sources | Published formats are GeoTIFF, CSV and KML. ALS reference metrics are not evidence of LAS/LAZ availability in each country within the regional map extent. |

## Direct file and directory evidence

* [Canadian public object listing](https://canelevation-lidar-point-clouds.s3.ca-central-1.amazonaws.com/?list-type=2&max-keys=10): the checked asset is a COPC LAZ, LAS 1.4 point format 6. Only the first 65,536 bytes were read; this is not a full point-stream decode.
* [Swiss STAC item](https://data.geo.admin.ch/api/stac/v0.9/collections/ch.swisstopo.swisssurface3d/items/swisssurface3d_2015_2494-1140): the complete 11,225-byte ZIP matches its STAC SHA-256. `lidR::readLAS()` decoded 930 points. This small edge tile establishes file access, not representative national quality.
* [Paracou LAZ directory](https://data.ceda.ac.uk/neodc/forestscan/data/french_guiana/paracou/ALS-Paracou-2019/paracou): public file links are listed; full files were not decoded.
* [Panama BCI directory](https://smithsonian.dataone.org/datasets/ALS_Panama_2023/03_Barro_Colorado_Island/): classified and unclassified LAZ folders are listed. [DataCite](https://api.datacite.org/dois/10.60635/C3F593) confirms the supplied DOI's title and 2023 survey identity.
* [Lopé ForestScan directory](https://data.ceda.ac.uk/neodc/forestscan/data/gabon/lope): inspected folders are labelled TLS and UAV-LS; a collection-level ALS label cannot be applied automatically.
* [Malaysia SEP-11 directory](https://data.ceda.ac.uk/neodc/forestscan/data/malaysia/SEP-11): scan-project content is listed. The supplied Danum Valley / SAFE extension was not established by this collection DOI.
* [Scottish bucket](https://srsp-open-data.s3.eu-west-2.amazonaws.com/?list-type=2&prefix=lidar/&delimiter=/&max-keys=30): public campaign listing verified; no assumption that every campaign has the same license.

Additional official evidence: [NEON token requirements](https://www.neonscience.org/resources/learning-hub/tutorials/neon-discrete-point-clouds),
[Poland file lookup and indexes](https://www.geoportal.gov.pl/en/data/lidar-measurements-lidar/),
[AHN native grids](https://www.ahn.nl/dataroom),
[Estonian LAZ download examples](https://geoportaal.maaamet.ee/est/abi-ja-juhised/andmed),
[GlobALS publication metadata](https://api.crossref.org/works/10.3390/rs12111877),
[ForestScan publication](https://essd.copernicus.org/articles/18/1243/2026/).

## Recheck and integration gate

Run `python tools/check-source-links.py` from the checkout to refresh HTTP evidence. This explicit network utility only checks pages; it neither downloads full datasets nor marks providers implemented.
For existing adapters, use `tools/validate-samples.R` as described in [VALIDATION.md](VALIDATION.md).
Link checks are a dated snapshot, not a guarantee of future availability.

Before adding a source adapter: verify acquisition method, native geometry/CRS, acquisition dates, stable file discovery,
dataset-specific terms/citations and at least one representative decoded sample. Credentials and provider quotas need a separate workflow.
All remaining untested or restricted access is recorded above; this review does not claim universal country-by-country download validation.

# Additional live European access checks

Run on 17 September 2026 local time (some records are 18 September UTC).
Exact URLs, HTTP status, response sizes, partial-content hashes and sample
geometries are in [public-access-checks.json](public-access-checks.json).
Requests used no account, API key, TLS bypass or identity-form bypass.
Only bounded headers/ranges and metadata were read, not full point clouds.

| Source | Live result | Remaining work |
|---|---|---|
| France — IGN LiDAR HD | Three official COPC files returned HTTP 206 and 65,536 bytes with LASF, LAS 1.4 format 6. Official Atom listing resolves a filename and size. A public INRAE/TETIS STAC AOI query returns an actual polygon and official IGN asset; that exact asset's header also passed. | No full point decoding or full-file checksum. In-app connector not implemented. Spatial catalogue is a research service, not IGN's own WFS. |
| Spain — PNOA third coverage | Official tile detail and polygon queries returned HTTP 200. Product-specific CC BY 4.0-compatible attribution displayed. The standard anonymous download-init request returned HTTP 403. | File transfer remains unvalidated. Stop at 403; do not infer its cause, try private endpoints or bypass it. Official portal remains the route for user access. |
| Luxembourg — ACT LiDAR 2019 | Official GeoJSON ZIP downloaded and parsed: 10,908 polygon features with LAZ and parent ZIP filenames. Nine features match the independently published sample ZIP. That ZIP returned HTTP 206, 65,536 bytes and PK signature. | ZIP contents/point stream not decoded; no full-file checksum. No in-app connector or active-catalogue addition yet. |
| Belgium — Wallonia | Official ArcGIS service and layer returned HTTP 200. A one-feature query returned a Polygon with LAZ basename, point count, first/last flight timestamps, CRS and file size. | No download URL in this feature; point-file transport not yet verified. No in-app connector or active-catalogue addition. |

## France

[IGN's official dataset record](https://www.data.gouv.fr/datasets/nuages-de-points-lidar-hd)
identifies Licence Ouverte 2.0. The official
[download API documentation](https://cartes.gouv.fr/aide/fr/guides-utilisateur/utiliser-les-services-de-la-geoplateforme/telechargement/)
supports resource/file listing and downloads, with pagination (maximum 50
entries/page) and 10 requests/second. The sample download response also reports
a stricter one-request/second header; these probes were sequential and spaced
by at least 1.2 seconds. A future connector must respect applicable limits.

The advertised `IGNF_NUAGES-DE-POINTS-LIDAR-HD:dalle` WFS query returned 400,
and the complete current capabilities response did not list that layer. We did
not use the private API-key endpoint mentioned by older examples.

Instead, the [INRAE tutorial](https://lidar.pages-forge.inrae.fr/lidarHD/articles/lidarHD.html)
documents a public research STAC catalogue. Its collection identifies
UMR TETIS / INRAE as contact, IGN as producer and `etalab-2.0` as licence.
A small AOI near 5.72 E, 45.11 N returned
`LHD_FXX_0913_6450_PTS_LAMB93_IGN69_PM`, with a polygon and an official
`data.geopf.fr` COPC URL. That URL returned a valid LAS header anonymously.
Generic STAC dates and edition dates are not treated as acquisition dates;
the catalogue also supplies explicitly named acquisition fields.

## Spain

The checked [CNIG tile](https://centrodedescargas.cnig.es/CentroDescargas/detalleArchivo?sec=11970508)
is `PNOA_2023_EXT_624-4393_H29_NPC02.laz`. The returned geometry is a small
boundary fragment, so its filename is not used to invent a full square.
The public page's own download handler calls `initDescargaDir`; this call
returned 403 even after loading the detail page in a normal anonymous cookie
session. No subsequent file request or restricted workaround was attempted.
This failure does not establish that the dataset is legally restricted or
unavailable through the normal browser portal.

[CNIG's manual](https://centrodedescargas.cnig.es/CentroDescargas/txtInfoDesc/ManualDescargaAutomatica_ES.pdf)
documents a maximum of 20 products per unregistered download. No bulk selection
was attempted and no request splitting was used.

## Luxembourg and Wallonia

[ACT's official record](https://data.public.lu/en/datasets/lidar-2019-releve-3d-du-territoire-luxembourgeois/)
identifies CC0 and describes 500 m clouds grouped into ZIPs of up to nine tiles.
The sampled parent ZIP is `LIDAR2019_NdP_C9_R7_LL62000_67000_EPSG2169.zip`.
Its public resource URL was used directly; the optional API-key bulk-download
script was not used. Range hash is not the checksum of the complete ZIP.

[SPW's official index record](https://geoportail.wallonie.be/catalogue/7341def8-8ea4-4031-9b2a-4a31f1954d05.html)
documents CC BY 4.0. The sampled feature is
`LIDAR_2021_2022_500mN5335E7180`. Polygon and acquisition fields are verified
at the metadata level only. A successful index query alone does not establish
downloadable point-cloud access.

# Native European tile integration

See [the Europe-wide access review](EUROPE_ACCESS_REVIEW.md) for the broader
country screening and the anonymous-access-only admission policy.

17 September 2026. Integrate provider tiles and boundaries as published;
do not generate a replacement grid or infer acquisition dates from filenames.

| Source | Result |
|---|---|
| Netherlands, AHN6 | Implemented native OGC Features AOI search, pagination, exact polygon intersection and original LAZ downloads. Other AHN generations remain outside this adapter. |
| Switzerland, swissSURFACE3D | Implemented native STAC AOI search and original LAS ZIP or LAS/LAZ asset downloads. ZIPs remain intact; extract locally for point preview. |
| Germany, Saxony | [Official 2 km tiled LSC LAZ delivery](https://www.geodaten.sachsen.de/downloadbereich-digitale-hoehenmodelle-4851.html) identified. ZIP/index connector pending; not a nationwide German endpoint. |
| Finland | [File service](https://www.maanmittauslaitos.fi/en/e-services/open-data-file-download-service/open-data-file-updating-service-interface) publishes LAZ links and pagination but requires a personal API key. An authenticated connector is outside the current anonymous-access scope. Any alternative public route needs separate verification. Only the open product is in scope. |
| Austria, Tirol | [Official source](https://www.tirol.gv.at/sicherheit/geoinformation/geodaten-tiris/laserscandaten) offers free derived elevation products but lists classified LAS/LAZ point clouds as a request with a processing charge. Not added as anonymous open-cloud access. This does not establish availability in other Austrian states. |
| Italy, Tuscany | [Regional LiDAR catalogue](https://dati.toscana.it/dataset/lidar) is a lead; products and ownership vary. Original open cloud-to-tile access is not yet verified, so no national Italian connector is claimed. |

## Verified endpoint chain

AHN's [official dataroom](https://www.ahn.nl/dataroom) links to the AHN6
OGC index at `api.ellipsis-drive.com`. Collection
`6aec07f5-f7eb-4f51-b6f7-aee45e5767bd`, under dataset
`0820faae-5240-499b-8486-cf406433cf71`, returns geometry and `Puntenwolk`
URLs. The adapter chooses only this cloud property, excluding DTM/DSM URLs.
It handles the provider's terminal empty page and checks URL prefixes.

Swiss [STAC documentation](https://docs.geo.admin.ch/download-data/stac-api/overview.html)
describes the official file catalogue. The adapter uses collection
`ch.swisstopo.swisssurface3d` under `/api/stac/v1/`, filters actual AOI
intersection and keeps original filenames/URLs. A downloaded LAS ZIP is
checked for expected archive entries and transport size, then checksummed;
this is not full decompression/point validation. No archive is extracted by
the downloader.

Both adapters leave acquisition dates unknown when the returned catalogue
does not explicitly establish collection dates. Generic STAC `datetime`,
creation/publication timestamps and filename years are not substituted.

## Live checks

- AHN6: a small AOI near 6.55 E, 53.21 N returned the native
  `AHN6_2025_C_232000_581000.LAZ` tile. HTTP 206 returned 1,024 bytes with
  a LASF header. Full file transfer/point decoding was not performed.
- Switzerland: an AOI near 7.44 E, 46.94 N returned
  `swisssurface3d_2023_2600-1198_2056_5728.las.zip`. HTTP 206 returned
  1,024 bytes with a ZIP header. Separately, the previously identified small
  historical tile `swisssurface3d_2015_2494-1140_2056_5728.las.zip` was
  downloaded fully (11,225 bytes) through the new downloader validation.
- The live tests exposed and fixed an HTTP helper bug that removed embedded
  URL query parameters. Spatial filters and pagination queries are now preserved.
- Offline tests cover query preservation, exact intersections, cloud-asset
  selection, unknown dates, pagination limits/host checks and ZIP handling.

Choose **Netherlands - AHN6** or **Switzerland - swissSURFACE3D** in Shiny,
or call `find_tiles(aoi, provider="ahn6")` / `provider="swisstopo"`.
Known hosted transfer limits continue to apply; unknown-size files may need
local download. These checks do not prove availability of every tile, and
the UI has not received a new browser smoke test for these additions.

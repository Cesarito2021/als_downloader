# Regional verification — 19 September 2026

Small-area samples were checked with the actual provider adapters, original-file
downloads and `read_preview()`. These are sample checks, not certification of all
tiles, acquisition dates, classification quality or national coverage.

| Source / sample | AOI search | Original file and sampled points |
| --- | --- | --- |
| Brazil — São Paulo / BR17_SaoPaulo | 1 tile | LAS/LAZ decoded: 19,965 points |
| New Zealand — Auckland_2013 | 1 tile | LAS/LAZ decoded: 19,914 points |
| Netherlands — AHN6, Groningen | 1 tile | LAZ decoded: 19,997 points |
| France — IGN LiDAR HD, Paris | 1 tile | COPC LAZ decoded: 19,987 points |
| United States — USGS 3DEP test AOI | 7 tiles | COPC LAZ decoded: 19,798 points from one tile |
| Canada — Athabasca_2018 | 1 tile | COPC LAZ decoded: 19,990 points |
| Switzerland — swissSURFACE3D, Bern | 1 ZIP tile | ZIP access verified; a separate small archived Swiss ZIP was downloaded, extracted and decoded (930 points) |

The browser verification draws small rectangles, performs the application search
and renders samples decoded above. Rendering reuses those samples; it does not
repeat the original download through every Plot button. The Swiss render uses
the separate archived sample, not the Bern ZIP. A subsequent ZIP visualization
check passed through the app's Plot button: the original archived Swiss ZIP was
downloaded and extracted automatically, and sampled points were displayed. A
100% reader check decoded all 930 source points and verified temporary cleanup.
ZIP limits are 1 GiB downloaded and 2 GiB uncompressed; multiple-cloud ZIPs require
explicit member selection. This applies to 3D view, not remote comparison.

## Coverage shown on the map

Countries are neutral reference outlines. Regional overview polygons are derived
only from configured indexes (including approved contributions); they are dissolved
and generalized for navigation. Search uses the original tile geometries. Empty
areas on the overview do not establish absence of data: live providers may have
additional coverage revealed by a search. No national polygon substitutes for a
regional footprint.

The local review instance uses São Paulo and Auckland OpenTopography archives
and 76 official tiles from Canada's Athabasca_2018 project. These external indexes
are not bundled into the R package. Another deployment must configure its indexes.
No new Zenodo contribution was approved or published during this verification.

## Canadian index and geometry repair

The [official CanElevation record](https://open.canada.ca/data/en/dataset/7069387e-9986-4297-9f55-0288e9676947)
links the [NRCan LiDAR service](https://maps-cartes.services.geo.ca/server_serveur/rest/services/NRCan/lidar_point_cloud_canelevation_en/MapServer).
Layer 1 provides tile polygons, project identifiers and original download URLs.
The regional query used `project = 'Athabasca_2018'`, EPSG:4326, and returned 76
features without a transfer-limit flag. The app still uses a configured local
index; automatic discovery through that service is not implemented here.

One official polygon retained a crossing after spherical validity repair. The
adapter now repairs remaining invalid rings in EPSG:3347 before spatial queries.
A minimal geometry regression fixture records this case. Source: Natural Resources
Canada / Government of Alberta; [Open Government Licence – Canada](https://open.canada.ca/en/open-government-licence-canada).

## Controls

`Clear study area` removes the drawn area and search results. `Reset workspace`
also stops the download and restores download settings. `Cancel download` stops
only the active transfer. Both area resets clear Leaflet's drawing layer; indexed
survey outlines remain as reference data. Report and transfer panels use subdued
translucent purple styling.

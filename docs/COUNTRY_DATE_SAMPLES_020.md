# Acquisition-date samples for 0.2.0

Reviewed 22 September 2026. Two distinct point-cloud assets were queried per
integrated national provider, excluding USGS and OpenTopography as requested.
This is a sample audit of date provenance, not complete national validation or
a point-by-point verification of the eight clouds. Public test AOIs were used.

| Country | Asset / project | Acquisition evidence | Final year |
|---|---|---|---|
| Netherlands | AHN6_2025_C_232000_581000 | Intersecting official flight strips: 2025-03-07 | 2025 |
| Netherlands | AHN6_2025_C_234000_582000 | Intersecting official flight strips: 2025-03-07 | 2025 |
| France | LHD_FXX_0651_6862_PTS_LAMB93_IGN69 | Acquisition fields in the TETIS/INRAE index: 2023-03-03 | 2023 |
| France | LHD_FXX_0653_6863_PTS_LAMB93_IGN69 | Acquisition fields in the TETIS/INRAE index: 2023-03-03 | 2023 |
| Switzerland | swisssurface3d_2023_2600-1198 | Official swisstopo metadata gpstime_min/max: 2023/2023 | 2023 |
| Switzerland | swisssurface3d_2023_2601-1199 | Official swisstopo metadata gpstime_min/max: 2023/2023 | 2023 |
| Canada | pc_083I11NE41NE_20181006, Athabasca_2018 | Official project interval: 2018-10-06 to 2018-10-10 | 2018 |
| Canada | 1km175810478302017LLAKEERIE, Lake_Erie_Fall_2017 | Official project interval: 2017-10-20 to 2017-12-06 | 2017 |

## Sources and interpretation

- AHN: [official flight-strip GeoPackage](https://basisdata.nl/hwh-ahn/AUX/omhullen/AHN6_2025_omhullen_clip.gpkg).
  The spatial intersection establishes the dates of flight strips touching the
  tile, not per-point timestamps for the entire cloud. An exhausted Ellipsis
  page can link back to page one; this provider-specific sentinel is handled
  explicitly and tested. Generic STAC pagination remains strict.
- France: [TETIS/INRAE LiDAR HD index](https://api.stac.teledetection.fr/collections/lidarhd).
  These are catalogue-reported acquisition fields, not independently verified
  flight logs. The original IGN asset directories give edition **2025-06-06**;
  that edition is not substituted for the acquisition year **2023**.
- Switzerland: [official LiDAR metadata layer](https://map.geo.admin.ch/?layers=ch.swisstopo.swisssurface3d.metadata).
  Matching tile identifiers and version years are required. Only year precision
  is reported here; no exact flight days are invented.
- Canada: [CanElevation metadata geodatabase](https://canelevation-lidar-point-clouds.s3.ca-central-1.amazonaws.com/pointclouds_nuagespoints/Metadata_PointCloud_NRCAN.gdb.zip),
  linked by the [official product record](https://open.canada.ca/data/en/dataset/7069387e-9986-4297-9f55-0288e9676947).
  The tile service lacks dates, but the separate metadata has
  `TEMPORAL_EXTENT_DATE_MIN/MAX`. The new reader matches province/project to
  `ID`, retaining project scope. `MD_DATE` and `PRS_DATE_TIME` are not used as
  acquisition dates. The Athabasca filename suggests one day; the documented
  project interval is retained rather than mislabelled as a tile's exact day.

The reproducible audit script, returned records and raw Canadian geodatabase
are retained in the local review directory. A CSV with all eight full asset
URLs, years, intervals and evidence accompanies the release review.

No changes are made to USGS/OpenTopography date rules by this sample audit.
Planetary is excluded from automatic app discovery; it remains explicit opt-in
through the R API. A portal link update never enables a new download adapter.

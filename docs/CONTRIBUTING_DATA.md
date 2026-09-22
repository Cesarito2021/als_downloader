# Contribute ALS data

Keep clouds in a stable public scientific or institutional repository. ALS
Downloader stores approved metadata, coverage and download links, without
transferring ownership or rehosting the original point clouds.

## Zenodo dataset

In **Contribute ALS data → Zenodo dataset**, provide:

1. Published record link or Zenodo DOI; click **Read Zenodo metadata**.
2. Coverage polygons, selected from the record or uploaded, and the matching
   LAS/LAZ or ZIP file. Distinct footprints for different files need a `file_key`
   column with exact Zenodo filenames.
3. Acquisition year or interval; leave blank if unknown.
4. Aircraft/helicopter ALS or UAV LiDAR platform.
5. Optional private contact email.

Authors, title, DOI, supported licence and file sizes come from Zenodo. Publication
is not acquisition. Coverage accepts GeoJSON, single-layer GeoPackage or a ZIP
containing one Shapefile with SHP, SHX, DBF and PRJ; maximum 5 MiB, 10,000 polygons
and 100,000 vertices. Use a known CRS. Study-area polygons do not prove continuous
point coverage.

Without polygons, contributors may declare an approximate square by centre and
distance to each side (1–10,000 metres). It retains its approximate label and
requires explicit file selection and maintainer review.

**Validate submission** checks structure and mapping. **Submit for maintainer
review** stores a pending proposal on configured instances; otherwise save the
proposal JSON and share it privately. The ALSdownloadeR team reviews submissions;
publication requires explicit approval. No review deadline is promised.

Approved coverage appears automatically in Explorer and its files become available
through AOI search. Notifications require administrator SMTP configuration.
See [the real ALS/Shapefile test](ZENODO_LIVE_CHECKS.md) and
[contact-data handling](CONTRIBUTOR_PRIVACY.md).

## Index for multiple files

The [GeoJSON example](../inst/extdata/contribution-template.geojson) is fictional.
Replace its geometry and values. Each EPSG:4326 Polygon/MultiPolygon identifies
one asset through `tile_id`, `dataset`, `url`, `platform`, `license_url`, `citation`,
`acquired_start` and `acquired_end` (nullable). `size_bytes` is optional.
Use stable HTTPS LAS/LAZ links; Zenodo ZIP links refer to the complete archive.
Indexes are limited to 5 MiB and 10,000 rows.

An approved index can be installed as `source.tiles.geojson` in a configured
index directory and queried with `find_tiles(..., provider="contributed")`.

Downloads preserve originals. Spatial search does not make large archives smaller,
stream arbitrary ZIP members or guarantee CRS compatibility. Hosted preview limits
still apply. Retain producer credits and licence links with downloads and figures.

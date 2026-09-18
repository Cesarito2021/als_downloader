# Contribute a searchable aerial LiDAR source

Keep the LAS/LAZ files in your own public repository. ALS Downloader stores
metadata and links, not your point clouds. Submit a small GeoJSON tile index
so a researcher can draw or upload an AOI, inspect intersecting tiles and
download the selected original files. 3D inspection is optional.

Active catalogue inclusion requires verified polygon coverage or a tile
index. A direct cloud URL or GPS point locations alone are insufficient.
Providers may also supply GeoPackage or a zipped polygon Shapefile for
maintainer review and conversion to the common index format.

Download the template in **Submit your dataset**, or use
[the example GeoJSON](../inst/extdata/contribution-template.geojson).
It contains fictional geometry and URLs: replace every example value and
footprint before submitting. It is not an active source.

## Index contract

Use a GeoJSON FeatureCollection in EPSG:4326 (longitude, latitude), with one
Polygon or MultiPolygon per tile. Each index may contain up to 10,000 tiles
and be at most 5 MiB. Split larger indexes by campaign or region.

| Property | Value |
|---|---|
| `tile_id` | Unique tile identifier within the dataset. |
| `dataset` | Dataset or campaign name. |
| `url` | Stable public HTTPS URL ending in `.las` or `.laz`, without credentials or temporary query tokens. |
| `acquired_start`, `acquired_end` | Provider collection dates as `YYYY-MM-DD`, or `null` when unknown. Never substitute publication dates. |
| `platform` | `ALS` or `UAV-LiDAR`. |
| `license_url` | Public HTTPS link to the applicable open-data license. |
| `citation` | Dataset attribution, including its DOI. |
| `size_bytes` | Optional positive file size, or `null`. |

The footprint must describe that file's coverage, not the whole country.
Collection dates and citations may be repeated for tiles from the same
campaign. No point-cloud upload, conversion or visualization is required
for the compatibility check.

## Check and approval

Host the index at a direct public HTTPS URL ending in `.geojson`. The automatic
check requires an HTTP 200 response and a declared size. It reads at most
5 MiB of index metadata and validates the fields and polygons. It does not
download the referenced LAS/LAZ files or confirm their contents, dates or
license. A direct LAS/LAZ submission receives a header-only connection check.
Redirecting portals and access requiring an account need manual discussion.

Complete the ten-field form, run **Check compatibility**, then open and send
the email draft. Technical completion at 100% is not publication approval.
Cesar reviews access, attribution and coverage before installing an index.
The contact email stays out of the public index.

After approval, the maintainer saves the index as `campaign.tiles.geojson`
in the configured index directory. No provider-specific code is needed for
sources following this contract:

```r
tiles <- alsdownloader::find_tiles(
  "study-area.gpkg", provider = "contributed",
  tile_index_dir = "approved-indexes"
)
alsdownloader::download_tiles(tiles, "als-data", workers = 1L)
```

In Shiny choose **Approved contributed indexes** and configure the same
directory when running locally; the server administrator configures hosted
indexes. Installing an index enables AOI tile search; it does not automatically
add a new country or source card to the global catalogue.

## Large files

Downloads preserve complete original tiles, including portions outside the
AOI. An index cannot make a multi-gigabyte file smaller. Providers should
offer reasonably sized spatial tiles when possible; a single huge file or
ZIP archive requires a different access arrangement. This adapter does not
implement partial COPC/EPT reads or server-side clipping. Existing hosted
transfer and preview limits still apply. Export the selected metadata and R
script to download larger selections locally. Completed checksum-verified
files can be reused on rerun; interrupted file transfers restart.

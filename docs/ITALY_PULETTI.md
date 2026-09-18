# Italy — Nicola Puletti / CREA datasets on Zenodo

Historical access review. Sila and Tree-LiMS were subsequently removed from
the active catalogue because polygon coverage/tile indexes were not verified.
The GPS Shapefile in Sila contains points, not boundaries. The selection
language below describes the earlier review, not current app availability.
See the [coverage audit](COVERAGE_AUDIT.md).

Verified **17 September 2026**. The primary ALS entry for Italy is
[Sila National Park – 3D Point cloud data](https://zenodo.org/records/3633629),
by **Nicola Puletti (CREA)**, DOI **10.5281/zenodo.3633629**, licensed **CC BY 4.0**.
It represents a study area in Sila, Calabria, not national Italian coverage.

## Select the correct acquisition

| Files | Acquisition / processing | Use in this catalog |
|---|---|---|
| `merged.las` | Original ALS, collected in late July 2019 | Primary airborne dataset; 6,822,549,289 bytes. |
| `myLas_norm_lt22.las` | Normalized ALS, truncated at 22 m above ground | Processed subset; not equivalent to the complete vertical structure. |
| `ID_*.las` | Mobile terrestrial scanning, August 2019; normalized plots | **Excluded: terrestrial acquisition.** |
| `GPSpoints_EPSG_32633.*` | Surveyed plot centers and Shapefile companions | Plot locations, not airborne tile footprints. |

## Actual access check

Zenodo's public API returned the record, license, file URLs and published checksums without authentication.
Two HTTP 206 range requests to `merged.las` read **94,208 bytes** in total.
The LAS 1.2 header declares point format 1; XYZ coordinates were decoded from **1,024 uncompressed point records**.
This verifies bounded access and a point sample, **not a full-file download or full checksum validation**.
The approximately 6.82 GB file was not downloaded in full.

Exact byte ranges, hashes and coordinate extrema: [italy-sila-access.json](italy-sila-access.json).
The source record and related record metadata are summarized in [italy-puletti-records.json](italy-puletti-records.json).
Reproduce the bounded probe with `python tools/check-sila-access.py` from a checkout.

The **Sources and access** tab offers **Connect to Zenodo**, which retrieves live record metadata and exposes only the reviewed original aerial `merged.las` as a direct browser download. The app checks the record, exact file link, size, DOI and license before enabling the link. It does not store or proxy the full file. Connection failure leaves the download link disabled.

Italy appears in the application's country/source catalog with **Implemented = FALSE** for AOI search.
Automatic AOI discovery still requires verified spatial footprints, CRS and a Zenodo adapter.
The GPS Shapefile name alone does not establish the LAS coordinate reference system.

## Related aerial records and pending qualification

| Dataset | Classification | Assessment |
|---|---|---|
| [Multifordiv](https://zenodo.org/records/17227385) | Five LAS files; keywords `lidar`, `uav` | CC BY 4.0. Sparse metadata: acquisition dates, sensor and georeferenced coverage need clarification. |
| [Tree-LiMS, Alpe di Catenaia](https://zenodo.org/records/17492219) | UAV LiDAR and multispectral, single-tree dataset | CC BY 4.0. The concept record `14650937` resolved to version record `17492219`; single-tree LAS ZIP is listed. |

Sila and Tree-LiMS are discovery entries in the [aerial-only Zenodo overview](ZENODO_AERIAL.md). Multifordiv remains outside the eligible catalog pending platform verification. Related files were not downloaded. Terrestrial-only records are excluded; the metadata audit retains them for traceability.

## Dataset citation

Puletti, N. (2020). *Sila National Park – 3D Point cloud data* (Version 1) [Data set]. Zenodo.
[https://doi.org/10.5281/zenodo.3633629](https://doi.org/10.5281/zenodo.3633629).

Credit Nicola Puletti / CREA and the dataset DOI, link the CC BY 4.0 license and identify modifications when reusing the data.
The record acknowledges the AGRIDIGIT Selvicoltura project for data collection.
Dataset attribution is distinct from software authorship or endorsement of ALS Downloader.

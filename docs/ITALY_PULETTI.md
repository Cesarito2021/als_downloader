# Italy — Nicola Puletti / CREA datasets on Zenodo

Verified **17 September 2026**. The primary ALS entry for Italy is
[Sila National Park – 3D Point cloud data](https://zenodo.org/records/3633629),
by **Nicola Puletti (CREA)**, DOI **10.5281/zenodo.3633629**, licensed **CC BY 4.0**.
It represents a study area in Sila, Calabria, not national Italian coverage.

## Select the correct acquisition

| Files | Acquisition / processing | Use in this catalog |
|---|---|---|
| `merged.las` | Original ALS, collected in late July 2019 | Primary airborne dataset; 6,822,549,289 bytes. |
| `myLas_norm_lt22.las` | Normalized ALS, truncated at 22 m above ground | Processed subset; not equivalent to the complete vertical structure. |
| `ID_*.las` | Mobile terrestrial scanning, August 2019; normalized plots | Keep separate from ALS. |
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

Italy now appears in the application's country/source catalog with **Implemented = FALSE**.
Automatic AOI discovery still requires verified spatial footprints, CRS and a Zenodo adapter.
The GPS Shapefile name alone does not establish the LAS coordinate reference system.

## Other records found

| Dataset | Classification | Assessment |
|---|---|---|
| [Multifordiv](https://zenodo.org/records/17227385) | Five LAS files; keywords `lidar`, `uav` | CC BY 4.0. Sparse metadata: acquisition dates, sensor and georeferenced coverage need clarification. |
| [Tree-LiMS, Alpe di Catenaia](https://zenodo.org/records/17492219) | UAV LiDAR and multispectral, single-tree dataset | CC BY 4.0. The concept record `14650937` resolved to version record `17492219`; single-tree LAS ZIP is listed. |
| [Catenaia beech plots](https://zenodo.org/records/4297218) | Terrestrial LiDAR; listed downloads are `.Rdata` | CC BY 4.0. Not an ALS tile source or a direct LAS/LAZ upload workflow. |
| [Adamello Brenta understory](https://zenodo.org/records/5653007) | Mobile terrestrial LiDAR, June 2021 | CC BY 4.0. Ten normalized plot LAZ files; not airborne coverage. |

Only Sila was added as the primary Italian ALS source. Related entries were checked at metadata level; their files were not downloaded.

## Citation and acknowledgement

Puletti, N. (2020). *Sila National Park – 3D Point cloud data* (Version 1) [Data set]. Zenodo.
[https://doi.org/10.5281/zenodo.3633629](https://doi.org/10.5281/zenodo.3633629).

Credit Nicola Puletti / CREA and the dataset DOI, link the CC BY 4.0 license and identify modifications when reusing the data.
The record acknowledges the AGRIDIGIT Selvicoltura project for data collection.
Dataset attribution is distinct from software authorship or endorsement of ALS Downloader.

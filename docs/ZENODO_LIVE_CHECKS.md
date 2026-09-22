# Zenodo integration: live validation

Verified on 19 September 2026. No Zenodo deposit was created. The real maintainer
proposal remains pending; approval was exercised only in a disposable test queue.

## Verified example

[Wang and Kissling: extracted trees and shrubs, Oostvaardersplassen, Netherlands](https://doi.org/10.5281/zenodo.20311343),
version 6, CC BY 4.0. The dataset derives from AHN4 airborne laser scanning.
It contains extracted woody vegetation, not a complete all-return survey.

| Item | Validation |
|---|---|
| Cloud asset | `2_Extracted_trees_shrubs_points.zip`, 71,220,772 bytes; contains `Trees_shrubs_v2.las`. |
| Author boundary | `5_Marsh_area.zip`, 29,360 bytes; polygon Shapefile of the marsh study area. |
| Integrity | Both complete downloads match the MD5 checksums supplied by Zenodo. |
| Spatial check | 86,093 of 86,129 regularly sampled points fall inside the marsh polygon (99.958%). The boundary represents the study area, not an exact point-level occupancy mask. |
| CRS | The depositor's README declares EPSG:28992. The LAS header omits it. Independent spatial verification used that documented CRS; the original file was not modified. |
| Viewer | 49,785 sampled points decoded with the app's reader. ZIP viewing requires full download and temporary extraction. |
| Workflow | Live browser form, polygon/file selection, pending storage, isolated approval, AOI search and automatic red-mask refresh passed. Zoom was retained and a new browser session saw the approved coverage. |

The real proposal selects only the 71 MB archive, not the separate 2.31 GB
vegetation input archive. Acquisition dates remain unknown rather than copying
the publication date. Creator names, DOI, licence and file size are retained.
The depositor's README refers to an older v1 cloud; the actual archive contains
v2, which is the file tested.

## Required information

1. Published Zenodo record link or Zenodo DOI.
2. Coverage polygons and matching cloud files. The form now lets a contributor
   select one archive for a study area without editing the Shapefile. Different
   footprints for different files require a `file_key` column with exact names.
3. Acquisition year/interval, or unknown.
4. ALS or UAV LiDAR platform.
5. Optional private contact email.

Zenodo supplies title, authors, DOI, licence and asset metadata. Coverage accepts
GeoJSON, a single-layer GeoPackage or zipped Shapefile (SHP/SHX/DBF/PRJ), up to
5 MiB. Images and the record page's Export GeoJSON link are not substitutes for
coverage polygons. ZIP file contents and spatial correspondence need review.
Missing cloud CRS does not prevent metadata search or original-file download,
but limits automatic spatial comparison: the app must not guess. Contributors
should embed their CRS and document vertical units and datum.

## Approval and notifications

Approval re-fetches metadata, validates the saved proposal and writes an index
without the private contact. Open Explorer sessions refresh approved coverage
within approximately three seconds; new searches retrieve its files.
The reviewer link opens a proposal; opening it does not approve it.

The notification was prepared but **not sent**: sender SMTP configuration is
missing. Localhost review links work on the computer running the app. Public
hosting needs persistent private storage, restricted reviewer access and a mail
transport. This test does not establish capacity for 100 daily submissions or
approval directly from a phone.

## Earlier exploratory checks

Sabah record 14917551 supplied 124 plot polygons and large 2013/2020 archives.
Metadata and ZIP directory range checks passed, but full decoding and plot-to-file
mapping were not verified. It remains an exploratory fixture. Brazil record
7689693 was not adopted because a ready coverage/file mapping was unavailable.

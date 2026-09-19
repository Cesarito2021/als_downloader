# Airborne LiDAR research datasets on Zenodo

Reviewed **17 September 2026**. This is an initial curated overview, not an exhaustive inventory.
Zenodo hosts deposits from authors and projects; it is not presented here as an official national mapping provider.
Credit and cite the dataset creators and version DOI, rather than treating the repository as the data producer.

## Scope

Include laser point clouds collected from aircraft, helicopters or UAVs. Exclude terrestrial and vehicle-mounted scanning, spaceborne LiDAR and photogrammetric point clouds.
For deposits containing multiple acquisition methods, only the explicitly identified aerial laser files qualify.
A LAS/LAZ extension or a `lidar` keyword alone does not establish an aerial platform.

## Initial selection

This historical discovery list is not a list of submission-ready datasets.
Community submissions require existing coverage polygons linked to the files.
Records lacking verified coverage/file mapping, including the EBA record below,
are excluded from submission examples and automatic catalogue integration.

| Country / study area | Dataset and creators | Aerial acquisition / files | Verification and limitations |
|---|---|---|---|
| Italy · Sila National Park | [Sila — Nicola Puletti / CREA](https://zenodo.org/records/3633629) | July 2019 ALS: `merged.las`; `myLas_norm_lt22.las` is normalized and truncated at 22 m | CC BY 4.0. Public byte-range access and 1,024 point records decoded; no full-file checksum check. Exclude the terrestrial `ID_*.las` files. [Evidence](ITALY_PULETTI.md). |
| Italy · Alpe di Catenaia | [Tree-LiMS — Puletti, Guasti, Innocenti and Botticelli](https://zenodo.org/records/17492219) | UAV LiDAR at single-tree level: `las_singletree_data.zip` (204.2 MB) | CC BY 4.0. Record and file listing checked; archive contents and point decoding pending. Multispectral and trait files are ancillary, not aerial laser clouds. Acquisition dates and spatial footprints still need verification. |
| Brazil · Mato Grosso, Amazonas and Pará | [EBA L1A — Ometto and collaborators](https://zenodo.org/records/7636454) | Aircraft ALS, 2016/2017 and 2017/2018 campaigns; Trimble Harrier 68i aboard a Cessna 206; ZIP archives of transects | CC BY 4.0. Source description and file listing reviewed; full downloads and decoding pending. Study transects do not represent continuous national coverage. |

All three have **no Zenodo AOI-search adapter**. Sila additionally has a live metadata connection and direct browser download link in **Sources and access**, limited to the reviewed original aerial `merged.las`. No full-file proxy, clipping or point-cloud storage is provided. Tree-LiMS and EBA remain discovery links. Metadata review, file access and integration are separate validation stages.
The Sila [access evidence](italy-sila-access.json) and Italian [record metadata](italy-puletti-records.json) retain the exact records inspected.
The original [source audit](SOURCE_AUDIT.md) records EBA findings.

## Pending qualification

[Multifordiv](https://zenodo.org/records/17227385), by Puletti and Chianucci, lists five LAS files and the keywords `lidar` and `uav`.
Keep it outside the eligible selection until acquisition platform, sensor and spatial coverage can be substantiated.
Terrestrial-only deposits identified during the Italian review are excluded; historical audit metadata may retain their records for traceability.

## Before integration

Verify acquisition method, file-level scope, license and citation, acquisition dates, CRS and vertical datum, and georeferenced footprints.
Then test a bounded point sample and the download workflow. Publication year must not substitute for acquisition year in a future map legend.
Country navigation is informational; search will require a drawn/uploaded AOI and a verified mapping from footprints to aerial files.

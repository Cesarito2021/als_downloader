# Coastal tile date check: Hurricane Michael

Checked 17 September 2026 near Mexico Beach, Florida, at longitude -85.418, latitude 29.948.

## Verified post-event aerial tile

The [NOAA / USACE source record](https://www.fisheries.noaa.gov/inport/item/54682) identifies an airborne CZMIL topobathymetric survey acquired **24 October–4 November 2018**. This is laser scanning, including land and seabed points.

The source record's old GEOID12B bulk link returned 404. The current [GEOID18 distribution](https://noaa-nos-coastal-lidar-pds.s3.amazonaws.com/laz/geoid18/8625/index.html) provides a working spatial index and files, in NAD83(2011) / UTM 16N with NAVD88/GEOID18 elevations.

The index intersects the coastal test location in two blocks. We checked **Blk_861**, file `20181024_FEMA_PostMichael_FL_16rfu5214.copc.laz`:

| Evidence | Result |
|---|---|
| Complete file transfer | 45,866,026 bytes; matched HTTP size |
| LAS header point count | 7,983,156 |
| Decoded sample | 159,663 points, one in every 50 |
| Sample GPS date | **24 October 2018**, 13:43:59.638500–15:00:44.116400 in GPS calendar time |
| File creation date | **3 November 2018** (header year 2018, day 307) |
| Sample classes | 1: 120,694; 2: 37,095; 29: 1,874 |

The header sets the adjusted-standard-GPS flag. The check restores the 1 billion-second offset before converting to GPS calendar time, following the [LAS specification](https://www.asprs.org/wp-content/uploads/2019/07/LAS_1_4_r15.pdf). Times shown are not UTC. Sampling does not establish the full flight interval; the local MD5 is a fingerprint, not a comparison against a published checksum. [Machine-readable evidence and exact tile link](michael-tile-check.json).

This check confirms a post-event point sample. It does not measure hurricane damage, establish a matched pre-event survey, or add a NOAA AOI adapter to the app.

## Separate USGS catalog finding

At the same point, the application's Planetary Computer search returned a tile named
`USGS_LPC_FL_Lower_Choctawhatchee_2017_16RFU525135_LAS_2019.copc.laz`, with both catalog dates set to **2019-01-01**.
Neither the project-name year nor this single-day catalog value has been independently verified as its acquisition interval.
Do not label this tile pre-/post-Michael from its filename or catalog timestamp alone. This is a different survey from the NOAA tile above; no date correction was inferred or applied.
[Recorded catalog response](michael-stac-check.json).

The app currently displays provider-reported dates. A syntactically valid interval is not independent acquisition-date verification. Source survey metadata and, where available, correctly interpreted point GPS time must be checked before an event-based comparison.

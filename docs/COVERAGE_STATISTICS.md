# Coverage and storage statistics

Snapshot: **20 September 2026**. These numbers describe catalogue access, not
data owned or hosted by ALS Downloader, unique ground coverage, or a guarantee
that every provider link will remain available indefinitely.

## Verified OpenTopography access

| Measure | Value |
|---|---:|
| Hosted airborne-LiDAR collections with verified tile access | 428 |
| Tile-footprint records | 1,056,156 |
| Distinct public LAS/LAZ objects | 1,056,011 |
| Sum of distinct original-file sizes (bytes) | 42,598,350,440,697 |
| Decimal terabytes (bytes / 10¹²) | 42.598350440697 |
| Binary tebibytes (bytes / 2⁴⁰) | 38.7429740300804 |

One collection footprint can be a multipolygon with many separate parts or holes;
428 does not mean 428 individual polygon rings. A tile record is one indexed
footprint. Shared file references explain the difference between index rows and
distinct file objects. Acquisition years do not define distinct collections.

The calculation includes only `ready` records from the
[access audit](../inst/extdata/opentopography-access-audit.csv). For each collection,
the URL-set fingerprint must match the original object audit and the number of
positive-size objects must match the verified count. Public object keys are then
deduplicated **across collections** before summing bytes. This avoids adding
the same original file twice; distinct surveys over the same area remain distinct
files. Compressed LAZ sizes are counted as stored; these are not decompressed sizes.

All indexed links were matched to public inventory objects and a LAS-header
sample was read per readable collection. Complete downloads and decoding were
tested at selected sites, not for all one million files. The reported storage
is derived from object metadata, not a bulk download measurement. The
non-federated OpenTopography query excludes its 3DEP mirror; 3DEP volumes are
not added to this total.

Reproduce from the retained audit object-size inventories:

```sh
python tools/summarize_ot_coverage.py inst/extdata/opentopography-access-audit.csv /path/to/audit/objects-verified /path/to/statistics
```

[Machine-readable totals](../inst/extdata/coverage-statistics.json) ·
[Collection-level CSV](../inst/extdata/opentopography-collection-statistics.csv) ·
[Verification scope](OPENTOPOGRAPHY_VERIFICATION.md).

## Other provider inventories (kept separate)

The navigation snapshot retrieved on **19 September 2026** used:

| Source | Input footprint records | Interpretation |
|---|---:|---|
| USGS / Hobu EPT inventory | 2,279 | Project extents; the live app searches Microsoft's separate 3DEP COPC collection |
| NRCan CanElevation | 361 | Project extents; individual tiles are queried by AOI |
| AHN6 | 14,189 | Tile-footprint records |
| IGN LiDAR HD | 507,573 | Tile-footprint records |
| swissSURFACE3D | 56,841 | Tile-footprint records |

These numbers describe source-index inputs, not the number of dissolved shapes
drawn by Leaflet, unique acquisitions, or a global verified asset count. They
are not added to the OpenTopography file count. No defensible total file volume
has yet been computed for these sources. Provider updates can change inventories.
See [source endpoints, attribution and processing](../inst/sources/DISCOVERY_COVERAGE.md).

For a paper, report the snapshot date, distinguish collections from tiles and
file objects, cite the software and each source, and retain the statistics JSON
and audit with the exact software version used.

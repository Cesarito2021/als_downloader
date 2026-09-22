# OpenTopography access audit and catalogue refresh

The app searches the packaged hosted airborne-LiDAR catalogue automatically. An
OpenTopography account, a private API key, and a personal TileIndex folder are not
required for the verified public bulk route. Each dataset retains its specific
license, producer attribution and DOI. Where no specific license is supplied,
the source is explicitly OpenTopography's Data and Content terms, not an assumed
Creative Commons license.

The audit CSV in `inst/extdata/opentopography-access-audit.csv` enumerates every
record from the global, non-federated PointCloud catalogue. `ready` means the
index was parsed, every URL matched a positive-sized public object, and an
anonymous range request returned a LAS signature for a sample in that collection.
It does not mean every point stream was downloaded and decompressed. Three hosted indexes reuse entire file sets from differently described records;
those ambiguous references also remain external-only. `external`
and `out_of_scope` records do not supply red download footprints.

The package bundles search bounds, survey locations and external-access extents.
Exact dissolved tile footprints are retained in a separate, versioned release
asset (about 39 MB), downloaded only for detailed map views and verified against
a pinned SHA-256. Failed downloads retain survey-location markers; bounding
rectangles are never displayed as verified red coverage. Original provider tile
polygons determine AOI results, independently of the overview resource. The
resource is cached in the R session temporary directory. Package installation,
loading and global-map startup do not download it.

The hosted catalogue and Community Dataspace are different routes. This adapter
covers audited hosted aircraft/helicopter/UAV laser scans. Photogrammetry and
terrestrial scanning remain outside ALS scope. Community Dataspace records retain
source links and are not claimed as indexed/downloadable by this adapter.
Federated 3DEP is excluded from the OT catalogue request; the separate USGS 3DEP
adapter is unchanged. Historical hosted surveys may still overlap geographically
with 3DEP. Only identical file URLs deduplicate; overlapping surveys are retained.

At world scale, red OT dots identify survey locations, not area coverage. At zoom
9 or closer the map clips the original verified tile union to the current view.
This avoids sending millions of vertices at startup while preserving coverage
gaps. AOI searches always intersect the original tile polygons. Yellow OT polygons
are provider-reported extents with external portal links, not verified file tiles.

## Incremental updates (normal maintenance)

Use `tools/update_ot_catalog.py` for routine updates. The packaged
`opentopography-update-state.json` binds the existing audit to its original
catalogue and file checksums. No personal TileIndex directory or original audit
cache is needed. Identity is the provider's collection ID, not acquisition year:
a newly published 1998 survey is a new collection.

Maintenance commands run from a **GitHub source checkout**, which includes the
update state and uncompressed audit. The installed R package omits maintenance
state and stores the audit losslessly as gzip; its CSV download expands the
original bytes. Full snapshot release assets include all maintenance files.

From the package root, inspect the changes first (this fetches only metadata):

```sh
python tools/update_ot_catalog.py --baseline inst/extdata --output snapshots/preview --plan-only
```

Create a verified version in a **different, new directory**:

```sh
python tools/update_ot_catalog.py --baseline inst/extdata --output snapshots/2026-09-21 --rscript Rscript
```

When changes need merging, a compact package baseline first retrieves its pinned
full geometry resource once; this restores existing coordinates without revisiting
old tile indexes or object inventories. Full snapshot baselines need no such
request. An unchanged catalogue is copied directly, without R or tile requests.

The command queries the non-federated catalogue, compares content fingerprints,
and processes only new, changed, reappearing or explicitly selected IDs. It checks
the collection platform before requesting tile indexes: confirmed non-airborne
collections retain an exclusion reason without tile/object processing. Unknown
platforms or failed metadata requests block completion. Community Dataspace
remains external-only; polygon/bounding-box variants of an OTDS ID are compared
together, independent of response order. Their full metadata remains in the
snapshot's `pointcloud-catalog.json`.

Each selected ALS collection uses the same index, geometry, object inventory,
LAS-header, citation and license checks as the full audit. Alias detection also
compares against prior verified collections. A confirmed missing public index
(HTTP 404) stays external-only. Network errors, invalid geometries, missing file
links or uncertain metadata stop the candidate; they never replace the baseline.

Unchanged records keep their exact footprints, review dates, decisions and
exclusion reasons. The merge does not re-read their ZIPs, rebuild their unions,
or enumerate their objects. Removed provider IDs stop the run for review. After
review, `--allow-missing` retains their old records and geometry, marks them
missing, and moves previously ready records to external-only. Reappearing IDs
must pass verification again. Baseline directories are never edited. Every run
requires a new output directory; failed work cannot become an implicit cache.

Only a completed candidate has `complete.json`, written last with checksums.
Its `catalog/` directory becomes the baseline for the next update:

```sh
python tools/update_ot_catalog.py --baseline snapshots/2026-09-21/catalog --output snapshots/2026-09-28
```

For a locally administered or hosted app, set the completed **snapshot root**
before launching and restart the app to activate it:

```r
Sys.setenv(ALS_OT_CATALOG_SNAPSHOT = "/absolute/path/snapshots/2026-09-21")
ALSdownloadeR::launch_app()
```

The app validates the completion marker and checksums before loading the version.
An invalid snapshot keeps the last valid catalogue in memory, or falls back to
the packaged catalogue on startup. Maps, searches and the downloadable access
audit use the same selected snapshot. To roll back, select the previous completed
directory and restart. To use the packaged version, unset the variable. This is
maintainer configuration, not a setting ordinary users must manage.

**Availability is a separate check.** A catalogue fingerprint cannot detect a
file or license-page change that the provider has not reflected in its catalogue.
Periodically rotate explicit rechecks through existing IDs, without a full audit:

```sh
python tools/update_ot_catalog.py --baseline snapshots/2026-09-21/catalog --output snapshots/recheck-01 --recheck AUS11_Victor --recheck CA24_Volcan
```

`--recheck` accepts stable IDs or dataset names; typos fail. Other newly detected
changes are still processed. Runtime index SHA-256 checks remain in force, so a
silently changed ZIP raises a visible error until reviewed. Previous object
checks are historical evidence, not fresh availability claims. Per-collection
verification dates remain in the audit and proof. No scheduler, automatic public
publication, or remote client-update service is enabled by these tools.

Offline preservation tests: `python -m unittest discover -s tools -p test_ot_update.py`.

## Repeat a complete audit (exceptional rebuild)

Use a new audit directory for each date; never reuse a previous date's object
cache as a substitute for checking current availability. Python tools use only
the standard library. R needs sf, jsonlite and digest. Run from the package root:

```sh
python tools/refresh_ot_metadata.py /path/to/audit
Rscript tools/scan_ot_indexes.R /path/to/audit
python tools/audit_ot_objects.py /path/to/audit/tile-links-current.csv /path/to/audit/objects-verified
Rscript tools/build_ot_registry.R /path/to/audit /path/to/full-output
```

For a new full-audit output directory, initialize its incremental state using the
exact catalogue file whose SHA-256 appears in the proof (the output state file
must not already exist):

```sh
python tools/update_ot_catalog.py --baseline /path/to/full-output --catalog /path/to/audit/pointcloud-catalog.json --output /path/to/full-output/opentopography-update-state.json --initialize-state
```

Two bounded network workers, paginated directory listings and resumable page
caches avoid a million individual HEAD requests and exclude unrelated EPT nodes.
`--reuse` is only for resuming verified results from the **same audit run**. Every
reused result must match the entire current set of object keys; changed indexes
cannot inherit an incomplete object check. Review all errors, missing objects,
platform classifications and dataset license changes before accepting the build.
Keep the catalog JSON, index hashes, object summaries and live-test log as release
evidence. The complete audit CSV and compact verification JSON ship with the app;
raw ZIP indexes and point clouds do not.

The runtime fetches only indexes intersecting the AOI and verifies their SHA-256
against the audited snapshot. A changed or unavailable index raises an explicit
error; it never silently returns partial coverage for that provider. Download
failures remain visible as failed transfers. Successful downloads check HTTP
status, response size, LAS/LAZ signature and a local checksum. The live validation
also downloads and decodes selected complete files; it is separate from the full
object-link inventory audit.

Official references:
- [Tile Index access tutorial](https://opentopography.org/node/3598)
- [Terms, Data and Content](https://opentopography.org/usageterms)
- [Dataset citation guidance](https://opentopography.org/citations)

OpenTopography acknowledgment: This work is based on API services provided by the
OpenTopography Facility with support from the National Science Foundation under
NSF Award Numbers 2410799, 2410800 & 2410801.

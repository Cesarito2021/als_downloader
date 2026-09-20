# Incremental update verification — 2026-09-20

The packaged baseline was preserved. Its original metadata catalogue SHA-256
was checked before generating the initial update state; no tile reanalysis was
needed for initialization.

## Live incremental run

- The original catalogue contains 841 records representing 704 unique collection
  IDs. Repeated Community Dataspace geometry variants share their original ID.
- The current metadata comparison detected four changed IDs. Two additional
  hosted collections were explicitly selected for a targeted recheck:
  `AUS11_Victor` and `CA24_Volcan`.
- Six IDs were processed: four hosted ALS collections and two external Community
  Dataspace collections. The other 698 IDs were not sent through tile auditing.
- The four selected hosted indexes contained 3,179 tile rows. Their object
  inventories and anonymous LAS-header samples passed, with zero missing links
  and zero object-audit errors. This is not full decompression of every LAZ.
- The merged registry retained 476 airborne collections: 428 ready and 48
  external. Every column, including the complete coordinate geometry and review
  date, of every unchanged airborne record was compared with the baseline and
  was identical. Row-number representation is not collection identity.
- A second plan against the completed version and the same catalogue selected
  zero changes and 704 unchanged IDs.
- The app loaded the completed version through `ALS_OT_CATALOG_SNAPSHOT`.
  An incomplete version was rejected while the prior valid catalogue remained
  available. The test restored the packaged default afterwards.

## Regression checks

- 13 offline Python tests cover old acquisition years in new sites, metadata
  changes, stable-ID renames, preserved exclusions, missing/reappearing sites,
  explicit rechecks, OTDS geometry variants, corrupt baselines, unknown platforms,
  transient HTTP errors, interrupted updates and prevention of stale-run reuse.
- The R maintenance integration test covers geometry preservation, mixed old/new
  verification dates, retained missing records and an entirely non-ALS delta
  with no tile processing.
- OpenTopography R tests passed, including snapshot checksums and fallback.
- App, overview and coverage-gate regression tests passed. The installed Shiny
  build emits an existing R-version warning; no test failed.

No public release, scheduled update job or remote update distribution service
was activated by this verification. See [maintenance instructions](OPENTOPOGRAPHY_ACCESS.md)
for running another incremental update and selecting a completed snapshot.

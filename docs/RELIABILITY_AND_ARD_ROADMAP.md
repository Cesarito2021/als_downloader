# Reliability and optional local ARD: a realistic roadmap

Follow-up to a much larger "C++ / Rcpp / Python / CLI / keyring" rewrite
proposal from an external session. That proposal is **not adopted**: it
would add compiled code (major CRAN portability risk), Python/CLI
packaging (a second and third release pipeline), and credential/keyring
management (solving a problem this app does not have - every implemented
adapter is anonymous public access by design; see
[EUROPE_ACCESS_REVIEW.md](EUROPE_ACCESS_REVIEW.md)'s admission criteria).
It would also throw away the SSRF/zip-slip/redirect protections already
audited into the current pure-R code (`R/source-preflight.R`, `R/aoi.R`,
`R/download.R`) for an unaudited rewrite.

What follows is the part of that proposal worth keeping, rescoped to pure
R, the existing dependency set (`digest`, `lidR` as `Suggests`, no new hard
dependencies), and CRAN's rules (no network in examples/tests, write only
to a user-specified or temp directory).

## Design principle: reuse the memoization pattern that already exists

`transfer_tile()` in `R/download.R` already does this for downloads: it
writes a `.rds` sidecar record with the file's checksum next to each
downloaded tile, and on a later call, if that record exists and the
checksum still matches, it returns `status = "verified_existing"` without
re-downloading. This is the same idea `lidR::LAScatalog`'s processing
engine uses (`opt_output_files()`): each processed chunk is written to a
real file, and a valid existing file is never recomputed. Every item below
reuses this exact pattern - a sidecar record (inputs' checksums + the
parameters used) next to each output, checked before recomputing anything.

## 1. Real HTTP Range resume in `transfer_tile()`

**Problem:** `fetch_asset()` always starts a fresh `GET` into `.part`. A
connection drop at 90% means the retry re-downloads from byte 0 - the
existing retry/backoff loop is real, but it is not resumable in the literal
sense.

**Fix:** if `.part` already exists and is smaller than the expected
`Content-Length` (from a `HEAD` first), retry with
`httr::add_headers(Range = paste0("bytes=", file.size(part), "-"))` and
append rather than overwrite, only falling back to a fresh download if the
server responds `200` (ignoring the Range) instead of `206`. No new
dependency; `httr::add_headers()` already used elsewhere in the codebase.

## 2. A real pre-flight report, extending what already exists

**Already have:** `selection_summary` in `R/app.R` already sums known
`size_bytes` and separately counts unknown-size tiles before download.

**Add, honestly:**
- Convert the total to a clear "~X GB to download" line (already
  human-readable at MiB scale; extend to GB for large selections).
- Point density: only surface it when a provider's own metadata already
  reports it (e.g. some STAC items expose `pc:count`/`pc:density`
  properties) - store it in a new optional `points_per_m2` column on the
  tile table, `NA` when the provider does not report it. **Never estimate
  or infer a density value ourselves** - that would fabricate precision
  the source does not have, which contradicts this project's whole
  evidence-only approach to every other field (dates, licences, etc.).

## 3. Opportunistic checksum verification

**Already have:** post-download LAS/LAZ header + Swiss ZIP directory
validation, exact `Content-Length` and catalog `size_bytes` checks, and a
cached MD5 (`tools::md5sum`) for the resume-skip check above - this is
already real integrity checking, not merely a status-code check.

**Add:** when a tile row carries a provider-published `checksum` /
`checksum_algorithm` (most providers do not publish one - leave `NA` when
they don't), verify it with `digest::digest(file = path, algo = algorithm)`
(the `digest` package is already an Import) and record a pass/fail in the
manifest. Skipped, not faked, when no checksum is published.

## 4. Optional `as_catalog` return from `download_tiles()`

After a successful transfer, when `as_catalog = TRUE` and
`requireNamespace("lidR", quietly = TRUE)` (same guard pattern already used
in `R/preview.R`), return `lidR::readLAScatalog(output_dir)` alongside the
existing manifest data frame instead of requiring a second manual step.

## 5. Optional local ARD products (DTM / DSM / CHM), cached like everything else

A new function, e.g. `als_products(catalog, products = c("dtm","chm"),
res = 1, output_dir)`, guarded by `requireNamespace("lidR")`:

- Never touches the original downloaded tiles; every product is written
  to `output_dir` the caller chooses.
- DTM via `lidR::rasterize_terrain()`, CHM via
  `normalize_height()` + `rasterize_canopy()`, DSM directly via
  `rasterize_canopy()` on the raw cloud - the same linear, file-to-file
  chain `lidR`'s own catalog engine uses, so each stage's output (the DTM,
  the normalized cloud) is a real file the next stage can read instead of
  recomputing from raw points.
- Before computing a product, write/check a small sidecar record (inputs'
  checksums + product + resolution + algorithm) next to the output file,
  exactly like `transfer_tile()`'s `.rds` record; skip recomputation when
  it already matches.
- Strictly optional and downstream of the raw files, matching the existing
  principle in `docs/CONTRIBUTING_DATA.md`: "keep the point clouds in
  their original repository" - this only ever adds derived products next
  to them, on request.

## Explicitly not doing (from the original proposal)

- No C++/Rcpp core, no Python bindings, no standalone CLI binary, no
  system keyring/credential management. Each would either duplicate
  functionality this pure-R codebase already has and has had audited, or
  solve a problem (authenticated sources) this project has deliberately
  stayed out of.
- No invented point-density numbers where a provider does not publish one.

## Suggested order

1 and 2 are the safest, smallest, and most valuable ("solidez" +
transparency) - no new dependencies, pure extensions of existing tested
functions. 3 and 4 are small, additive, and low-risk. 5 is the biggest
piece (a new function, new tests, new docs) and is entirely optional
value-add; do it last, and only once the app itself is otherwise ready for
its first CRAN submission.

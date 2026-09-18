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

## 6. A downloadable session report (HTML by default, PDF optional)

Inspired by another of the maintainer's apps (CSF-Ind), which renders a
PDF report via `rmarkdown`/`tinytex` after processing a dataset. The idea
is worth adopting here - a "Download session report" button next to
**Download selected tiles** - but not the same implementation, because
that app's source loads `tinytex`, `rnaturalearth`, `tidyverse` and a dozen
other packages unconditionally with bare `library()` calls at the top of
the file, and hardcodes a Windows path (`C:\Users\...\main.R`). Either one
would fail `R CMD check` immediately (undeclared/unconditional heavy
dependencies; a package must never assume a specific machine's filesystem).

**Rescoped design:**
- An R Markdown template shipped at `inst/report/session-report.Rmd`,
  rendered with `rmarkdown::render()` (already a `Suggests` dependency via
  the vignette) - guarded by `requireNamespace("rmarkdown", quietly = TRUE)`,
  same pattern as the `lidR` guard elsewhere.
- **HTML by default** (self-contained, `knitr` already `Suggests`, needs no
  extra runtime install). Offer PDF only when
  `requireNamespace("tinytex", quietly = TRUE) && tinytex::is_tinytex()`
  is true; otherwise render HTML and say why, rather than failing or
  forcing every user to install a multi-hundred-MB LaTeX distribution just
  to use the app.
- Content: search parameters (AOI, provider, date range), tile count and
  the same GB figure from item 2 above, a per-provider breakdown, and the
  citations/licences already written to `CITATIONS.txt` - all data the app
  already has in hand at download time, no new computation.
- Written only to the user-chosen `output_dir` (or via `downloadHandler`
  in the Shiny app, same as the existing `.xlsx`/`.txt` downloads) -
  never to a fixed path.

## 7. What GeoLibre (opengeos/GeoLibre) does that's worth learning from

The maintainer pointed at [opengeos/GeoLibre](https://github.com/opengeos/GeoLibre),
a browser-based geospatial platform, asking whether its point-cloud handling
is worth mimicking. Its actual source
(`packages/map/src/cesium-point-cloud.ts`) confirms most of it already
matches this app's own design: it colours points by native RGB or a
height ramp when there is none (exactly `preview.js`'s LUT), it **refuses
to render a cloud with no resolvable CRS** rather than guessing (the same
"never infer" principle used everywhere else in this project), and it
lazy-loads its heavy point-cloud decoder only once the feature activates
(the same role `requireNamespace("lidR")` already plays here). Nothing to
change on those fronts.

Two things it does differently and better, worth adopting:

- **Real COPC octree-level sampling, not "every Nth point."** Its loader
  walks a COPC file's actual hierarchy breadth-first from the root -
  COPC's own coarse-to-fine level-of-detail structure - stopping at a
  point budget, so a bounded preview is a genuinely representative spatial
  sample. This app's own preview path (`preview_remote_tile()` /
  `lidR::readLAS(..., "-keep_every_nth")` in `R/preview.R` and
  `R/comparison.R`) instead keeps every Nth point in on-disk order, which
  can be spatially biased depending on how the provider wrote the file.
  Since USGS 3DEP and other integrated sources are frequently already
  COPC, reading via COPC's hierarchy pages when the source is COPC (with
  the current decimation kept as the fallback for plain LAS/LAZ) would be
  a real, scoped quality improvement to the existing preview path - not a
  new feature, an upgrade to one already there.
- **`whitebox` (a CRAN package) as an alternative to `lidR` for item 5's
  optional DTM/DSM/CHM products.** GeoLibre's own backend uses
  WhiteboxTools for all of its LiDAR processing. The R wrapper
  (`whitebox::wbt_init()`) downloads a standalone compiled binary once,
  rather than requiring the GDAL/PROJ-linked native compilation `lidR`
  needs - a smaller, more robust dependency footprint for a CRAN package
  to lean on for item 5. Worth trying both and keeping whichever installs
  more reliably across platforms, or supporting either behind the same
  `requireNamespace()` guard.

## 8. The bigger dream: a real 3D coverage/time explorer

The maintainer shared a GeoLibre screenshot of Manhattan buildings
extruded and coloured by construction year, with an automatic legend and
a time slider, and asked whether something in that spirit is realistic
for forest/LiDAR coverage. Short answer: yes, translated correctly, and a
first small piece of it is already done (item above: search-result tile
footprints on the 2D map, coloured by acquisition year with a legend -
same idea as the building screenshot, using `leaflet::colorNumeric()` and
`addLegend()`, both already-imported, no new dependency). The building
screenshot's actual mechanism is not special-cased for buildings at all:
it is real polygons with a real attribute (construction year) driving
colour/height and an automatically generated legend - exactly the tile
footprints this app already carries with their own real attribute
(acquisition date), just not yet rendered that way everywhere.

The full vision - true 3D (not Leaflet's 2D), plus a time slider to scrub
through acquisition years and watch coverage appear - is a real,
buildable target, but a genuinely larger step than anything else in this
document:

- The natural foundation is **`mapgl`**, a CRAN package (not yet verified
  against a live CRAN check from this session - confirm before relying on
  it) that wraps MapLibre GL JS as an htmlwidget and bundles its own JS/CSS
  inside the R package source, the same way `leaflet` does. That matters
  specifically because of a **real blocker in this session**: this
  sandbox can only reach `github.com`, not `unpkg.com`/`jsdelivr.net`/npm,
  so vendoring a third-party JS library file directly into
  `inst/app/www/` is not possible from here. An htmlwidget package whose
  JS assets ship inside the package itself sidesteps that entirely,
  because installing it is a normal CRAN dependency, not a network fetch
  this session has to perform.
- A time slider reads naturally on `acquired_end`/`acquired_start`,
  already real fields on every tile.
- This does not replace the existing Leaflet AOI-drawing map or the
  point-cloud viewer - it would be a new, additional exploration view.

This is not something to start under time pressure or close to a session
limit: it needs its own design pass (which map replaces or supplements
Leaflet, whether `mapgl` genuinely fits a Shiny app cleanly, a real check
that it builds under `R CMD check`) before writing code.

### Checked and ruled out: the experimental R `{cesium}` package

The maintainer also pointed at an R-Cesium option: the
[r-spatial/cesium#3](https://github.com/r-spatial/cesium/issues/3)
discussion about `goergen95/cesium`, an experimental sf-to-CZML converter
for CesiumJS, plus Hobu's Eptium (CesiumJS blog, June 2025) for
streaming EPT/COPC point clouds as native 3D Tiles. Cloned both repos in
this session (GitHub is reachable) rather than judging from the issue
text alone:

- `r-spatial/cesium`'s last commit is from **December 2019** - an
  unrelated placeholder, not the experiment discussed in the issue.
- `goergen95/cesium`'s last commit is from **October 2023** - about three
  years stale, still version `0.1.0`, never published to CRAN. It also
  **vendors 19 MB of CesiumJS inside the package**
  (`inst/htmlwidgets/lib/Cesium/`), which alone would likely draw a CRAN
  size objection (the practical soft ceiling is a few MB). And it never
  handles point clouds at all - grepping the whole source found no
  LAS/LAZ/COPC/point-cloud code; it only converts `sf` points, markers,
  lines, polygons and rasters to CZML with a time dimension.

**Verdict: not a foundation to build on**, for either half of the item-8
dream. It doesn't solve the point-cloud problem, and its abandonment plus
bundled size make it a worse CRAN bet than `mapgl` for the vector/legend/
time-slider half - though this is itself a reason to actually check
`mapgl`'s own bundled size before leaning on it, rather than assuming an
htmlwidget package is automatically small just because it vendors its JS.

Eptium itself (serving EPT/COPC as native Cesium 3D Tiles point clouds,
with real GPU-driven level-of-detail - a materially better result than
the bounded octree-walk-in-JS approach GeoLibre's own COPC path uses) is
real and relevant, but it is server/JS tooling from Hobu (the PDAL/Entwine
team), not an R package. It doesn't fit inside a CRAN package's scope; the
realistic way to offer it is as a documented *external* workflow - "convert
your downloaded tiles to EPT and view them with Eptium/CesiumJS for
museum-grade 3D" - alongside, not inside, `als_downloader`.

### Checked and ruled out: m-schuetz/compute_rasterizer

Also shared: [m-schuetz/compute_rasterizer](https://github.com/m-schuetz/compute_rasterizer),
Markus Schütz's (TU Wien) research code rendering up to two billion
points in real time via OpenGL compute shaders with atomic depth/colour
packing - genuinely impressive, published research (2021/2022 papers).
Cloned and confirmed from its own README: it **requires Windows and an
NVIDIA GPU specifically**, and builds as a native desktop app via a
Visual Studio 2022 solution file. There is no way to run this inside a
browser (WebGL has no compute shaders; this isn't written for WebGPU
either) - it cannot become part of a Shiny app's client-side rendering,
independent of any CRAN concern.

More importantly, it solves a problem this app doesn't have: it targets
rendering an *entire* cloud (billions of points) at full detail. This
app's own point-cloud panels intentionally cap at 50,000 display points
per cloud precisely so a preview loads fast in an ordinary browser - a
deliberate lightweight-preview design, not a full scientific render of
the tile. This technique would only become relevant if the app's purpose
changed from "downloader with a preview" to "full-detail point-cloud
viewer," which is a different product, not a rendering-library swap.

## Suggested order

1 and 2 are the safest, smallest, and most valuable ("solidez" +
transparency) - no new dependencies, pure extensions of existing tested
functions. 3, 4 and 6 are small, additive, and low-risk (6 needs one new
`.Rmd` file but no new hard dependency). 5 is the biggest piece (a new
function, new tests, new docs) and is entirely optional value-add; do it
last, and only once the app itself is otherwise ready for its first CRAN
submission.

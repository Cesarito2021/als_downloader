> Historical working notes follow. For the current release candidate, read [Release review](RELEASE_REVIEW.md) and [submission comments](../cran-comments.md). Statements below about older commits are not evidence for the current revision.

# Release candidate 0.1.0: readiness review

No submission has been made to CRAN. Passing local checks does not guarantee acceptance.

## Package and app

- Full `R CMD check --as-cran`, including the indexed PDF and HTML manuals, finished on Windows / R 4.4.0 with **0 errors, 0 warnings and 2 notes**: new submission; unable to verify the current time. Source build, installation, documentation, examples, vignette and offline tests passed. Missing Courier and makeindex components were repaired in the local TinyTeX environment.
- NEWS formatting and self-namespace worker calls were corrected. The earlier `--as-cran --no-manual` run also finished with 0 errors, 0 warnings and the same two notes.
- [GitHub Actions run 35271083291](https://github.com/Cesarito2021/als_downloader/actions/runs/35271083291), commit `1309077`, passed all four jobs: Windows, macOS and Linux with R release, plus Linux with R-devel (`--as-cran --no-manual`, warnings treated as failures). The macOS setup now installs GDAL so its current `rlas` dependency can compile. This historical run does not cover subsequent package changes.
- Local browser checks passed: required private email, valid DOI, 50-word limit, compatibility-gated submission, stale-result invalidation, text export and mobile width. No message was sent.
- Real USGS discovery returned seven Utah tiles; one source tile decoded to 7,523 preview points. A two-campaign overlap displayed 12,541 and 24,405 points over 0.0024 km2, with independent visibility and original-file selection.
- The globe and search screenshots were refreshed. Comparison camera framing now optionally fits the central 98% of projected display coordinates without deleting points.
- The visual-profile update passed a fresh full local `--as-cran` check (including PDF manual): 0 errors, 0 warnings, the same two notes. Browser tests with real Utah campaigns passed line selection by clicks/drag, shared colour/visibility controls, strip-width changes, three PNG downloads, stale-profile clearing and mobile layout. Offline JavaScript tests cover directional distances, strip/end boundaries and empty sections. Profiles use displayed samples without fitting curves or calculating campaign differences.
- [Visual-profile CI run 35275843652](https://github.com/Cesarito2021/als_downloader/actions/runs/35275843652), commit `763fb27`, passed Windows, macOS, Linux release and Linux R-devel, including the new JavaScript geometry checks. This historical run predates the latest catalogue changes; the published revision requires a fresh matrix.
- The catalogue contains 16 source/service references, not 16 integrated or fully validated datasets. Native live-query adapters cover USGS, AHN6, swissSURFACE3D and IGN LiDAR HD (France); OpenTopography and CanElevation (Canada) require supplied local tile indexes. Approved contributor indexes provide another route. See the installed [source tables, policies and validation evidence](../inst/sources/README.md) for portal-only entries and unresolved permissions.

## Final local verification: 17 September 2026

The full check passed again after the catalogue and licence-gate changes: **0 errors, 0 warnings, 2 notes**, including PDF/HTML manuals, examples, vignette and offline tests. The installed app passed browser smoke checks for the Shiny connection, globe, mobile width, map navigation and empty-selection guard, with no JavaScript errors.

Downloads and remote previews reject missing licence or attribution metadata. This is a completeness check, not automatic legal clearance. Current-R and R-devel results must be checked against the exact published commit in [GitHub Actions](https://github.com/Cesarito2021/als_downloader/actions); the historical runs above do not cover the final catalogue changes.

## Catalogue update: 18 September 2026

Two entries were removed from `inst/extdata/providers.csv` (18 -> 16 rows) because access was not guaranteed: PNOA LiDAR (Spain), whose anonymous download-init check returned HTTP 403, and OpenTopography AUS11_Victor (Australia), whose provider supplies no reuse licence despite a passing technical access sample. See [ACTIVE_SOURCES.md](ACTIVE_SOURCES.md) for the full record. `test-coverage-gate.R` was updated to match. This environment cannot run R, so this change has not been re-verified with a fresh `R CMD check --as-cran` or the GitHub Actions matrix — that re-run is still owed before submission, per item 2 below.

A new in-app adapter (`search_europe(aoi, "ignfr", ...)` in `R/europe.R`, wired through `find_tiles()`) queries IGN LiDAR HD (France) through the public STAC catalogue at `api.stac.teledetection.fr` (UMR TETIS / INRAE) and downloads original COPC LAZ files from `data.geopf.fr`. It is built directly from the live evidence already recorded in `inst/sources/VALIDATION.md` and `inst/sources/public-access-checks.json` (17 September 2026: STAC query and file download both succeeded anonymously). The adapter code itself, including its assumed OGC-API-Features pagination shape, has **not** been exercised against the live service in this session (no outbound network access here beyond GitHub) — mocked unit tests were added (`tests/testthat/test-europe.R`), but a real network run (local or CI) is required before this source is trusted for release. `providers.csv` flags this explicitly in its `access` field for `ignfr`.

A second new in-app adapter (`search_canelevation()` in `R/canada.R`, wired through `find_tiles()`) covers CanElevation (Canada): unlike `ignfr`, no live spatial query API was confirmed, so it follows the existing OpenTopography local-index pattern instead of a bbox search - the user supplies a directory of NRCan's official project/tile `.gpkg`/`.shp` indexes, and downloads use the confirmed public S3 bucket (`canelevation-lidar-point-clouds.s3.ca-central-1.amazonaws.com`, per `docs/file-access-checks.csv`). Unit tests write real temporary GeoPackage fixtures and exercise `find_tiles()` end-to-end (`tests/testthat/test-canada.R`), rather than mocking, since this adapter has no network call to mock in the first place. Still needs a real run against an actual NRCan-downloaded index before release.

## Catalogue update: portal-only EU coverage, 18 September 2026

`providers.csv` grew from 16 to 36 rows (`16 -> 36`). The 20 new rows are all
`implemented=FALSE` "national portal" entries (same pattern as the
pre-existing Norway/Finland/Poland/Estonia/Germany rows): Austria, Belgium
(Wallonia and Flanders as two separate rows sharing country code 56),
Bulgaria, Croatia, Czechia, Denmark, Hungary, Ireland, Italy, Latvia,
Lithuania, Luxembourg, Malta, Portugal, Romania, Slovakia, Slovenia, Spain
(re-added under a new id, `ign_pnoa`, distinct from the removed `pnoa`
adapter id) and Sweden. Each `info_url` is a real official government/agency
page drawn from `docs/EUROPE_ACCESS_REVIEW.md`; none were guessed. This
gives the app's globe/map (`R/app.R`, `inst/app/www/globe.js`) a yellow
marker and a working "open official source" link for every EU member state
that has a genuine official portal on record, even where no anonymous
in-app adapter exists. Cyprus and Greece are the only EU states still
unmapped in the catalogue: their only lead is a non-official Zenodo research
record, not a government page, so no `info_url` was added for either.
`test-coverage-gate.R` was updated to match (`nrow(catalog)` now `36L`, plus
a dedicated EU-country-code coverage test). This environment cannot run R,
so this change has not been re-verified with a fresh `R CMD check --as-cran`.

Added `docs/DATASETS.md`, a by-continent index generated alongside the
existing source tables, and trimmed the README's Source examples table to
its eight most representative entries so the catalogue's growth to 36 rows
does not make the README unreadable.

## Static check: undeclared `stats`/`tools`/`utils` imports, 18 September 2026

Without an R runtime in this environment, `R CMD check` itself cannot be
run, so a manual static pass grepped every `R/*.R` file for `pkg::fun()`
calls and compared the package names against `DESCRIPTION`'s `Imports`.
`stats::setNames`, `tools::file_ext`/`md5sum`, and `utils::read.csv`/
`unzip`/`write.csv`/`URLencode`/`capture.output`/`modifyList` were all in
use but none of `stats`, `tools` or `utils` were declared - a real
`R CMD check --as-cran` flag (`'::' or ':::' import not declared`). Fixed
by adding all three to `Imports`. They are base packages bundled with R
(not on CRAN), so `tools/update_source_docs.py` was also fixed to link
them to the R manual instead of a nonexistent CRAN package page in the
generated `inst/sources/DEPENDENCIES.md`.

Also checked (and found already correct): non-ASCII characters, bare `T`/
`F` literals, `sapply` instead of `vapply`, `1:n`-style loop bugs, every
`@export` tag against `NAMESPACE` and every `man/*.Rd` file, and every
`\link{}`/`[fn()]` cross-reference against an actually-exported function.
`read_comparison_cloud()`'s `lidR::` calls (in `R/comparison.R`) looked
unguarded at first read, unlike every other `lidR::` call site in the
package, but tracing its only caller confirmed it always runs inside
`preview_remote_tile()`, which already checks
`requireNamespace("lidR", quietly = TRUE)` before invoking it - not a bug.
This is still not a substitute for a real `R CMD check --as-cran` run,
which remains owed before submission.

## Comparison UI redesign: 18 September 2026

`R/comparison-app.R`, `inst/app/www/preview.js` and `inst/app/www/profile.js` were rewritten to show campaigns A and B as two synced side-by-side canvases (`als-compare-cloud-a`/`-b`) instead of one canvas with both clouds overlaid; drawing a profile line in either panel now places the same line in both, and the elevation chart below stays a single shared output. This is a real UI/rendering change, not a copy edit, and this environment has no R, so it could not be exercised through the actual Shiny app. It was instead verified with a headless Chromium harness (Playwright, pre-installed in this environment) driving the built JavaScript directly: loaded synthetic two-campaign point data, confirmed both panels render only their own campaign's palette color with zero cross-panel color contamination, dragged panel B and confirmed panel A's rendered pixels changed too (shared camera), and drew a line split across both panels (one endpoint clicked in each) producing a populated profile chart with points from both campaigns and no JavaScript errors. The R-side wiring (`comparison-app.R`'s `session$sendCustomMessage(..., target = "als-compare-cloud", ...)` calls) was not changed, since `"als-compare-cloud"` remains the logical key the JS `views` map resolves to the new two-canvas viewer — but the actual Shiny reactive flow around it (file downloads feeding the viewer, `compare_metadata`, PNG export buttons wired through R) has not been exercised end-to-end and still needs a real browser/CI pass before release, per item 2 below.

## First verified green CI run on this branch's actual head: 18 September 2026

Every prior GitHub Actions run referenced above and throughout this branch's
history had been failing (`conclusion: failure`) with a genuine `R CMD check`
ERROR - this was not caught earlier because this environment has no R and no
one had checked the Actions results themselves. Checked directly via the
`github` MCP tools' Actions endpoints (available in this session without any
local R install): `test-core.R:64` asserted the implemented-provider id set
was still `c("usgs3dep", "opentopography", "ahn6", "swisstopo")`, never
updated when `ignfr` (France) and `canelevation` (Canada) were added as
implemented adapters - all 235 other tests already passed. Fixed in commit
`cd2f4d1`, and
[run 35406911944](https://github.com/Cesarito2021/als_downloader/actions/runs/35406911944)
on that commit passed `--as-cran` (warnings as failures) on Windows, macOS,
Ubuntu-release and Ubuntu-devel - the first fully green run this session has
directly confirmed against the branch's actual current head, not a
historical revision. Confirmed again on the next two commits
(`fee02cd`, `8b9b38f`,
[run 35407457831](https://github.com/Cesarito2021/als_downloader/actions/runs/35407457831)):
Ubuntu-release's own log reports **`Status: OK`** - zero errors, zero
warnings, zero notes, all 236 tests passing. Re-check this after every
further change; a passing run today does not cover commits made after it.

## Before submission

### Review on 19 September 2026

The reviewed revision `aeebd87` had failing CI. The subsequent review
fixes signed URL redaction and background package loading. A fresh local
R 4.4.0 `R CMD check --as-cran` (including PDF manual, vignettes, report tests
and installed-package subprocess tests) completed with zero errors and zero
warnings. Two notes were new-submission feasibility and inability to verify
the environment's current time. Real development-session LAZ decoding also
passed (7,523 points, temporary file removed).

R-universe was healthy but still at `3932974` when inspected; that result does
not validate the new code. Cross-platform checks for the review are tracked
in [PR 2](https://github.com/Cesarito2021/als_downloader/pull/2).
The browser COPC experiment stays under `tools/`, excluded from the R package.
There is no fixed minimum function count in the published CRAN policy;
usefulness, documentation, portability and passing checks are the relevant
criteria. Do not add artificial functions just to increase the count.

1. Authorship confirmed by the maintainer: the project collaborators did not contribute to this application and have been removed from DESCRIPTION. Cesar Ivan Alvites Diaz is the sole package author and maintainer (`aut`/`cre`). The brief OpenForest4D project acknowledgement remains in the README. This metadata correction follows the linked release checks.
2. Re-run the successful cross-platform matrix if package code or metadata change before submission (done for the current head as of `cd2f4d1`, above). Local R 4.4.0 checks include the full PDF manual; verify current-R and R-devel against the latest published revision.
3. Review the linked dataset licenses and third-party notices; access checks do not grant rights. CC BY examples retain DOI credit; national products retain provider terms. The app never claims complete national coverage from country shading.
4. Decide the operational email process. The app prepares private drafts; automated confirmations require a separately configured mail service. The public approval metric excludes email-only requests.
5. Confirm the final package name, version and maintainer address, review any remaining check notes and prepare a truthful submission comment. No `--as-cran` run is an acceptance guarantee.

References: [CRAN Repository Policy](https://cran.r-project.org/web/packages/policies.html), [third-party notices](../inst/NOTICE), [contributor contact handling](CONTRIBUTOR_PRIVACY.md).

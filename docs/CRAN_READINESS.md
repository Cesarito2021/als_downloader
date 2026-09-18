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
- The catalogue contains 16 source/service references, not 16 integrated or fully validated datasets. Native adapters cover USGS, AHN6 and swissSURFACE3D; OpenTopography requires supplied tile indexes. Approved contributor indexes provide another route. See the installed [source tables, policies and validation evidence](../inst/sources/README.md) for portal-only entries and unresolved permissions.

## Final local verification: 17 September 2026

The full check passed again after the catalogue and licence-gate changes: **0 errors, 0 warnings, 2 notes**, including PDF/HTML manuals, examples, vignette and offline tests. The installed app passed browser smoke checks for the Shiny connection, globe, mobile width, map navigation and empty-selection guard, with no JavaScript errors.

Downloads and remote previews reject missing licence or attribution metadata. This is a completeness check, not automatic legal clearance. Current-R and R-devel results must be checked against the exact published commit in [GitHub Actions](https://github.com/Cesarito2021/als_downloader/actions); the historical runs above do not cover the final catalogue changes.

## Catalogue update: 18 September 2026

Two entries were removed from `inst/extdata/providers.csv` (18 -> 16 rows) because access was not guaranteed: PNOA LiDAR (Spain), whose anonymous download-init check returned HTTP 403, and OpenTopography AUS11_Victor (Australia), whose provider supplies no reuse licence despite a passing technical access sample. See [ACTIVE_SOURCES.md](ACTIVE_SOURCES.md) for the full record. `test-coverage-gate.R` was updated to match. This environment cannot run R, so this change has not been re-verified with a fresh `R CMD check --as-cran` or the GitHub Actions matrix — that re-run is still owed before submission, per item 2 below.

A new in-app adapter (`search_europe(aoi, "ignfr", ...)` in `R/europe.R`, wired through `find_tiles()`) queries IGN LiDAR HD (France) through the public STAC catalogue at `api.stac.teledetection.fr` (UMR TETIS / INRAE) and downloads original COPC LAZ files from `data.geopf.fr`. It is built directly from the live evidence already recorded in `inst/sources/VALIDATION.md` and `inst/sources/public-access-checks.json` (17 September 2026: STAC query and file download both succeeded anonymously). The adapter code itself, including its assumed OGC-API-Features pagination shape, has **not** been exercised against the live service in this session (no outbound network access here beyond GitHub) — mocked unit tests were added (`tests/testthat/test-europe.R`), but a real network run (local or CI) is required before this source is trusted for release. `providers.csv` flags this explicitly in its `access` field for `ignfr`.

## Before submission

1. Authorship confirmed by the maintainer: the project collaborators did not contribute to this application and have been removed from DESCRIPTION. Cesar Ivan Alvites Diaz is the sole package author and maintainer (`aut`/`cre`). The brief OpenForest4D project acknowledgement remains in the README. This metadata correction follows the linked release checks.
2. Re-run the successful cross-platform matrix if package code or metadata change before submission. Local R 4.4.0 checks include the full PDF manual; verify current-R and R-devel against the latest published revision.
3. Review the linked dataset licenses and third-party notices; access checks do not grant rights. CC BY examples retain DOI credit; national products retain provider terms. The app never claims complete national coverage from country shading.
4. Decide the operational email process. The app prepares private drafts; automated confirmations require a separately configured mail service. The public approval metric excludes email-only requests.
5. Confirm the final package name, version and maintainer address, review any remaining check notes and prepare a truthful submission comment. No `--as-cran` run is an acceptance guarantee.

References: [CRAN Repository Policy](https://cran.r-project.org/web/packages/policies.html), [third-party notices](../inst/NOTICE), [contributor contact handling](CONTRIBUTOR_PRIVACY.md).

# Release candidate 0.1.0: readiness review

No submission has been made to CRAN. Passing local checks does not guarantee acceptance.

## Package and app

- Full `R CMD check --as-cran`, including the indexed PDF and HTML manuals, finished on Windows / R 4.4.0 with **0 errors, 0 warnings and 2 notes**: new submission; unable to verify the current time. Source build, installation, documentation, examples, vignette and offline tests passed. Missing Courier and makeindex components were repaired in the local TinyTeX environment.
- NEWS formatting and self-namespace worker calls were corrected. The earlier `--as-cran --no-manual` run also finished with 0 errors, 0 warnings and the same two notes.
- [GitHub Actions run 35271083291](https://github.com/Cesarito2021/als_downloader/actions/runs/35271083291), commit `1309077`, passed all four jobs: Windows, macOS and Linux with R release, plus Linux with R-devel (`--as-cran --no-manual`, warnings treated as failures). The macOS setup now installs GDAL so its current `rlas` dependency can compile. Subsequent documentation-only updates do not alter the checked package source.
- Local browser checks passed: required private email, valid DOI, 50-word limit, compatibility-gated submission, stale-result invalidation, text export and mobile width. No message was sent.
- Real USGS discovery returned seven Utah tiles; one source tile decoded to 7,523 preview points. A two-campaign overlap displayed 12,541 and 24,405 points over 0.0024 km2, with independent visibility and original-file selection.
- The globe and search screenshots were refreshed. Comparison camera framing now optionally fits the central 98% of projected display coordinates without deleting points.
- The visual-profile update passed a fresh full local `--as-cran` check (including PDF manual): 0 errors, 0 warnings, the same two notes. Browser tests with real Utah campaigns passed line selection by clicks/drag, shared colour/visibility controls, strip-width changes, three PNG downloads, stale-profile clearing and mobile layout. Offline JavaScript tests cover directional distances, strip/end boundaries and empty sections. Profiles use displayed samples without fitting curves or calculating campaign differences.
- [Visual-profile CI run 35275843652](https://github.com/Cesarito2021/als_downloader/actions/runs/35275843652), commit `763fb27`, passed Windows, macOS, Linux release and Linux R-devel, including the new JavaScript geometry checks. This run supersedes the earlier release check for current application code.
- Ten active catalog entries remain, with nine representative file endpoints checked; see [access scope](ACTIVE_SOURCES.md). This is not verification of every file in each national archive.

## Before submission

1. Authorship confirmed by the maintainer: the project collaborators did not contribute to this application and have been removed from DESCRIPTION. Cesar Ivan Alvites Diaz is the sole package author and maintainer (`aut`/`cre`). The brief OpenForest4D project acknowledgement remains in the README. This metadata correction follows the linked release checks.
2. Re-run the successful cross-platform matrix if package code or metadata change before submission. Local R 4.4.0 checks include the full PDF manual; current-R and R-devel checks are covered by the linked CI run.
3. Review the linked dataset licenses and third-party notices; access checks do not grant rights. CC BY examples retain DOI credit; national products retain provider terms. The app never claims complete national coverage from country shading.
4. Decide the operational email process. The app prepares private drafts; automated confirmations require a separately configured mail service. The public approval metric excludes email-only requests.
5. Confirm the final package name, version and maintainer address, review any remaining check notes and prepare a truthful submission comment. No `--as-cran` run is an acceptance guarantee.

References: [CRAN Repository Policy](https://cran.r-project.org/web/packages/policies.html), [third-party notices](../inst/NOTICE), [contributor contact handling](CONTRIBUTOR_PRIVACY.md).

# Release candidate 0.1.0: readiness review

No submission has been made to CRAN. Passing local checks does not guarantee acceptance.

## Package and app

- Full `R CMD check --as-cran`, including the indexed PDF and HTML manuals, finished on Windows / R 4.4.0 with **0 errors, 0 warnings and 2 notes**: new submission; unable to verify the current time. Source build, installation, documentation, examples, vignette and offline tests passed. Missing Courier and makeindex components were repaired in the local TinyTeX environment.
- NEWS formatting and self-namespace worker calls were corrected. The earlier `--as-cran --no-manual` run also finished with 0 errors, 0 warnings and the same two notes.
- Local browser checks passed: required private email, valid DOI, 50-word limit, compatibility-gated submission, stale-result invalidation, text export and mobile width. No message was sent.
- Real USGS discovery returned seven Utah tiles; one source tile decoded to 7,523 preview points. A two-campaign overlap displayed 12,541 and 24,405 points over 0.0024 km2, with independent visibility and original-file selection.
- The globe and search screenshots were refreshed. Comparison camera framing now optionally fits the central 98% of projected display coordinates without deleting points.
- Ten active catalog entries remain, with nine representative file endpoints checked; see [access scope](ACTIVE_SOURCES.md). This is not verification of every file in each national archive.

## Before submission

1. Confirm the existing DESCRIPTION contributor roles with the maintainer: Carlos Alberto Silva, Viswanath Nandigam, Chelsea Scott and Inacio Bueno are still recorded as `ctb`; Cesar Ivan Alvites Diaz is the sole `aut`/`cre`. Their roles were not silently removed. The maintainer must confirm accurate authorship and permission to distribute contributions.
2. Require successful Windows, macOS and Linux checks on current R, plus R-devel. The repository workflow covers that matrix; its result must match the release commit. Local R 4.6.0 is installed without the package dependencies, so the local R 4.4.0 result alone is insufficient.
3. Review the linked dataset licenses and third-party notices; access checks do not grant rights. CC BY examples retain DOI credit; national products retain provider terms. The app never claims complete national coverage from country shading.
4. Decide the operational email process. The app prepares private drafts; automated confirmations require a separately configured mail service. The public approval metric excludes email-only requests.
5. Confirm the final package name, version and maintainer address, review any remaining check notes and prepare a truthful submission comment. No `--as-cran` run is an acceptance guarantee.

References: [CRAN Repository Policy](https://cran.r-project.org/web/packages/policies.html), [third-party notices](../inst/NOTICE), [contributor contact handling](CONTRIBUTOR_PRIVACY.md).

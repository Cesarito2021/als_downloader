## Submission status

Draft for maintainer review; not submitted to CRAN.

## Local check

Windows 11, R 4.4.0 (x86_64-w64-mingw32), 19 September 2026.
Full `R CMD check --as-cran`, including PDF/HTML manuals and incoming feasibility:
0 errors, 0 warnings, 2 notes:

- New submission.
- Unable to verify current time.

All 523 test expectations passed; none were skipped. The test runner reports one
test-runner warning (not a package-check warning). Pandoc also emits a
highlight-style deprecation message. Vignette/report builds complete successfully.
Private GitHub URLs were removed from DESCRIPTION and CITATION to avoid public
404 links. The designated maintainer email remains available.

## Additional validation

Offline tests and examples do not contact providers. Live development checks
include real point-cloud examples for USA, Brazil, Canada and the Netherlands,
and a Zenodo ALS archive with its author-provided Shapefile. Complete archive and
boundary checksums were verified; isolated approval, spatial search and browser
coverage refresh passed. Production inclusion remains pending maintainer approval.
No downloaded clouds, private proposals or credentials are included in the package.
JavaScript tests cover profiles and shared absolute-elevation colour scales.
Browser verification also covers readable typography, Zenodo-only submission,
private invitation exchange, explicit rejection and replay denial using synthetic data.

GitHub Actions checks Windows, macOS, Ubuntu release and Ubuntu devel with
warnings treated as failures. The final pushed commit's matrix must pass before
submission; earlier results are not evidence for a newer candidate.

## Purpose and attribution

The package provides spatial discovery, original-file downloads, bounded point
visualization, visual comparison and PDF download-planning reports in R/Shiny.
Dataset licences and citations remain separate. Notices are retained in inst/NOTICE
and alongside bundled browser libraries. Optional cloud/PDF dependencies are checked
when requested. The documented outstanding basemap/public-hosting and source-credit
questions must be resolved before the maintainer authorizes public release.

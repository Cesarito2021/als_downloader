## Submission status

Draft for maintainer review. This package has not been submitted to CRAN.

## Local test environment and results

Windows 11, R 4.4.0 (x86_64-w64-mingw32), 19 September 2026.
The full `R CMD check --as-cran`, including PDF and HTML manuals, completed
with 0 errors, 0 warnings and 1 note: unable to verify the current time.
CRAN incoming remote checks were disabled for this local run; the new-submission
review is still performed by CRAN. No acceptance is implied by local checks.

## Additional validation

Offline tests and examples do not contact data providers. Network discovery,
previews and downloads run only when explicitly invoked. Live development checks
covered a real USGS/local LAS comparison and two Zenodo records in a separate
review queue; no test data or private proposals are included in the package.
JavaScript checks cover visual-profile geometry and absolute-elevation displays.

The GitHub Actions workflow checks Windows, macOS, Ubuntu-release and Ubuntu-devel
with warnings treated as failures. Verify the final commit's successful matrix
before submitting: https://github.com/Cesarito2021/als_downloader/actions

## Purpose and attribution

The package provides spatial discovery, original-file downloads, bounded visual
previews, optional visual comparisons and PDF reports through R and Shiny.
Provider data retain their own licences and citations. Third-party code notices
are included under inst/NOTICE and alongside the bundled browser libraries.
Optional lidR/PDF dependencies are checked when their features are requested.

## Submission status

Version 0.1.2, prepared for maintainer review; not submitted to CRAN.
Public GitHub and R-universe distribution is authorized.

## Local check

Windows 11, R 4.4.0 (x86_64-w64-mingw32), 20 September 2026.
Full R CMD check --as-cran, including rebuilt vignettes and PDF/HTML manuals:
0 errors, 0 warnings, 2 notes.

- Incoming feasibility: new submission.
- Unable to verify current time.

Installed package size passes. All 637 test expectations passed, none skipped.
The test runner separately reports one dependency warning (Shiny was built under
R 4.4.3); this is not an R CMD check warning. Pandoc reports a deprecated
highlight-style option; vignette and manual builds succeed.

## Additional validation

Thirteen Python tests verify incremental catalogue preservation and exclusions.
JavaScript tests cover profile geometry and point-cloud comparisons. A public
OpenTopography metadata download passed SHA-256 and schema verification; a real
Australian AOI returned a LAZ file containing 79,770 readable points.
The README reports reproducible catalogue statistics, distinguishes collection
footprints from tile records and distinct objects, and attributes provider storage.
The optional detailed footprint resource is downloaded only when requested.
Examples and automated tests do not contact data providers. No downloaded clouds,
private proposals or credentials are included in the package.

GitHub Actions checks Windows, macOS, Ubuntu release and Ubuntu devel, treating
warnings as failures. The pushed candidate's matrix must pass before submission;
earlier green builds do not validate a newer candidate.

## Purpose and attribution

The package provides spatial discovery, original-file parallel downloads, bounded
point visualization, visual comparison and PDF download-planning reports in R/Shiny.
Dataset licences and citations remain separate. Notices are retained in inst/NOTICE
and alongside bundled browser libraries. Optional dependencies are checked on use.
Optional hosted contribution intake still requires private persistent storage and
mail/transport configuration; it is not advertised as a completed hosted service.
CRAN submission follows the maintainer's final local app review.

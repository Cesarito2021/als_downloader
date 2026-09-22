## ALSdownloadeR 0.2.0: submission preparation

The earlier alsdownloader 0.1.3 submission was cancelled at the maintainer's
request before publication. This revised submission uses ALSdownloadeR.

Final checks, 22 September 2026, code commit 4df3eced9244fe06e054f7f8cf9ad5bb1be56eef:

- Windows 11, R 4.6.0: 0 errors, 0 R CMD check warnings, 2 notes (new submission
  and HTML Tidy unavailable). 765 test expectations passed; none failed/skipped.
- Ubuntu 24.04.5, R 4.6.1: Status: OK; 766 expectations passed.
- Ubuntu 24.04.5, R-devel (2026-09-21 r90579): Status: OK; 766 expectations passed.
- macOS Tahoe 26.6.2, R 4.6.1: Status: OK; 766 expectations passed.

Remote run: https://github.com/Cesarito2021/als_downloader/actions/runs/35740368124
Remote checks used --as-cran --no-manual. The full Windows check included the
PDF manual, examples and vignette. The 24-page PDF was also visually reviewed.
Testthat reports two internal warnings on Windows and one in each remote run;
these are retained in the logs and are distinct from R CMD check warnings.

Final source archive SHA256:
43FCE84D0AE12D66B326EE7E77BC2D82C2F5AD080241782351C1F6C61E67891F
Its 146 comparable source files match the checked working tree, normalizing
line endings. Only submission notes were updated after this checked code.

The revision uses original USGS discovery and acquisition XML, including
bounded metadata-directory lookup and valid alternative XML handling. Legacy
tile-specific metadata is not propagated to neighbouring tiles. Undocumented,
ambiguous and filename-reference years remain explicitly distinguished.

A live test of the Shiny search on 40 study polygons found data in all 40;
it is not a claim of exhaustive national catalogue or point-cloud validation.
Network requests are not made on package loading or in automated tests/examples.
The optional country-link registry persists in tools::R_user_dir() or a user
path; reading packaged defaults does not write files or make network requests.

This preparation does not indicate that the CRAN form has been submitted.

## ALSdownloadeR 0.2.0: final submission checks

The earlier alsdownloader 0.1.3 submission was cancelled at the maintainer's
request before publication. This revised submission uses ALSdownloadeR.

Final checks on 22 September 2026 include the final display-name correction.
Code checked: 43aeb81235dd709eed952049fb44c4c89caa4b2c

- Windows 11, R 4.6.0: 0 errors, 0 R CMD check warnings, 2 notes (new submission
  and HTML Tidy unavailable). All 765 test expectations passed; none skipped.
- Ubuntu 24.04.5, R 4.6.1: Status: OK; 766 expectations passed.
- Ubuntu 24.04.5, R-devel (2026-09-21 r90579): Status: OK; 766 expectations passed.
- macOS Tahoe 26.6.2, R 4.6.1: Status: OK; 766 expectations passed.

Remote run: https://github.com/Cesarito2021/als_downloader/actions/runs/35743585774
Remote checks used --as-cran --no-manual. The exact submission archive was
checked with --as-cran on Windows, including the manual, examples and vignette.
The 24-page PDF manual was visually reviewed. Testthat records two internal
warnings on Windows and one per remote run; all checks completed successfully.

Source archive SHA256:
1AC6D9BB4E7D5D19E94B25AF6433A3AFC4D601764009E53BE2D87BA7D216E67D
The archive was not rebuilt or changed during these final checks.

The revision uses original USGS discovery and bounded acquisition-XML lookup,
including valid alternative XML handling and legacy tile identity checks.
Unavailable, ambiguous and filename-reference years remain distinguished.
The live Shiny search found data in all 40 study polygons; this is not an
exhaustive national catalogue or point-cloud validation claim.

Network requests are not made on loading or in automated tests/examples.
The optional country-link registry persists in tools::R_user_dir() or a user
path; reading packaged defaults does not write files or make network requests.

Only these submission notes were updated after the final checks. This document
does not indicate that the CRAN form has already been submitted.

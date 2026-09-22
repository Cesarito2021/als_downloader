## Submission preparation: ALSdownloadeR 0.2.0

The previous alsdownloader 0.1.3 submission was cancelled by CRAN at the
maintainer's request. The revised package is named ALSdownloadeR and is
prepared as version 0.2.0. This file does not indicate that a submission has been sent.

The revision changes USGS discovery, acquisition metadata, download-size
reporting and comparison selection. See NEWS.md and docs/IMPLEMENTATION_REVIEW.md.

Checks on the revised code (22 September 2026):

- Windows 11, R 4.6.0: no R CMD check errors or warnings; two notes
  (new submission and HTML Tidy not installed). All 750 test expectations
  passed, with no skips. Examples, vignettes and the PDF manual passed.
- Ubuntu 24.04.5, R 4.6.1: Status: OK.
- Ubuntu 24.04.5, R-devel: Status: OK.
- macOS Tahoe 26.6.2, R 4.6.1: Status: OK.

GitHub Actions run: https://github.com/Cesarito2021/als_downloader/actions/runs/35732487331
All three remote jobs passed 751 test expectations with no failures or skips.
Remote checks used --as-cran --no-manual; the PDF manual was checked on Windows.
The checked code and tests are commit 998e3fe3ab818e5ec8cd0777d9f5f40b2774c9c0.
The archive was then rebuilt only to restore the concise README and update NEWS;
its R code, tests, Rd files, installed assets and vignette source were compared
with the checked archive and found identical (147 files, excluding the generated
DESCRIPTION packaging timestamp).

The package reads original provider metadata and labels unavailable or
unverified dates explicitly. Network-dependent workflows are invoked only by
the user, not by package loading, tests or vignette examples. The new optional
country-link registry is stored in tools::R_user_dir() or an explicit user path;
reading the packaged defaults does not create files or make network requests.

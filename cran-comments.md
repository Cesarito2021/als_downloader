## Submission preparation: ALSdownloadeR 0.2.0

The previous alsdownloader 0.1.3 submission was cancelled by CRAN at the
maintainer's request. The revised package is named ALSdownloadeR, version 0.2.0.
This file prepares a new submission; no submission has been sent.

The revision changes USGS discovery, acquisition metadata, download-size
reporting and comparison selection. See NEWS.md. Final validation also covers
targeted USGS XML discovery in large projects, legacy tile identity checks,
and reuse of cached acquisition evidence after the lookup time budget.

Final checks are being rerun on this candidate. The results and archive hash
will be recorded in the submission preparation folder after completion.
Earlier checks on commit 998e3fe passed Windows, Linux release/devel and macOS;
those results do not certify the subsequent metadata correction.

Unavailable or unverified acquisition dates remain explicitly labelled.
Network-dependent workflows are invoked only by the user, not by loading,
tests or vignette examples. The optional country-link registry is stored in
tools::R_user_dir() or an explicit user path; reading packaged defaults does
not create files or make network requests.

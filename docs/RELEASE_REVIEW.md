# ALS Downloader 0.1.0 — maintainer review

No CRAN submission, Zenodo software deposit or production dataset approval has
been performed. These remain maintainer decisions.

## What is ready to test

- Five-field Zenodo form, metadata import, explicit polygon/file mapping,
  deduplication and private approval queue. [Live checks](ZENODO_LIVE_CHECKS.md)
  document isolated Sabah test requests, pending coverage/file validation.
  The form also accepts a centre and metric half-width for an author-declared
  approximate square, with explicit file selection and persistent approximate
  labels. This does not make the incomplete Brazil record a ready example.
- **Compare campaigns → Source campaign + my LAS/LAZ** works with one available
  source campaign. A real USGS download and local upload completed through the
  background worker. The local QA cloud had a deliberately added 2 m offset;
  this is an integration test, not observed environmental change.
- Comparison checks common coverage, projected CRS and units. Source-to-source
  mode still requires separate dates. Distributions use absolute elevations,
  consistent with the cloud legends and profile axes.
- PDF reports use the compact GitHub banner, figures, source credits and NSF
  acknowledgement. Figures are display samples, not analytical differences.

## Release checks

The full local Windows/R 4.4.0 check passed with zero errors and warnings; its
only note was inability to verify current time. Remote incoming checks were
disabled locally. The final source revision must also have a green [GitHub
matrix](https://github.com/Cesarito2021/als_downloader/actions) on Windows,
macOS, Linux release and R-devel. Historical results do not substitute for it.

[R-universe](https://cesarito2021.r-universe.dev/alsdownloader) already tracks
the repository's default branch. It updates asynchronously; verify its published
commit against the release candidate before announcing the update. Registry
registration alone does not prove a new build is available.

## Your final decisions

Review the local app, package name/version, sole-author metadata and maintainer
email; confirm remaining source and third-party attribution obligations. The
package has a substantive workflow: no artificial function count is needed.
CRAN makes its own acceptance decision under its [repository policy](https://cran.r-project.org/web/packages/policies.html).

The concise [Zenodo software-release draft](ZENODO_RELEASE_DRAFT.md) is ready for
review. Public community submissions still need operational deployment checks
(private persistent storage, reviewer access and rate/load testing); the desktop
tests do not establish capacity for 100 submissions per day.

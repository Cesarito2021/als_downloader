# Hosted pilot before CRAN

The release sequence is local maintainer review, an explicitly authorized
shinyapps.io pilot, hosted verification, then a separate CRAN decision.
No deployment is performed by loading this package.

## Prepared configuration

The repository `app.R` defaults to hosted mode: serial transfers, up to ten
tiles and 500 MiB per download batch, a 250 MiB temporary preview download,
and a 250 MiB upload request limit. Local use remains available through
`alsdownloader::launch_app()`. The private reviewer queue and credentials must
never be included in a public bundle.

Deploy only the entry point and explicitly reviewed configuration files using
`rsconnect::deployApp(appFiles = ...)`; do not upload the workspace, downloaded
clouds, historical RGB screenshots, local review folders or credentials.
The exact package revision must be installed with reproducible source metadata.
The private GitHub package requires the host's authorized private-repository
access; it must not silently use the older R-universe build.

Install `lidR`, `rmarkdown`, `tinytex` and all package imports in the deployment
environment. PDF output additionally needs Pandoc and a working TinyTeX
installation on the host. Declaring the R package alone does not install TeX.
Do not promise working hosted PDF reports until a real PDF download passes.

## Required hosted verification

- Confirm the installed source revision and memory allocation in host logs.
- Test the map and visible OpenStreetMap attribution; retain the browser's
  origin referrer and ordinary cache. No bulk/offline basemap download is offered.
- Search a small supported AOI, preview one bounded tile, export a PNG and PDF,
  and download a small batch. Check cancellation and two simultaneous sessions.
- Send one labelled Formspree test from the hosted origin and confirm actual
  Gmail delivery. Receiving metadata never publishes or approves a dataset.
- Keep approved catalogue data in durable, managed storage. shinyapps.io local
  storage is ephemeral; it is not a persistent review database. The current free
  Formspree inbox does not implement automatic approval from email.

## Current external constraints

R-universe synchronization on 20 September 2026 failed because the upstream
GitHub repository is private. Keep it private unless the maintainer explicitly
changes that decision. This is separate from GitHub's operating-system checks.
Historical Esri screenshots remain private evidence and are excluded from README;
the running map now uses OpenStreetMap. Dataset-specific conditions in
`inst/sources/USE_REVIEW.md` still apply.

References: [Posit deployment](https://rstudio.github.io/rsconnect/reference/deployApp.html),
[private package access](https://support.posit.co/hc/en-us/articles/204536558-Enabling-use-of-private-packages-on-github-com-for-applications-on-shinyapps-io),
[ephemeral storage](https://docs.posit.co/shinyapps.io/guide/storage/),
[OpenStreetMap tile policy](https://operations.osmfoundation.org/policies/tiles/).

# Private backend readiness — 20 September 2026

The maintainer has now authorized public GitHub and R-universe publication for
local testing. This does not deploy a hosted contribution backend or submit to
CRAN. The earlier backend validation below is historical evidence.

## Implemented path

Validation of the current changes: full local Windows/R 4.4.0
`R CMD check --as-cran` completed with 0 errors, 0 warnings and 2 notes;
574 expectations passed, none skipped. The test runner recorded one warning.
The existing Formspree TEST ONLY proposal was rebuilt from live Zenodo metadata,
imported twice into an isolated private queue and remained a single pending
request. No real dataset was approved. A separate synthetic end-to-end test
passed import, explicit approval and tile discovery without exposing contact
details in the approved index.

Browser checks: a 0.1386 km2 USGS AOI returned seven tiles; one real tile loaded
7,523 preview points. The viewer reported PNG ready. A second launch with
Pandoc/TinyTeX configured completed PDF generation without a server error.
Browser download persistence and the visual PDF layout were not independently
verified in this run. The later local app launch uses review/start_backend_review.R.

Contributors validate a Zenodo proposal and explicitly submit its fields to the
configured Formspree inbox. `import_zenodo_inbox(fields, queue)` reconstructs a
received proposal from current Zenodo metadata and a bounded coverage file or
declared square. It refuses arbitrary boundary URLs, record/DOI disagreement,
changed proposal references and inconsistent file mappings. Imports are
deduplicated and remain pending. The contact is private.

The existing maintainer review functions revalidate metadata before explicit
approval. Only approved indexes are searchable. Tests exercise the private queue,
invitation scope, expiry, replay rejection and catalogue refresh. Local queue
storage persists across app restarts, but needs owner-managed backups.

## Connection still missing

The free Formspree inbox is not an automatic private queue. Its supported
submission-reading API requires a Professional or Business plan and an API key:
https://help.formspree.io/articles/the-forms-api/form-submissions-api
https://help.formspree.io/articles/the-forms-api/api-keys

The authenticated dashboard can be inspected by the maintainer/agent, but its
browser session is not a supported unattended backend credential. No scraper,
payment, paid upgrade or credential extraction is implemented. A transport must
still deliver inbox fields to the importer using an authorized service. SMTP
invitation delivery is also unconfigured in the current local setup (preview
mode). Do not claim autonomous receipt-to-review delivery or phone review works.

Public catalogue distribution, a durable hosted private queue and hosted review
remain deployment tasks. A free Formspree receipt alone does not solve them.
These must be verified before offering an unattended hosted contribution service.

## R-universe

The existing public registry is `Cesarito2021/cesarito2021.r-universe.dev`; its
`packages.json` lists `alsdownloader` at the private `als_downloader` repository.
The universe URL is https://cesarito2021.r-universe.dev. Before this release its package
uses source revision `27e0161add5529b2485c00c2578c41df360d4e2b`.
The current checked development revision was `818ff8182aba1b0649925c207a623d84bb1d541d`.
Publication is now authorized. Current build evidence belongs in the release
review; the source revisions above describe the earlier private backend review.

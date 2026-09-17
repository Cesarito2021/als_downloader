# Review gate: no CRAN submission

The maintainer requested a reviewable app and country-by-country samples before submission. This branch is a development preview, not a final worldwide release.

Completed locally:

- GPL-3 package; Gmail maintainer and UF secondary contact.
- Documented functions, offline tests, built vignette.
- AOI discovery, full decoding, checksum restart for one sample in USA, Australia, Brazil, New Zealand, Taiwan; see validation.csv.
- Background Shiny transfer and bounded preview through the browser.
- Previous app preserved under legacy/; raw downloads and credentials excluded.
- Dataset suggestion form with reviewed inclusion.

Required before a release proposal:

- Implement remaining national/research adapters and sample-test each country to be advertised as supported.
- Resolve exact dataset licenses, DOIs and producer citations. Index-only OpenTopography metadata is incomplete.
- Add a second sample when first-tile results, formats or metadata vary.
- Review contributor roles and release version with the maintainer.
- Verify hosted limits, concurrent sessions, cancellation, crash recovery and quotas on the deployment server.
- Test physical Android/iOS devices, accessibility and keyboard workflows.
- Complete current R release/devel checks on Windows, macOS, Linux; build the PDF manual with TeX.
- Resolve check notes and obtain explicit submission authorization.

No CRAN upload, repository visibility change or default-branch merge is included in this preview.

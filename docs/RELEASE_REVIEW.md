# ALS Downloader 0.1.2 — release review

Prepared for maintainer review on 20 September 2026. No CRAN submission or Zenodo
software deposit has been made. The maintainer has authorized public GitHub and R-universe publication.

## Verified scope

- README now uses an annotated interface guide, concise authorship and the
  maintainer-supplied OpenForest4D, NSF and University of Florida logos.
- Welcome portrait is 104 px on desktop (88 px on narrow screens), with larger
  author and affiliation text. The README banner no longer carries an author byline.
- The maintainer approved the green terrain-line banner for the README.

- README contains four top-down cloud examples: USA, Brazil, Canada
  and the Netherlands. Point colours use Viridis by source Z. Image provenance,
  display sampling and limitations are documented; these examples do not certify
  every tile in a country.
- Outputs include original point files, map/cloud/profile/distribution PNGs,
  metadata CSV, an R download script and a PDF with known storage needs and credits.
- Explorer uses red source footprints and yellow external-access country outlines.
  Approved Zenodo coverage updates without restarting the map.
- The real Oostvaardersplassen Zenodo example passed checksum, spatial sample,
  cloud-reader, isolated approval and search checks. Its actual proposal remains
  pending. See [live validation](ZENODO_LIVE_CHECKS.md).
- Zenodo's form now supports selecting the matching archive for an ordinary
  study-area Shapefile; contributors need not edit an attribute for this case.
  Distinct per-file footprints still require explicit mapping.
- Private review uses a proposal-scoped email invitation and explicit approval,
  without an app password. Real delivery still requires host mail configuration.
  [Setup and deployment limits](REVIEWER_ACCESS.md).

## Release evidence

See [submission comments](../cran-comments.md) for the final local check and
[GitHub Actions](https://github.com/Cesarito2021/als_downloader/actions) for the
Windows, macOS, Linux release and R-devel matrix. An older green commit does not
validate a newer candidate. The source tarball and check logs are prepared locally,
without including downloaded examples, private queues or SMTP credentials.

The final 0.1.2 local Windows/R 4.4.0 check completed with 0 errors, 0 warnings and
2 notes (new submission; current time could not be verified). It includes the
PDF/HTML manuals, vignette and 637 passing test expectations, with none skipped.
The test runner additionally recorded one warning; see the local test log.

The 0.1.2 metadata includes the public R-universe package page and GitHub source
and issue URLs. Current release validation is recorded in cran-comments.md.

## Conditions still requiring attention

- **Cartography and source conditions:** Esri has been replaced by attributed OpenStreetMap cartography.
  IGN product-edition dates and original asset links are now preserved, with missing/conflicting dates refused; see
  [the use review](../inst/sources/USE_REVIEW.md). Historical Esri figures remain private review material and are no longer embedded in README.
  Software tests and source citations do not constitute blanket legal clearance.
- **Email:** optional Formspree transport is implemented with quota/error feedback.
  Its free plan is intended for Zenodo links; local polygon attachments require
  upload support. The free community form is active. A labelled localhost test was accepted
  and the maintainer confirmed Gmail receipt on 20 September 2026. SMTP remains an alternative local
  queue transport. See [Formspree setup](FORMSPREE_SETUP.md).
- **Approval:** the maintainer must review and approve the pending proposal.
  The localhost email link must be opened on the computer running the reviewer
  app; it is not a public one-click approval endpoint.
- **Public submissions:** persistent private storage, protected review, retention
  handling and load/rate-limit testing are deployment requirements. The current
  test does not certify 100 submissions per day.
- **R-universe:** verify the published build SHA matches the candidate before
  announcing an update; a registry entry is not proof of a successful new build.

The package is prepared for public distribution and local user review. Optional
hosted contribution automation remains a separate deployment task and is not
advertised as a completed public service. CRAN submission follows the final user
review; acceptance remains separate from local and CI check results.

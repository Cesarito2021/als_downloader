# ALS Downloader 0.1.0 — release review

Prepared for maintainer review on 19 September 2026. No CRAN submission or Zenodo
software deposit has been made. The GitHub repository remains private.

## Verified scope

- README contains four paired RGB/top-down cloud examples: USA, Brazil, Canada
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

## Release evidence

See [submission comments](../cran-comments.md) for the final local check and
[GitHub Actions](https://github.com/Cesarito2021/als_downloader/actions) for the
Windows, macOS, Linux release and R-devel matrix. An older green commit does not
validate a newer candidate. The source tarball and check logs are prepared locally,
without including downloaded examples, private queues or SMTP credentials.

The full local Windows/R 4.4.0 check completed with 0 errors, 0 warnings and
2 notes (new submission; current time could not be verified). It includes the
PDF/HTML manuals, vignette and 508 passing test expectations, with none skipped.
The test runner additionally recorded one warning; see the local test log.

Private GitHub URLs were removed from DESCRIPTION and the software citation to
avoid inaccessible links in CRAN metadata. The maintainer contact is retained.
This does not require making the development repository public.

## Conditions still requiring attention

- **Cartography and source conditions:** the documented Esri entitlement/public
  hosting question and IGN information-update attribution remain unresolved in
  [the use review](../inst/sources/USE_REVIEW.md). Gallery RGB figures remain
  private review material until their publication conditions are settled.
  Software tests and source citations do not constitute blanket legal clearance.
- **Email:** the real proposal notification is a local preview, not a sent email.
  Configure a sender and SMTP transport before claiming automatic email delivery.
- **Approval:** the maintainer must review and approve the pending proposal.
  The localhost email link must be opened on the computer running the reviewer
  app; it is not a public one-click approval endpoint.
- **Public submissions:** persistent private storage, protected review, retention
  handling and load/rate-limit testing are deployment requirements. The current
  test does not certify 100 submissions per day.
- **R-universe:** verify the published build SHA matches the candidate before
  announcing an update; a registry entry is not proof of a successful new build.

The app is available for local review. Do not describe the candidate as fully
cleared for public release while these items remain open. CRAN submission and
acceptance remain separate from local and CI check results.

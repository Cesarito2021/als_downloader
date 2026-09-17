# Preview validation record

Validated locally on 2026-09-16, Windows 11, R 4.4.0.

| Check | Result | Boundary |
|---|---|---|
| Offline tests | 59 assertions passed | Geometry/CRS, uploads, measured tile indexes, workers, pagination, bounded sampling, transfers, checksum restart, error responses, output locking |
| R CMD check | 0 errors, 0 warnings, 2 notes | `--as-cran --no-manual`; notes concern the development version/new submission and external time verification |
| Source package | Built and installed | Includes generated Rd documentation and offline HTML vignette |
| Five point-cloud samples | Passed transport/decoding | Four ALS samples (USA, Australia, Brazil, New Zealand) and one UAV-photogrammetry sample (Taiwan); corrected 2026-09-17; evidence in validation.csv |
| Real parallel download | Two workers passed | Australia and Taiwan samples; original future plan restored |
| Local Shiny workflow | Passed | Upload AOI, map controls, discover, select, background download, bounded preview |
| Hosted Shiny workflow | Passed on localhost | Local path/worker controls absent; one-tile transfer and downloadable ZIP with point cloud, manifest, citations |
| Responsive browser layouts | Passed | Headless Edge, 1440px desktop / 768px tablet / 390px phone; no horizontal overflow |
| Browser errors | None during final workflows | Includes Shiny client errors captured in console |
| GitHub CI | See Actions for latest status | Windows/macOS/Linux current R and Linux R-devel; not equivalent to the local R 4.4 result |

Run live sample validation explicitly from the repository root, after installing the package and `lidR`:

```sh
Rscript tools/validate-samples.R /path/to/TileIndex_all /path/to/sample-output
```

The script uses five recorded point-cloud samples, including the explicitly classified Taiwan photogrammetry sample, and writes `sample-report.csv`. It needs internet access and roughly 50 MB for raw samples. Never run it automatically on package load or in offline checks. Transport validation in normal downloads does not fully decode point streams; the acceptance script does.

Physical devices, production hosting, concurrent-session/cancellation stress tests, complete citation metadata and the remaining provider adapters are still release gates. See RELEASE_CHECKLIST.md.

## Source review and documentation update — 2026-09-17

Version 0.1.0.9001 reorganizes the README and updates the source catalog; download algorithms are unchanged.
All 45 supplied source records were reviewed. See [SOURCE_AUDIT.md](SOURCE_AUDIT.md) for corrected identities, dated HTTP checks, authentication limits and direct file evidence.
One Canadian COPC header was read anonymously; one complete Swiss ZIP matched its STAC checksum and decoded to 930 LAS points.
The Swiss check is outside the application adapter and does not enable Swiss AOI search.

New live Shiny captures exercised AOI upload, a seven-tile USGS search and a 47,020-point local preview without JavaScript page errors.
The README images are actual application screenshots with red control annotations, not interface mockups.
The existing offline suite was rerun: 59 assertions passed.
The updated package installed and loaded successfully as 0.1.0.9001.

Inspection of the preceding GitHub run (`a485c72`) showed successful Windows, Linux release and Linux-devel checks;
macOS failed while installing dependencies, before the package check. This is not a macOS pass.
Consult the latest Actions run for the newly published commit; the preceding results do not certify it.

Italian source supplement: the public Sila record (Nicola Puletti / CREA) was verified via Zenodo API.
Two HTTP byte ranges from the original July 2019 ALS file returned 94,208 bytes; 1,024 point coordinates were decoded.
This is a bounded sample check, not complete-file validation. See [ITALY_PULETTI.md](ITALY_PULETTI.md).
# Aerial-only Zenodo scope update — 17 September 2026

Source submission UI update: the 59 existing package assertions passed and local installation succeeded.
Live Edge checks verified required fields, review acknowledgement, Unicode and embedded-URL encoding in
email/GitHub drafts, text export, 390 px mobile width and access from another tab. No email or issue was sent.
The [form capture](images/interface-submit-source.png) uses illustrative metadata. Delivery through an external
email client and GitHub submission were not exercised; no direct email service is configured.

The complementary research selection now includes Sila, Tree-LiMS and EBA; all Zenodo adapters remain disabled.
Known Taiwan photogrammetry was removed from the active catalog but retained in the historical validation/audit records.
The package's 59 existing assertions passed again and the updated package installed locally.
Relative documentation links and image assets passed validation. No additional full dataset download was performed.
# RGB basemap and selected-tile preview — 17 September 2026

The package suite passed **63 assertions**, including four checks rejecting unknown/zero/oversized remote previews and non-HTTPS URLs.
Local installation succeeded. A live Edge run searched the Utah AOI and downloaded
`USGS_LPC_UT_StatewideSouth_2020_A20_12SUH7020.copc.laz` through the new temporary preview path.
The plot displayed **98,758 sampled points**. RGB image tiles loaded; Viridis/Magma switching,
keyboard zoom, fit reset, empty-selection rejection and 390 px mobile layout passed.
This verifies a representative remote preview, not all provider datasets. Hosted multi-session operation was not browser-tested in this update.
Screenshots: [map and plot](images/interface-rgb-tile.png), [Viridis](images/tile-viridis.png), [Magma](images/tile-magma.png).
These are actual app renders, not illustrative point clouds. Full remote files are limited to 200 MB before sampling;
the viewer does not implement COPC range streaming. RGB imagery dates are independent of LiDAR dates.
# Campaign comparison — 17 September 2026

The package suite passed **91 assertions**, including shared-grid +3 m, -2 m and zero differences,
missing/undersampled coverage, shared display origins, CRS rejection, date eligibility, vertical-reference gating and app startup without campaigns.
The installed Shiny dependency emits a build-version warning (built under R 4.4.3); tests otherwise passed and local package installation succeeded.

Live Edge test: the small Utah AOI returned three tiles each from `UT_KaneCo_2019` and `UT_StatewideSouth_1_2020`.
The comparison processed **12,525 A points and 24,390 B points** inside the AOI. Overlay, independent palettes,
show/hide controls, campaign-only download selection and 390 px layout passed without JavaScript errors.
The source reports A's interval as **2020-01-01 to 2019-12-31**; the app flags it and correctly leaves temporal-difference output unavailable.
This is evidence of real-source access and overlay, **not** evidence of forest change or a validated interannual difference.
One earlier provider request failed; later access checks and the complete browser run succeeded.

A separately labelled synthetic browser fixture verified the known +3/-2 m grid, the difference plot,
CSV export, parseable provenance JSON and disabling exports after a vertical-reference mismatch.
The synthetic fixture is a test, not an actual dataset included in the catalog.
[Comparison method and limits](TEMPORAL_COMPARISON.md). Hosted multi-session comparison was not browser-tested.

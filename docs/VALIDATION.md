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

## Forest close-up preview — 17 September 2026

The package suite passed **99 assertions**, including XY cropping, input preservation, selection of actual
returns during voxel thinning, retention of an explicit elevation outlier without thinning, and display limits.
Local installation succeeded; the existing Shiny build-version and locale warnings remain.

The display design draws on the user's biomass `export_lidar_html` scripts. Live Edge checks loaded the
user-supplied `03_04_exercise__laz.laz` forest exercise (33,845,306 source points). A reader target of 2%,
a central window covering 25% of each XY axis, no voxel thinning and vertical exaggeration 1 produced
**58,648 displayed points**. Viridis/Magma, camera controls and 390 px layout passed without JavaScript errors.
No denoising or height normalization was applied. The file lacks usable CRS metadata and is an illustrative
forest example, not a newly verified geographic source or evidence of interannual change.

The README now uses the actual forest render and a newly annotated interface capture.
[Capture settings and provenance](images/forest-capture-provenance.json).
Display sampling is bounded at 750,000 reader points and 150,000 plotted points; source downloads and
the separate campaign-comparison calculations are unchanged.

## Zenodo source connection — 17 September 2026

The Sila access probe succeeded again: HTTP 206, 94,208 bytes read and 1,024 LAS point records decoded.
The full 6.82 GB file and its checksum were not validated. The general web reader returned HTTP 429;
the direct API probe and the live Shiny API connection succeeded.

The package suite passed **105 assertions**, including aerial-file selection and rejection of missing files,
changed links, mismatched records and changed licenses. Installation succeeded with the existing locale
and Shiny build-version warnings. Live Edge verified the initial disabled link, successful metadata connection,
the exact aerial file URL, size, citation, license and 390 px layout without JavaScript errors.
No browser download of the complete file was initiated. The app connects and links to Zenodo; it does not
proxy or store this point cloud and does not offer AOI discovery for it.
[Live interface capture](images/interface-zenodo-source.png).

## Provider proposal metadata — 17 September 2026

The existing 105 package assertions passed and local installation succeeded with the previously recorded warnings.
Live Edge verified that storage location, short description, acknowledgement and dataset DOI/origin are required
alongside the access link and existing fields. All new values survived Unicode/URL encoding in both email and
GitHub drafts and the downloaded text proposal. Review gating, access from another tab and 390 px layout passed
without JavaScript errors. No email or GitHub issue was sent. The GitHub suggestion template requests the same
metadata; its hosted submission flow was not exercised. The README form screenshot was refreshed.

## Welcome globe and shared preview — 17 September 2026

The existing 105 package assertions passed; installation succeeded with the known locale and Shiny build warnings.
Live Edge verified the initial globe, keyboard rotation, opening the map, rejected empty selection, seven-tile
USGS search, and automatic navigation from Plot selected tile to the single 3D preview. The real remote tile
`USGS_LPC_UT_StatewideSouth_2020_A20_12SUH7020.copc.laz` displayed 61,230 sampled points at the 2% reader target.
Palette switching and 390 px layouts passed without JavaScript errors. Country shading is catalog presence,
not survey coverage. The globe uses bundled Natural Earth / World Atlas outlines with an original canvas
renderer; the basemap still requires its external imagery service. The duplicate map figure was removed
from the README and replaced by one refreshed satellite capture with A–F annotations.

## Provider dates and globe refinement — 17 September 2026

The application now displays provider dates unchanged and does not independently validate years.
The previous chronological comparison gate is removed: unknown, inconsistent and overlapping intervals
no longer block B-minus-A output. Spatial/vertical compatibility and point-count checks remain.
The 105 package assertions passed, including updated date-policy checks, with the existing dependency warnings.
Browser checks verified globe rotation, button and keyboard reset, mobile width and the provider-date
explanation without page errors. The README still contains exactly three figures; the globe screenshot
is retained only as a separate documentation asset.

## Overlap-only visualization — 17 September 2026

Comparison now displays two clouds within the intersection of their provider footprints and the AOI,
with a maximum shared area of 1 km². The former analytical grids, metrics, date gates and difference
exports are removed. No denoising or duplicate removal is performed; finite coordinates are clipped
and sampled for display. Internal gaps in source footprints are not inferred as observed coverage.

The revised package suite passed 96 assertions, including partial/disjoint/touching footprints,
AOI clipping, rejection above 1 km², shared display coordinates, CRS mismatch rejection and absence
of analysis controls/results. Installation succeeded with the previously documented dependency warnings.

Live Edge displayed 12,541 A points and 24,405 B points in a 0.0024 km² overlap from the two USGS campaigns.
Independent palettes, show/hide, separate original-download selection and 390 px layout passed without
JavaScript errors. No analysis controls or difference exports were present. The comparison capture was
refreshed as a separate asset; the README retains exactly its three requested figures.

## User-selected comparison — 17 September 2026

Comparison now requires explicit opt-in, at least two campaigns returned for the same AOI, and a
user-selected distinct pair with positive overlap no larger than 1 km². The button is disabled
until these conditions hold; the server repeats the eligibility check before starting work.
There is no default pair. Opting out cancels the worker and clears the preview.
The package suite passed 103 assertions. Live Edge verified the disabled initial state, selection
without opt-in, enabling an eligible pair, opting out, and rejecting the same campaign twice.
No point-cloud download was started by that browser check; no JavaScript errors occurred.

## Grey/black visual overlay — 17 September 2026

Comparison defaults to solid grey A and solid black B on a white canvas. Solid-colour legends identify
clouds without an elevation ramp or numerical elevation bounds. The user assigns the first/earlier
cloud to A and latest to B; no automatic chronology or difference calculation is introduced.
A labelled synthetic browser fixture verified actual grey/black/white rendered pixels and hiding B,
without JavaScript errors. The existing 103 package assertions passed with the previously recorded warning.
No new LiDAR download was needed, and the README still has exactly three figures.


## Contributor technical pre-test - 17 September 2026

The public lidR `MixedConifer.laz` example was fetched from its upstream GitHub raw URL, decoded and displayed in the submission modal: 0.25 MB and 18,828 sampled points. This was a technical test only, not a catalog addition or submission. Browser checks confirmed that changing the URL clears the successful result, private hosts are rejected, and no browser errors occurred. Tests also cover credentials, private/mixed DNS addresses and non-HTTPS URLs. The test pins a vetted public IPv4 address, disables redirects, and limits transfers to 50 MB with a known length.

The approval-report calculation was checked with zero approvals and a synthetic 2.5-day approval. Real statistics are produced from GitHub issue approval labels; no synthetic approval is published. License declarations are collected, not legally validated by this technical test.


## Link-only contributor check supersedes sample preview - 17 September 2026

The submission pre-test now requests HTTP headers only. Cloud downloading, decoding and plotting were removed from this flow. The vector alligator indicates technical stages; 100% means ready for maintainer review. Unit checks cover a 9 GB declared file without transferring its body, unrecognized links and HTML responses. The earlier sample-preview validation records a superseded implementation. Approval statistics remain separate.

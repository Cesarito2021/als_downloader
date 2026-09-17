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

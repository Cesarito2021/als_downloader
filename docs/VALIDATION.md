# Preview validation record

Validated locally on 2026-09-16, Windows 11, R 4.4.0.

| Check | Result | Boundary |
|---|---|---|
| Offline tests | 59 assertions passed | Geometry/CRS, uploads, measured tile indexes, workers, pagination, bounded sampling, transfers, checksum restart, error responses, output locking |
| R CMD check | 0 errors, 0 warnings, 2 notes | `--as-cran --no-manual`; notes concern the development version/new submission and external time verification |
| Source package | Built and installed | Includes generated Rd documentation and offline HTML vignette |
| Five country samples | Passed | One decoded tile each; exact evidence in validation.csv |
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

The script uses the five recorded samples and writes `sample-report.csv`. It needs internet access and roughly 50 MB for raw samples. Never run it automatically on package load or in offline checks. Transport validation in normal downloads does not fully decode point streams; the acceptance script does.

Physical devices, production hosting, concurrent-session/cancellation stress tests, complete citation metadata and the remaining provider adapters are still release gates. See RELEASE_CHECKLIST.md.

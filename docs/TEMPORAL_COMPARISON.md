# Comparing acquisition campaigns

This is an **exploratory comparison**, not an automated forest-change assessment.
The application links existing source archives; it does not publish or permanently host their clouds.

## Workflow

1. Draw/upload a small AOI and search a date interval that includes both surveys.
2. Open **Compare campaigns**. Choose reference **A** and comparison **B**. B need not be later than A.
   The options identify the source project, acquisition interval and number of intersecting tiles.
   Provider dates are displayed unchanged; project-name years are not substituted. The app does not independently validate or correct acquisition years.
3. Choose the grid resolution and the minimum point count per cell in each campaign, then **Load and compare AOI**.
4. Inspect the overlaid clouds. Set A and B independently to Cyan, Orange, Viridis, Magma, Plasma or Cividis;
   hide either cloud, orbit, zoom and adjust the explicitly labelled vertical exaggeration.
5. Before unlocking a temporal difference, verify the source vertical datum/geoid, metre Z units,
   absolute elevations and alignment. Enter the matching vertical references and acknowledge verification.
   The application does not perform datum conversion or automatic registration.
6. Inspect the difference and export its grid CSV and provenance JSON when eligible.
7. **Select A tiles for download** or **Select B tiles for download** selects only that campaign in Explore.
   Review that selection and use **Download selected tiles**. The comparison does not decide which campaign to retain.

Even a preview needs source data: this implementation temporarily downloads complete tiles before clipping.
It does not promise comparison without downloading, and is not a remote COPC streaming implementation.

## What is calculated

Both campaigns must declare equivalent projected CRS definitions in metres. Incompatible or missing CRS definitions are rejected;
there is no silent per-cloud recentering or reprojection. The overlay uses one shared XYZ origin, camera and elevation range.
Up to 50,000 points per campaign are retained for display only.

For analysis, points are clipped to the **exact AOI**, including holes. Withheld points and noise classes 7/18 are excluded;
exact duplicate XYZ coordinates within each campaign are removed. Other returns remain included.
All retained AOI points contribute to a grid anchored at integer multiples of the selected cell size in the source CRS.
Each cell records point counts and the **95th percentile of source elevation** (R quantile type 7) for A and B.

**Difference = P95(B) − P95(A)**, in metres. Positive is higher in B; negative is lower in B.
Only cells meeting the minimum count in both campaigns receive a difference. Missing/undersampled cells are grey and export as missing values,
not zero. Cells are not interpolated. The summary reports the number of eligible cells and their median difference.
CSV cell centers and the CRS in the accompanying JSON allow reconstruction of the grid; partial edge cells do not imply full-cell AOI coverage.

This metric is not a canopy-height model or point-to-point displacement. It can respond to terrain sampling, density,
leaf-on/leaf-off conditions, noise, classification, alignment and acquisition differences. No uncertainty interval,
minimum detectable change, significance test, growth estimate or biomass estimate is computed.
Ground normalization is a separate processing step: see the [lidR height-normalization documentation](https://r-lidar.github.io/lidRbook/normalization.html).
Reference systems and units must be checked against [USGS processing requirements](https://www.usgs.gov/ngp-standards-and-specifications/lidar-base-specification-data-processing-and-handling-requirements)
and the metadata for the particular surveys.

## Eligibility and resource limits

| Check | Current behavior |
|---|---|
| Dates | Provider metadata is displayed unchanged. Missing, inconsistent, overlapping or reversed intervals do not block B-minus-A differences. The user chooses the campaign order. |
| Vertical reference | Explicit matching user-entered references and verification are required; this acknowledgement is not automated verification. |
| AOI | Maximum 0.25 km². |
| Transfers | At most four tiles and 200 MB per campaign; maximum 100 MB per tile, known HTTP size required. Sequential downloads, shared hosted transfer lock. |
| Processing | Maximum 50 million source points per tile, two million retained AOI points per campaign and approximately 50,000 AOI grid cells. |
| Sampling | Analysis uses full retained AOI points; only display clouds are thinned. |
| Lifecycle | Changing AOI, campaigns or grid settings cancels/invalidate the previous comparison. Temporary files are removed after completion, cancellation or session exit. |
| Dependencies | Optional `lidR` is required. Errors leave the comparison unavailable; they do not substitute an empty cloud or zero difference. |

The JSON export records the selected campaigns/files, CRS, user-verified vertical reference, method, grid parameters,
point counts, AOI geometry, software version and export time. Keep it with the CSV and cite the original surveys separately.

## Validation

Controlled tests cover known +3 m, −2 m and zero differences, missing coverage, low point counts, shared-origin preservation,
different CRS definitions and unverified/mismatched vertical references. Tests confirm that inconsistent/unknown dates and date overlap do not block differences.
A clearly labelled synthetic browser fixture verifies the difference plot, CSV/JSON export and invalidation when the vertical reference changes.
Synthetic offsets are not evidence of real landscape change. See [validation history](VALIDATION.md) for live-provider checks and limitations.

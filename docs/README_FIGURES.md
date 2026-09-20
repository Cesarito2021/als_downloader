# README figure provenance

Prepared 19 September 2026 from the current interface and recorded regional
verification results. An isolated documentation instance replayed previously
retrieved tile metadata and decoded point samples. These are real source samples,
not newly executed provider searches or a fresh cross-country validation run.
No synthetic points are used in the four examples. The header banner is a
decorative, procedurally generated illustration, not a survey result.

The coverage overview image uses the bundled public survey masks and configured
regional indexes, with no RGB basemap. Geometry provenance and display
generalization are documented in [DISCOVERY_COVERAGE.md](../inst/sources/DISCOVERY_COVERAGE.md).

## Examples

| Example | Source sample | Displayed points | Credit / licence |
|---|---|---:|---|
| USA: Utah | USGS_LPC_UT_StatewideSouth_2020_A20_12SUH7021.copc.laz; search also includes nearby 2019/2020 tiles | 19,798 | USGS 3DEP; Microsoft Planetary Computer access. Retain original survey provenance. [Dataset](https://planetarycomputer.microsoft.com/dataset/3dep-lidar-copc). |
| Brazil: Sao Paulo | BR17_SaoPaulo | 19,965 | PMSP (2024), Sao Paulo 2017 LiDAR. OpenTopography, [DOI 10.5069/G9NV9GD1](https://doi.org/10.5069/G9NV9GD1). Municipality of Sao Paulo, SMDU/M3DC; AWS hosting. Source record lists GNU GPLv3. |
| Canada: Athabasca | pc_083I11NE41NE_20181006.copc.laz | 19,990 | Natural Resources Canada, CanElevation. Contains information licensed under the [Open Government Licence – Canada](https://open.canada.ca/en/open-government-licence-canada). |
| Netherlands | AHN6 | 19,997 | AHN; [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/). [Official access](https://www.ahn.nl/dataroom). |

The cloud images use the application's PNG export, including classification
legends, source credits and coordinate-unit notes. Source classes are preserved;
no forest classification, normalization or change detection is performed. The
camera is top-down and the vertical scale factor is 1. Samples are not full
tile-density representations. Where units are unresolved, they remain explicitly
unverified. Acquisition dates remain unknown if not provided; the year in a
filename is not substituted for verified metadata.

Map panels fit the returned tile extents and retain the AOI. Remote basemaps are
omitted pending confirmation of the maintainer's Esri entitlement. Country/source
reference overlays are hidden to focus these examples on the returned tiles.
These images must not be described as RGB maps. Table images show a visible
portion of actual search metadata; full metadata remain available through export.

## Interface gallery

The gallery covers Explore, 3D view, 3D comparison, About, the source catalogue,
contribution and public submission-status screens. The comparison capture shows
the unavailable state for a single dated source; it does not demonstrate measured
change or a real two-source comparison. No private review queue or contact
submission is displayed. The example output path is illustrative.

The app may download point clouds temporarily for visualization. These
documentation captures reuse existing local samples and initiate no new cloud
download. Image assets live under `docs/`, excluded from the R package source
bundle by `.Rbuildignore`.

Before publishing RGB versions, resolve the Esri item/account conditions in the
[access review](../inst/sources/USE_REVIEW.md). Software licensing does not grant
additional rights to imagery or source data. Preserve this provenance and the
figure credits when sharing the examples.

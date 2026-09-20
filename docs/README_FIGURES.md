> 20 September 2026: historical Esri RGB panels and the old Explorer screenshot
> are retained only as private review evidence and are no longer embedded in README.
> The application now uses OpenStreetMap; the cloud figures remain unchanged.

# README figure provenance

Prepared 19 September 2026 from the current interface and recorded regional
verification results. An isolated documentation instance replayed previously
retrieved tile metadata. The paired gallery resampled the same verified original
files with a 250,000-point limit; a more complete Utah tile from the same search replaces the narrow fragment. These are real source samples,
not newly executed provider searches or a fresh cross-country validation run.
No synthetic points are used in the four examples. The header banner uses a cropped Groningen AHN6 point-cloud view with a dark overlay for title legibility. Source: AHN, licensed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/). The full example and elevation legend remain in the gallery.

The coverage overview image uses the bundled public survey masks and configured
regional indexes, with no RGB basemap. Geometry provenance and display
generalization are documented in [DISCOVERY_COVERAGE.md](../inst/sources/DISCOVERY_COVERAGE.md).

## Examples

| Example | Source sample | Displayed points | Credit / licence |
|---|---|---:|---|
| USA: Utah | USGS_LPC_UT_StatewideSouth_2020_A20_12SUH6920.copc.laz; search also includes nearby 2019/2020 tiles | 248,432 | USGS 3DEP; Microsoft Planetary Computer access. Retain original survey provenance. [Dataset](https://planetarycomputer.microsoft.com/dataset/3dep-lidar-copc). |
| Brazil: Sao Paulo | BR17_SaoPaulo | 240,450 | PMSP (2024), Sao Paulo 2017 LiDAR. OpenTopography, [DOI 10.5069/G9NV9GD1](https://doi.org/10.5069/G9NV9GD1). Municipality of Sao Paulo, SMDU/M3DC; AWS hosting. Source record lists GNU GPLv3. |
| Canada: Athabasca | pc_083I11NE41NE_20181006.copc.laz | 249,310 | Natural Resources Canada, CanElevation. Contains information licensed under the [Open Government Licence – Canada](https://open.canada.ca/en/open-government-licence-canada). |
| Netherlands | AHN6 | 248,192 | AHN; [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/). [Official access](https://www.ahn.nl/dataroom). |

The paired cloud figures use R point plots of the application reader's display
samples, on a square 900-pixel canvas. The view is straight down onto the source
XY plane, with source elevation Z colours (Viridis) and no vertical exaggeration. No forest classification, height normalization or change detection is
performed. For contrast, each colour scale spans the 2nd to 98th percentiles
of the displayed source elevations; points outside that range use the endpoint
colours. Legend limits are marked <= and >=. Original elevations are unchanged.
Samples are not full tile-density representations. Where units are
unresolved, they remain unverified. Acquisition dates are not inferred from
filenames. The metadata tables have been removed from the paired gallery.

Each RGB panel frames the exact original tile shown in the adjacent cloud,
with a red footprint and the Esri service attribution retained inside the image.
The overview layers and map controls are hidden for figure capture. Satellite
imagery may have a different acquisition date from the LiDAR. These are private
review figures; publication remains subject to the imagery conditions below.

## Interface gallery

The gallery covers Explore, 3D view, 3D comparison, About, the source catalogue
and the Zenodo contribution form. The comparison capture shows
the unavailable state for a single dated source; it does not demonstrate measured
change or a real two-source comparison. No private review queue or contact
submission is displayed. The example output path is illustrative.

The app may download point clouds temporarily for visualization. The paired
gallery fetched the selected Utah tile and the same verified Brazil, Canada
and Netherlands tiles for denser display samples. Other interface captures
reuse the earlier samples. Image assets live under `docs/`, excluded from the R package source
bundle by `.Rbuildignore`.

Before publishing RGB versions, resolve the Esri item/account conditions in the
[access review](../inst/sources/USE_REVIEW.md). Software licensing does not grant
additional rights to imagery or source data. Preserve this provenance and the
figure credits when sharing the examples.


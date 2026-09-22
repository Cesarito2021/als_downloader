> 20 September 2026: historical Esri RGB panels and the old Explorer screenshot
> are retained only as private review evidence and are no longer embedded in README.
> The application now uses OpenStreetMap; the cloud figures remain unchanged.

# README figure provenance

## Concise README restoration (22 September 2026)

The submitted 0.1.3 README is the structural baseline. `01-globe-complete.png`
is one complete capture of the current app, with labels A-L for the globe,
buttons, tabs and source/author links. It replaces the split captures below.
`02-explorer-guide.png` restores the complete A-F Explorer view with the current
acquisition-year table and download controls. A documentation-only app session
replayed the previously retrieved original USGS California record (2018,
39.69 MB); it did not invent records or repeat a provider search.
`03-workflow-outputs.png` retains the composite layout: the current USGS table
and PDF report excerpt replace the old panels A/D; the existing real AHN and
Apalachicola point-cloud panels B/C are retained. Only screenshot framing,
cropping, panel placement and annotation were applied. The application code
was not changed. The older preparation notes below remain historical evidence.

## Complete globe control labels (22 September 2026)

The README uses `workflow/01-globe-labelled.png` and
`workflow/01-globe-controls-labelled.png`, rendered from the accompanying SVG
overlays on the unchanged current screenshots. Every visible button, navigation
tab and source link has a letter and a corresponding caption: A globe, B Open
map, C Submit ALS data, D About the project, E GitHub, F Source catalogue,
G Restart rotation, H Explore, I 3D view, J 3D comparison, K Natural Earth.
The mode indicator and access legend are not buttons. The two images are
separate viewport captures, not a single continuous screenshot.

## Explorer and output figures (20 September 2026)

`workflow/02-explorer-guide.png` combines consecutive viewport screenshots from
the current local app without stretching the map. Red outlines and corner letters
are added after capture. The Utah AOI returned seven USGS 3DEP records from the
Planetary Computer catalogue. The overview access layers were hidden using the
map's layer control so that returned tile footprints remain visible. OpenStreetMap
attribution is retained. Missing acquisition dates and file sizes remain blank;
they were not filled for illustration.

`workflow/03-workflow-outputs.png` is a cropped and labelled composite of actual
outputs, not an AI-generated illustration. Panels intentionally use different
sites: (A) the same Utah search table; (B) the existing Groningen AHN6 elevation
view, under CC BY 4.0; (C) recorded USGS Florida Panhandle 2018 and Hurricane
Michael 2019-2020 acquisitions over a 100 x 100 m Apalachicola area; (D) the
current PDF report for one IGN France tile. The report title is ALSdownloadeR
Report. Its displayed missing-size message is retained.

For panel C, the unmodified application comparison UI and canvas renderer were
run in an isolated local documentation session with previously downloaded real
points. Up to 50,000 points per acquisition were sampled at evenly spaced record
indices. Both clouds share EPSG:6345 and NAVD88 elevations in metres. A 2 m-wide
profile was drawn through the UI; the elevation distribution is a binned count,
not a fitted probability density or a calculated change. No synthetic offsets,
surface interpolation or registration were applied. Source records:
[Florida Panhandle](https://www.fisheries.noaa.gov/inport/item/58298) and
[Hurricane Michael](https://www.fisheries.noaa.gov/inport/item/69038).

Only crops, image placement, resizing and labels were applied to these captures.
The existing globe animation is unchanged. These documentation images are
excluded from the CRAN source archive by the `docs` rule in `.Rbuildignore`.

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


# alsdownloader 0.1.0.9001

* Default visual cloud overlays to solid grey for A and solid black for B on a white background, with cloud-identity legends and no elevation numbers for solid colours.

* Make comparison opt-in with no default pair. Enable visualization only when the current AOI contains at least two campaigns and the chosen pair has eligible overlap up to 1 km².

* Restrict comparison to visualization of two clouds within their shared footprint/AOI intersection, at most 1 km². Remove P95 grids, statistics, analysis exports and analytical preprocessing; retain display sampling and separate original-file downloads.

* Use provider dates unchanged without independent year verification or chronological restrictions on B-minus-A comparisons; retain spatial compatibility checks.
* Refine the globe colors and geographic grid, and add button/keyboard reset controls. The README retains exactly three figures.

* Limit the README to three figures: annotated main map, 3D preview and source submission form. Record a decoded coastal post-Michael tile and distinguish acquisition, file creation and catalog dates.

* Add a rotatable welcome globe rendered from bundled Natural Earth cartography; catalog countries use a red 0.55-opacity tint. Open map continues to AOI discovery.
* Route selected map tiles to the shared 3D preview instead of a duplicate viewer under Explore. Consolidate the README map guide into one annotated satellite capture.

* Require separate source proposal fields for the access link, storage host/location, short description, acknowledgement and dataset DOI/original platform in Shiny and the GitHub template; include them in all proposal drafts and text exports.

* Add display-only forest windows, adjustable reader sampling (2% target by default), optional voxel thinning, point size and a silhouette camera at 1:1 vertical scale; replace the representative preview with a real forest close-up from the supplied biomass example.

* Add campaign-specific download selection, AOI-clipped dual-cloud overlays and independent palettes.
* Add exploratory shared-grid P95 elevation differences with date/CRS/vertical-reference checks and CSV/JSON export; missing coverage never becomes zero change.

* Add Esri RGB imagery with translucent footprints and a selected-tile 3D panel in Explore; fit the landscape automatically with Viridis/Magma palettes and explicit vertical exaggeration.
* Bound temporary remote previews to one known-size tile up to 200 MB and at most 100,000 displayed points.

* Add a persistent source-submission button with a review form, email/GitHub drafts and text export; no point-cloud hosting or automatic publication.

* Separate complementary Zenodo research deposits from national sources; curate aircraft/UAV laser datasets and exclude known photogrammetry from the active catalog.

* Add Nicola Puletti / CREA Sila National Park as the Italian ALS catalog source, with CC BY 4.0 attribution and bounded file-access evidence.

* Reorganize the README around annotated live UI captures and a concise workflow.
* Review all 45 supplied source records; record HTTP checks, access limits and corrected links.
* Correct Taiwan and East Helanshan photogrammetry and distinguish derived AfriSAR/GEDI products from raw ALS.
* Verify an anonymous Canadian COPC header and decode a small Swiss LAS sample.
* Restore contributor credits, acknowledgements, software/data citation guidance and disclaimer.
* Remove the previous standalone application from the active tree; Git history retains it.

# alsdownloader 0.1.0.9000

* Introduced an R package and responsive Shiny explorer.
* Added validated polygon inputs and global geodesic area calculations.
* Added USGS 3DEP STAC discovery and OpenTopography local TileIndex discovery.
* Added explicit hosted/local worker planning and background Shiny transfers.
* Added transport validation, checksum-based resume records and citation exports.
* Added bounded LAS/LAZ previews and an offline-testable coordinate sampler.
* Additional providers are catalog candidates, not active download adapters.
# Zenodo source connection

* Add an explicit live connection to the Sila Zenodo record with a direct browser link to its reviewed original ALS file, size, license and dataset citation. Terrestrial and processed files are excluded; AOI search remains unavailable for this source.
* Remove the personal Sila acknowledgement from the README; retain dataset attribution with the source.

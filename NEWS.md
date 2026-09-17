# alsdownloader 0.1.0.9001

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

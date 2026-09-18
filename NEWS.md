# alsdownloader 0.1.0

* Introduce an R package and Shiny application for aerial laser scanning discovery and original-file downloads.
* Support USGS 3DEP searches and local OpenTopography TileIndex archives.
* Add a rotatable public-domain globe and a source catalogue distinguishing implemented adapters from reference portals.
* Support native AHN6 and swissSURFACE3D tile searches and maintainer-approved contributor GeoJSON indexes.
* Ship source, dependency, licence and access-check tables inside the installed package; block downloads and remote previews when licence or attribution metadata are missing.
* Provide bounded point-cloud previews and contrasting visual overlays on black, with 100 m to 1 km square sides. Draw a transect to view both sampled clouds in profile and export cloud, profile or combined PNG figures without calculated changes.
* Represent acquisition intervals by their final collection date; never infer acquisition from publication or filename dates.
* Collect ten concise contributor fields, including a private contact email and a dataset DOI. Gate request preparation on successful connection checks, without transferring point-cloud files.
* Include source attribution, third-party notices and observed approval-time reporting.
* Remove PNOA LiDAR (Spain, failed anonymous download-init check) and OpenTopography AUS11_Victor (Australia, no supplied reuse licence) from the catalogue; access was not guaranteed for either.
* Add an in-app AOI adapter for IGN LiDAR HD (France): AOI search via a public STAC catalogue maintained by UMR TETIS / INRAE, original COPC LAZ download from data.geopf.fr, real per-tile acquisition dates. Built from previously verified live evidence; pending its own live/CI re-validation before relying on it in production.
* Distinguish in-app download adapters from portal-only reference sources by colour on the globe and map: red for countries with an implemented adapter, yellow for countries with only a linked official source the user must visit directly. Choosing a country (by click or from the sidebar list) still shows its official links either way.
* Redesign campaign comparison as two side-by-side panels (A, B) sharing one synced camera, instead of one canvas with both clouds overlaid. Drawing a profile line in either panel places the same line and strip in both automatically; the elevation profile below remains a single shared chart.
* Mention IGN LiDAR HD in the package Description. Add CITATION.cff and CONTRIBUTING.md. Refine interface polish (rounded controls, smoother hover/focus transitions, subtle depth) without changing layout or the colour palette.
* Replace two stray RStudio project files left over from early working names (lidar_app.Rproj, ot_pc_app.Rproj) with a single alsdownloader.Rproj.
* Reorder the README walkthrough as five numbered steps: explore sources, search/download, point cloud 3D viewer, compare campaigns, submit your dataset; use existing real screenshots (interface-preview.png, interface-submit-source.png) for the last two steps instead of placeholders.
* Add an in-app AOI adapter for CanElevation (Canada): a locally supplied NRCan project/tile .gpkg/.shp index (no live spatial API confirmed, unlike France), original COPC LAZ download from the confirmed public canelevation-lidar-point-clouds S3 bucket. Follows the existing OpenTopography local-index pattern. Built from previously verified live evidence; pending its own live/CI re-validation before relying on it in production.

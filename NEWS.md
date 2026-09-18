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

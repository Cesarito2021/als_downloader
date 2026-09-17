# Informational coverage map — next stage

Implemented: a rotatable Natural Earth welcome globe, Esri RGB/terrain basemap switching, translucent search footprints and selected-tile navigation to the shared 3D preview, with Viridis/Magma colors and automatic landscape fitting. Red country tint has opacity 0.55 and represents catalog presence only.
The acquisition-year coverage map and summary-grid features below remain proposed.

The application now provides a persistent **Submit a data source** form for community suggestions,
with email/GitHub drafts and a text export. It connects to existing deposits; it does not host point clouds.
The next map stage should expose a product information card with title, producer, repository, acquisition
platform and interval, DOI/paper/preprint, license, access conditions and reviewed coverage.
Keep research publication links distinct from actual downloadable datasets. A proposed record must pass
review before it becomes a map entry, and a mapped entry needs a working AOI adapter before downloads are enabled.
Small study areas can intersect large source tiles: show known file sizes before transfer and retain the existing hosted limits.

Prefer native provider footprints whenever available: they preserve actual survey/tile geometry and identifiers.
Canada exposes project and tile indexes; Swiss STAC exposes item geometry; AHN documents version-dependent sheet grids;
Poland documents WFS indexes and WMS file lookup. Each source still needs an adapter and real AOI tests.

A separate summary grid can help explore dense coverage. Its cells would summarize intersecting verified footprints,
not assert full-cell coverage. Clicking a cell should report source datasets, acquisition date ranges, distinct tile count,
known/unknown dates and an explicit coverage measure. Overlapping tiles and repeat surveys must not be counted as new area.
An equal-area projection or geodesic area calculation is necessary; a degree grid does not have constant cell area.

Use a dark background and a Turbo-style blue-to-red acquisition-year palette with a visible numerical legend.
Unknown acquisition dates need a separate neutral style. Multi-year acquisitions should expose their full interval,
even if one explicitly labelled summary year controls color. Publication dates and upload timestamps are not acquisition dates.
Offer a perceptually ordered alternative for accessibility and scientific figures.

Exploration remains informational. A click should not start a download or silently replace the AOI.
Users still draw or upload a study area, run a provider search and select files before downloading.
National outlines, regional bounding boxes and research-network membership must never become simulated ALS coverage.

Before implementation: normalize acquisition metadata, distinguish ALS/TLS/UAV-LS/photogrammetry,
establish index refresh rules and test national adapters. The source audit is the evidence baseline.

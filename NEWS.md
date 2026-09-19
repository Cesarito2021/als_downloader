# alsdownloader 0.1.0

* Show absolute source elevations in comparison distributions, consistent with
  cloud legends and profiles, and respect the visibility of each cloud.

* Compare one source campaign with a user-uploaded LAS/LAZ cloud, with shared
  coverage, CRS and unit checks, bounded display samples and session cleanup.
  Retain the distinct-date gate for comparisons between two source campaigns.
* Repair degenerate vertices introduced by GeoJSON serialization of joined
  Zenodo coverage polygons; validate the serialized index before queuing.
* Include the original ALS Downloader banner in a compact PDF report header.

* Add a five-field Zenodo contribution form, automatic metadata retrieval,
  polygon/file mapping, a private deduplicated queue and explicit maintainer
  approval. Only approved coverage enters searches; no real records are bundled.

* Simplify the app to one PDF report download while retaining PNG figure exports.

* Automatically compose a centred satellite RGB map for PDF/HTML reports using
  the actual AOI and selected tile outlines, with scale and source credits.
  Offer the same map as a PNG; reject stale captures and incomplete imagery.

* Redesign session reports around key information, large figures and concise
  metadata-based conclusions. Include the package citation and OpenForest4D NSF
  acknowledgement; make technical download details an optional appendix.

* Offer PDF and HTML session reports with study-area footprints, reported dates,
  known storage in GiB, clearly labelled transfer-speed scenarios and source
  credits. Attach exported PNG views with their legends and attribution.

* Convert known horizontal and elevation units independently to metres for
  display, including international and US survey feet. Preserve source files
  and report uncertain units; allow documented user confirmation when needed.

* Put USGS 3DEP first in catalogue cards and explain the optional GeoJSON index example.
* Hide comparison controls when distinct acquisition periods are unavailable;
  reject duplicate source files and overlapping or unknown selected periods.
* Show preview transfer/reading progress, allow cancellation, and bound preview
  runtime. Explain large-area search limits and show incomplete-source errors.

* Clarify software versus dataset licences and include available source citations
  and licence links in map, point-cloud, comparison and profile PNG exports.

* Preserve source intensity and offer automatic classification/intensity/elevation selection, with all 13 existing and greyscale palette choices.
* Add camera-icon PNG exports for the AOI/tile map, single point cloud and elevation distribution; retain comparison/profile exports. Include legends and attribution, with an optional basemap-free map export.

* Default single-cloud previews to source classification colours, with an observed-class legend. Preserve class codes through sampling; use grey for unclassified or unavailable labels. Apply the same colours to the browser COPC prototype.

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
* Map every EU member state with a genuine official-portal link on the globe and map, even without an in-app adapter: add 20 portal-only (yellow) catalogue rows for Austria, Belgium (Wallonia and Flanders), Bulgaria, Croatia, Czechia, Denmark, Hungary, Ireland, Italy, Latvia, Lithuania, Luxembourg, Malta, Portugal, Romania, Slovakia, Slovenia, Spain and Sweden, each linking to a real official government or agency source page. Only Cyprus and Greece remain unmapped, since neither has a genuine official portal identified yet.
* Add docs/DATASETS.md, a by-continent index of the full catalogue (generated by tools/update_source_docs.py alongside the existing source tables), so the README no longer needs to list every country. Trim the README's Source examples section to its eight most representative entries, pointing to the new by-region index for the complete list.
* Rename "Submit a data source" to "Share your dataset" and rewrite its form in a warmer, more encouraging voice (intro text, field placeholders, validation messages) so contributing data feels approachable rather than bureaucratic. Add an optional "what kind of dataset is this" tag (country-wide, national/regional agency, or local/campus survey) that rides along in the request but is never required. All nine required fields and their validation rules are unchanged.
* Change the campaign-comparison default colours from light purple/pale yellow to red/blue for a clearer visual contrast between the two panels (still user-selectable; all previous palettes remain available). Regenerate the README's compare-campaigns screenshot with a denser, more realistic synthetic forest-canopy example (tree crowns over rolling terrain, not sparse random points) shaped like a thinned/shortened canopy in one half, standing in for a real pre/post-storm pair near northwest Apalachicola, FL until one is verified. Regenerate the README's globe screenshot at the app's actual default rotation (Canada, USA and South America), matching what users see on first load.
* Enlarge the profile chart's point markers (2x2 to 3x3 px, higher opacity) so the drawn-segment side view reads clearly as a point-cloud cross-section rather than faint scattered dots; no change to what is plotted (still raw sampled points, no fitted curves or calculated differences).
* Colour search-result tile footprints on the map by acquisition year (viridis, grey where the provider reports no date) with an automatic Leaflet legend, instead of one flat colour, so overlapping surveys from different years are distinguishable at a glance.
* Add `als_report()` and a "Download session report" button: a self-contained HTML summary (study area, tile count, known size, a per-provider breakdown and every tile's citation/licence) generated from an R Markdown template (`inst/report/session-report.Rmd`), guarded by `requireNamespace("rmarkdown")`. PDF is available by calling `als_report(..., format = "pdf")` directly in R when `tinytex` is installed; the in-app button always produces HTML so the interactive workflow never depends on a LaTeX installation. Content is limited to data the app already has (nothing inferred, estimated or fetched), and the report is written locally only.
* The welcome globe now rotates on its own (throttled to ~16 fps; stops the instant a user drags or presses an arrow key, and resumes on **Reset globe**), and shows a small red/yellow legend in its top-right corner instead of relying on the paragraph below it alone.
* Add a README banner (`docs/images/banner.png`): real crops of the actual globe and point-cloud screenshots (not stock imagery or a fabricated mockup) alongside simple original drone and download glyphs, telling the discover-collect-download story at a glance above the title.
* Declutter the welcome globe screen: the "Study area and downloads" sidebar (country selector, AOI upload, provider, dates, download controls) now only appears after **Open map** is clicked, instead of alongside the globe on first load. The globe itself is larger (up to 1100px, was 900px), the introductory heading and two paragraphs are gone (they repeated the page header), and the status line under the globe is a single short caption instead of a full paragraph. The standalone "Explore a country" dropdown is removed; clicking a country on the map (once opened) still shows its official links, exactly as before.
* Add a short mission blurb and "by Cesar Alvites" credit below the welcome globe, plus GitHub and Natural Earth links; enlarge and unify those link-style buttons (Reset globe, GitHub, Natural Earth) to one consistent, more legible size.
* Improve the session report (`als_report()`): carry the app's mission blurb at the top; add an optional `aoi` argument so, when the study area geometry is available, the report includes a simple map figure of the tile footprints (coloured by acquisition year) against the study area outline -- geometry only, no basemap fetched or transmitted; round the by-provider and per-tile size figures to 2 decimals (tile sizes now shown in MB rather than raw bytes); add a small fixed "ALS Downloader" badge in the page corner. All figures are derived only from data the report already had.
* Fix the mission blurb so it also appears in `als_report(..., format = "pdf")`: it was written as raw HTML, which pandoc silently drops when targeting `pdf_document`, so PDF reports were missing it even though the map figure and tables (ordinary code-chunk output) already worked in both formats. It is now a plain Markdown blockquote, rendered identically to a styled quote in HTML and a normal quote block in PDF. The page-corner badge stays HTML-only, since a fixed-position element has no PDF/print equivalent.
* Re-add the Compare campaigns elevation-density panel (two overlaid histograms, red A / blue B, with each campaign's point count and mean elevation) that had been reverted together with an unrelated change; it is independent of the Explore-screen search/provider changes.
* Add Paisagens Sustentaveis LiDAR (Embrapa), a real Brazilian government map-based catalogue of LiDAR and forest-inventory surveys by state and year, with per-site files hosted on Embrapa's own Redape (Dataverse) repository including a DOI, citation and checksum. Listed as portal-only (no bulk area-of-interest search API confirmed, licence not confirmed per dataset) alongside the existing ORNL Brazilian forest surveys entry.
* Remove the "Search provider" dropdown: **Find intersecting tiles** now searches every source usable in the current deployment automatically (all network-based adapters, plus OpenTopography, contributed indexes and CanElevation once a local tile-index directory is supplied), reporting per-search how many sources returned tiles and which, if any, were unavailable. Replace the standalone AOI upload control with a "Draw on the map" / "Upload a file" choice, so only one method is shown at a time.
* Fix a real bug where completing a drawn polygon or rectangle could snap the map back to a corrupted or wildly oversized study area instead of zooming to the shape just drawn: the drawn/edited GeoJSON is now converted to an `sf` polygon directly from the browser event, instead of round-tripping through `jsonlite::toJSON(auto_unbox = TRUE)` and `sf::st_read()`, which could silently collapse a coordinate ring under certain shapes.
* Add a **Reset explorer** button (subtle red, transparent) that clears the current study area, search results and any in-progress download, and restores the local output directory, worker count and draw/upload choice to their defaults -- independent of the intro screen's **Reset globe**, which only resets the globe's rotation.
* Default the local output directory to the system temporary directory and the download worker count to 4 (still capped by the machine's actual limit), so a first-time user does not need to choose either before downloading.
* Remove the sidebar's "click a country to see its links" panel; a country's official source link is still available from the Sources and access tab's table. Clicking a country on the map still zooms to it.
* Reword the always-visible country red/yellow map layer's caption to be explicit that it shows country-level source availability before a study area is drawn, not confirmed survey coverage at a specific site; a true tile-level pre-draw coverage indicator (only truly known where a local tile index is supplied) remains a follow-up, since most in-app adapters only return coverage for an already-submitted area.
* Fix the map sidebar's open/closed layout so it no longer depends on the CSS `:has()` selector matching another element's exact inline `style` string -- behaviour that turned out to vary with the browser's effective zoom level. The server now tells the page directly whether the map is open, and the page toggles a plain CSS class in response.
* Rename "Study area" to "Area of interest" throughout, and split the single **Reset explorer** button into two: **Reset area of interest** (clears only the drawn/uploaded area and its results) next to the area-of-interest controls, and a general **Reset** at the bottom of the panel (also restores the output directory, worker count and download job state to their defaults). Group the panel into visually separated sections (area of interest / search / download), rename **Find intersecting tiles** to **Find ALS data**, and give it and **Download selected tiles** a distinct dark blue so the action buttons read clearly apart from the red reset buttons.
* Move the "Approved tile-index directory" path -- only meaningful for OpenTopography, contributed or CanElevation indexes a maintainer has placed on disk -- into a collapsed "Advanced" section, since most users never need to see or understand it.
* Fix the search results' "Acquisition year" map legend, which could render blank: it used a continuous colour scale (`colorNumeric`) over the reported years, which degenerates when a search returns only one distinct year or only undated tiles. It is now a discrete scale (`colorFactor`) with one swatch per year actually present, plus "Unknown" for undated tiles. Each year is also now its own Leaflet layer, so the map's existing layer-toggle control can show or hide one acquisition year at a time instead of only the whole result set together.
* Raise the point-cloud preview size ceiling from 200 MB to 1 GB, for both a remote result's "Plot selected tile in 3D" and a locally uploaded LAS/LAZ file, since the previous limit rejected many real tiles outright before the existing point-count decimation ever ran. Raise the Compare campaigns per-campaign/per-tile budget the same way, from 200 MB/100 MB to 600 MB/300 MB.
* Show the real error when a tile or local-file preview fails, instead of one fixed generic message ("check provider access, known file size..."). The actual reason (an oversized file, a missing LAS point count, a failed download, a missing `lidR` install, etc.) is now shown directly, with any signed request URL's query string stripped first.
* Fix "Plot selected tile in 3D" and the local-file preview failing with `could not find function "require_data_terms"` in a background process: they called their reader functions as plain closures passed into `callr::r_bg()`, whose captured environment does not reliably carry the rest of the package namespace into the background process, so an internal helper the reader called by name could fail to resolve there even though the reader itself started running. `preview_remote_tile()` and `read_forest_preview()` are now exported and called by namespace-qualified name inside the background process instead, the same way `download_tiles()` already was.
## Development review, 19 September 2026

- Resolve all application background tasks against the installed package or
  current development source, fixing preview helper lookup failures.
- Correct signed URL redaction in error messages.
- Add real subprocess regression tests and an isolated, bounded browser COPC
  prototype under tools/copc (not part of the installed Shiny application).

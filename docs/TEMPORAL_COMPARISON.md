# Visual comparison of two overlapping point clouds

Comparison is **visualization only**, with a maximum overlapping area of **1 km²**.
It does not calculate changes, differences, statistics or analysis exports.

1. Draw or upload a study area and search the provider catalog.
2. If the same AOI has at least two campaigns, open **Compare campaigns**, tick **I want to compare two point clouds in this study area**, and choose two distinct campaigns. Nothing is selected or loaded automatically. Provider dates are displayed unchanged.
3. Choose a square side: **100 m** (default), **250 m**, **500 m**, or **1 km**. Choose **View overlapping clouds**. A window is placed inside the largest shared footprint and clipped to shared coverage and the AOI; near boundaries its visible area can be smaller. Draw a smaller AOI in Explore to choose the location. The automatic location is not guaranteed to be ecologically representative.
4. Two side-by-side panels appear: A in **light purple** (left) and B in **pale yellow** (right), on black. Red and blue are also available. Choose the earlier/later ordering from provider metadata. The panels share one camera: rotate or zoom in either panel and both move together, so the same viewpoint compares both clouds. Hide either cloud independently with the Show A / Show B controls.
5. Click **Draw profile line**. Both panels switch to plan view together. Click start and end in either panel, or drag, in any direction; the same line and strip are drawn in both panels automatically. A single horizontal profile opens immediately underneath, combining sampled points from both campaigns. Adjust the strip width (default 2 m) to include points near the line. The profile shows distance along the line and original elevation, using the same campaign colours and visibility controls. It uses the displayed sample, not the full-resolution cloud; gaps are left empty and missing campaigns are identified. No curves are fitted or differences calculated. Escape stops drawing; Clear profile removes the section from both panels. Loading another AOI/campaign pair clears it.
6. Download the cloud figure (both panels stacked), profile figure or both as PNG. Exports include campaign labels, colours, framing and strip coordinates. They contain rendered figures only, with no point-data or analytical export. Profile elevation axes are not multiplied by the cloud's vertical exaggeration.

Use **Return to 3D** to restore the oblique camera in both panels while keeping the profile. The line and strip are shown in plan view in both panels. [Exported example from two Utah surveys](images/interface-compare-campaigns.png); nearly coincident points are not evidence of a quantified change.

The app rejects empty overlap before downloading comparison files. Only the viewing window is displayed.
The visualization button remains disabled until the user opts in and selects an eligible pair. With more than two campaigns available, the user chooses which two to view. Opting out clears the view and cancels any comparison worker.
A larger study area is accepted: comparison is cropped to the selected window. Source files may still need to be downloaded in full temporarily before cropping.
Provider footprints may include internal gaps: their intersection does not certify that every location has points in both surveys.
No overlap is fabricated from country outlines or by moving either cloud. If either cloud has no points within the overlap, visualization stops.

Both clouds use one coordinate origin and camera. Their embedded projected CRS must match and use metres;
the app does not register clouds or convert horizontal/vertical datums. Dates are not independently verified.
There is no denoising, height normalization, difference grid, change classification or statistical output.

## Resource limits

- At most four source tiles and 200 MB per campaign; at most 100 MB per tile.
- At most 50 million source points per tile and two million retained overlap points per campaign.
- At most 50,000 display points per cloud; thinning affects the view only.
- Source files are downloaded temporarily and removed after completion, cancellation or session exit.

These limits can require a smaller area even when overlap is below 1 km².
The normal download workflow remains separate and saves the original provider files.

Earlier validation records referring to P95 differences document a superseded feature, which has been removed.

The comparison camera fits the central 98% of projected display coordinates by default, so isolated extreme returns do not compress the entire view. This changes camera framing only: no source or sampled points are deleted. Turn off **Focus camera on central 98%** to fit all points.

# Visual comparison of two overlapping point clouds

Comparison is **visualization only**, with a maximum overlapping area of **1 km²**.
It does not calculate changes, differences, statistics or analysis exports.

1. Draw or upload a study area and search the provider catalog.
2. If the same AOI has at least two campaigns, open **Compare campaigns**, tick **I want to compare two point clouds in this study area**, and choose two distinct campaigns. Nothing is selected or loaded automatically. Provider dates are displayed unchanged.
3. Choose a square side: **100 m** (default), **250 m**, **500 m**, or **1 km**. Choose **View overlapping clouds**. A window is placed inside the largest shared footprint and clipped to shared coverage and the AOI; near boundaries its visible area can be smaller. Draw a smaller AOI in Explore to choose the location. The automatic location is not guaranteed to be ecologically representative.
4. View the first/earlier cloud in **solid grey** and the latest in **solid black**, on a white background. The user chooses this ordering from provider information. Rotate or zoom, change colours if desired, and show/hide either cloud. Solid colours identify the clouds; they do not encode calculated change.

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

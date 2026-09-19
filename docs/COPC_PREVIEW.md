# Remote preview decision and next steps

Reviewed 19 September 2026.

The main app's preview defect was background namespace resolution. All five
background entry paths now use one task dispatcher that loads the parent
package's development source or installed namespace before looking up a task.
This includes download, local/remote preview, comparison and source preflight.
Error redaction also now removes complete signed query strings and fragments.

## Display and analysis are separate workflows

1. A confirmed COPC asset with browser CORS/range support can supply a bounded
   coarse preview directly to the browser. See the working
   [prototype](../tools/copc/README.md). No R point-cloud download is involved.
2. Ordinary LAS/LAZ needs the existing temporary download and reader-side
   sampling, currently capped at 1 GiB. ZIP sources must be extracted locally.
3. Scientific comparison, CHM/DTM, classification and height normalization
   must read the relevant full-resolution region, check CRS/vertical datum,
   and record provenance. A display octree sample is not a substitute.

COPC reduces transfer; it does not eliminate transfer. It also does not
replace a spatial catalogue: tile discovery still needs footprints/indexes.
OpenTopography index provisioning remains a separate deferred task.

## Evidence and scope

| Source | Current evidence | Decision |
|---|---|---|
| USGS 3DEP / Planetary Computer | COPC catalogue; live signed-URL browser preview passed | First app-integration candidate |
| IGN LiDAR HD | Adapter selects `.copc.laz` assets | Browser CORS/ranges still need live validation |
| CanElevation | [Official specifications](https://canelevation-lidar-point-clouds.s3-ca-central-1.amazonaws.com/pointclouds_nuagespoints/CanElevation-LiDARPointClouds_products_specs_EN.pdf) describe COPC | Validate each asset and browser response; local spatial index still required |
| NOAA Florida sample | Live browser COPC preview passed | Demonstration only; no new provider adapter implied |
| OpenTopography | [Official FAQ](https://opentopography.org/faq-page) describes bulk LAS/LAZ access; no blanket COPC guarantee | Inspect each asset, not the portal name |
| AHN6 / swissSURFACE3D | Current adapters accept ordinary LAZ / LAS ZIP respectively | Retain download reader unless COPC structure is confirmed |
| Contributed / portal-only sources | Heterogeneous formats and access rules | No global streaming claim; per-asset verification required |

[GeoLibre](https://github.com/opengeos/GeoLibre) is a broader cloud-native GIS
reference. Its [R integration](https://github.com/opengeos/geolibre-r) is worth
evaluating separately. No GeoLibre code or account is needed for the current
prototype, which uses [copc.js](https://github.com/connormanning/copc.js).

Before promoting the prototype into Shiny: add a worker-thread decoder,
camera/AOI-driven node selection, cancellation/session lifecycle integration,
source attribution in the viewer, and a distributable JS/WASM source/licence
strategy suitable for CRAN. Do not silently download a full tile on a CORS
failure. The current prototype intentionally stays outside the R package.

The globe Read more/author design and analytical products remain later work;
the author section still needs the maintainer's reference site.

# Explorer coverage masks

Explorer has two access masks over its RGB basemap:

* **In-App Access (red):** source survey and tile footprints, supplemented by
  configured local and approved contributor indexes. This is coverage geometry,
  not a point marker or a national bounding box.
* **External Access (yellow):** country-level links to other providers. The
  country shape identifies the access route, not complete LiDAR coverage.

The welcome globe is illustrative: country colours represent access routes.
It is deliberately separate from Explorer's regional survey geometry.

## Snapshot sources

The bundled snapshot is retrieved from public indexes, not generated from point
clouds. It loads without remote coverage requests on application startup. The
snapshot is not a guarantee that every asset remains available or that every
acquisition period is represented. Search verifies the current file catalogue.

Snapshot retrieved 19 September 2026: 2,279 USGS project footprints, 361 Canadian
project footprints, 14,189 AHN6 tile footprints, 507,573 IGN tile footprints and
56,841 swissSURFACE3D tile footprints. Adjoining European tiles are dissolved
for display; these counts describe input index records, not unique acquisitions
or guaranteed downloadable assets.

| Source | Coverage geometry and attribution |
|---|---|
| USGS 3DEP | [Hobu public EPT survey boundaries](https://github.com/hobuinc/usgs-lidar/blob/master/boundaries/resources.geojson), linked from the [USGS AWS registry](https://registry.opendata.aws/usgs-lidar/). US Government Public Domain. The overview uses this public project inventory; the app searches Planetary Computer COPC, whose inventory may differ. This distinction appears in the popup. |
| CanElevation | [NRCan LiDAR Projects](https://maps-cartes.services.geo.ca/server_serveur/rest/services/NRCan/lidar_point_cloud_canelevation_en/MapServer/0). Contains information licensed under the [Open Government Licence – Canada](https://open.canada.ca/en/open-government-licence-canada). Individual tiles are queried from layer 1 on AOI search; a regional local file is no longer required by the app. |
| AHN6 | Native OGC feature collection linked by the [AHN dataroom](https://www.ahn.nl/dataroom). Only the available AHN6 point-cloud index is used. [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/). |
| IGN LiDAR HD | [UMR TETIS / INRAE STAC catalogue](https://api.stac.teledetection.fr/collections/lidarhd), retaining survey identifiers and source-edition information. Producer: IGN; [Licence Ouverte 2.0](https://www.data.gouv.fr/pages/legal/licences/etalab-2.0). |
| swissSURFACE3D | [Official STAC collection](https://data.geo.admin.ch/api/stac/v1/collections/ch.swisstopo.swisssurface3d). Source: Federal Office of Topography swisstopo. [OGD conditions](https://www.swisstopo.admin.ch/en/terms-of-use-free-geodata-and-geoservices). |
| Configured OpenTopography / contributed indexes | Use the supplied polygons and their dataset-specific licences. No new Zenodo record is approved by creating this mask. |

Country outlines for the illustrative globe and yellow access mask are from
Natural Earth / World Atlas; see [NOTICE](../NOTICE).

## Processing and limits

The maintainer build script is `tools/build_discovery_coverage.R`. It reads fully
paginated source indexes, combines adjoining European tile footprints, and
generalizes geometry with a 50 m tolerance while preserving topology. Source
project boundaries are retained for USGS and Canada. The operation does not
replace separated regions with a single bounding rectangle. This is a navigation
overview; original tile geometry is used for searches and downloads.

The Canadian project service additionally generalizes its WGS84 response at
0.001 degrees. These display outlines must not be used for surveying or exact
area measurement.
Residual spherical ring crossings are repaired at 0.0001-degree precision
(up to approximately 11 m), only where needed for valid display geometry.

Coverage credits and licence links are included in map-image exports. The source
snapshot date is shown in feature popups. Basemap rights are separate and remain
subject to the Esri review in [USE_REVIEW.md](USE_REVIEW.md).

The direct AHN GeoPackage host returned a certificate-expiry error during this
review. TLS verification was not disabled: the mask uses the existing native
OGC catalogue with valid HTTPS access. File downloads remain subject to the
asset host's availability and TLS validity.

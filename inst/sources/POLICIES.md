# Dataset licences and access policies

Policy review: 17 September 2026. This is a source-based summary, not a legal
opinion or a guarantee of clearance. Licences apply to particular datasets and
versions, not automatically to every product of a country or repository.
COPC/LAS/LAZ are formats, not permissions. Public access and reuse rights must
be checked separately. Service limits still apply to openly licensed data.

| Source / product | Policy evidence | Reuse and obligations / review status |
|---|---|---|
| USGS 3DEP | [Official point-cloud collection](https://catalog.data.gov/dataset/lidar-point-cloud-usgs-national-map-3dep-downloadable-data-collection) identifies all 3DEP products as public domain. | Not limited to scientific use. Preserve survey provenance; do not extend this finding to unrelated third-party content on USGS websites or imply endorsement. |
| swissSURFACE3D | [swisstopo OGD terms](https://www.swisstopo.admin.ch/en/terms-of-use-free-geodata-and-geoservices) | Use and distribution, including commercial use, permitted with required swisstopo attribution. Excessive service use can be restricted. |
| AHN6 | [AHN6 release and licence](https://www.ahn.nl/eerste-deel-ahn-6-beschikbaar) | **CC BY 4.0**, not an assumed CC0 inherited from an older AHN release. Credit the source, link the licence and indicate modifications. Commercial use permitted under the licence. |
| Spain PNOA (removed from active catalogue 18 Sep 2026; see [ACTIVE_SOURCES.md](../../docs/ACTIVE_SOURCES.md)) | [IGN/CNIG licence](https://www.ign.es/resources/licencia/Condiciones_licenciaUso_IGN.pdf) and [checked tile](https://centrodedescargas.cnig.es/CentroDescargas/detalleArchivo?sec=11970508) | CC BY 4.0 framework; preserve the product-specific producer/date credit. IGN specifies visible attribution on maps/images, with limited alternatives when justified. Commercial use is not generally excluded; download access remains unvalidated (anonymous download-init returned HTTP 403). |
| France IGN LiDAR HD | [Dataset licence](https://www.data.gouv.fr/datasets/nuages-de-points-lidar-hd), [Licence Ouverte 2.0](https://www.data.gouv.fr/pages/legal/licences/etalab-2.0) | Commercial and noncommercial reuse permitted. Acknowledge source and the reused information's last update. That update date is an attribution field, not the acquisition date. Observe the download API's limits. |
| Canada CanElevation | [Product record](https://open.canada.ca/data/en/dataset/7069387e-9986-4297-9f55-0288e9676947), [OGL Canada](https://open.canada.ca/en/open-government-licence-canada) | Commercial and noncommercial reuse permitted for information covered by the licence. Preserve specified attribution; licence excludes unlicensed third-party rights, personal information and official symbols. |
| Luxembourg ACT LiDAR 2019 | [Official record, CC0](https://data.public.lu/en/datasets/lidar-2019-releve-3d-du-territoire-luxembourgeois/) | No scientific-use-only restriction identified in the record. Preserve producer and source for provenance. Public individual downloads do not authorise bypassing the separate API-key bulk workflow. |
| Wallonia SPW cloud grid | [Official index record](https://geoportail.wallonie.be/catalogue/7341def8-8ea4-4031-9b2a-4a31f1954d05.html) | Record identifies CC BY 4.0. Credit SPW. Index access is verified; point-file delivery is not yet verified. Check the selected cloud distribution's notices before integration. |
| Auckland_2013 | [Dataset metadata](https://portal.opentopography.org/datasetMetadata?otCollectionID=OT.062016.2193.1) | Acknowledgement explicitly specifies CC BY 3.0 New Zealand and different Auckland Council credit wording for copies and derived works. The separate Use License field says Not Provided: retain this discrepancy and original acknowledgement, do not silently label the record CC BY 4.0. |
| BR17_SaoPaulo | [Dataset metadata](https://portal.opentopography.org/datasetMetadata?otCollectionID=OT.062020.31983.1) | Provider lists GNU GPLv3. Do not relabel the data CC BY or infer data permissions from the package's own GPL licence. Redistribution/derivative obligations require product-specific review. |
| AUS11_Victor (removed from active catalogue 18 Sep 2026; see [ACTIVE_SOURCES.md](../../docs/ACTIVE_SOURCES.md)) | [Dataset metadata](https://portal.opentopography.org/datasetMetadata?otCollectionID=OT.062013.28354.1) | **Use License: Not Provided.** Access test passed, but open reuse permission is unresolved. Not cleared for the approved open-download collection; a paper citation or disclaimer does not resolve this gap. |
| Other catalogue entries | See [complete catalogue](SOURCES.md) and each official provider link. | No blanket reuse clearance. Unreviewed, authenticated, paid or research-only products must not be presented as approved anonymous open downloads. |

## Meaning for users and the maintainer

[CC BY 4.0](https://creativecommons.org/licenses/by/4.0/) permits commercial
reuse as well as research. It requires appropriate credit, a licence link and
identification of modifications; it does not allow imposing additional legal
restrictions on the licensed material. Other rights may remain applicable.
Do not add a blanket "scientific use only" restriction to these data or to the
GPL-licensed software. A scientific audience is a purpose, not a new licence.

Users must consult the original dataset's conditions for their intended use,
including commercial reuse, redistribution and publication. The platform must
also comply with applicable conditions for its own downloads, displays and
exports. A disclaimer or routing downloads through the original server does
not automatically eliminate the maintainer's responsibilities.

Keep source/DOI/licence references with downloads and preserve any required
on-image attribution. If a dataset explicitly restricts use to research or
noncommercial purposes, do not treat it as unrestricted open data. Under this
project's current admission policy it stays outside the approved automatic
download collection. Conflicting or absent permissions remain pending rather
than being resolved by an assumption of public availability.

For jurisdiction-specific liability or final release wording, seek review by
the institution's legal or research-data office. This document does not claim
to waive liability or modify any provider's licence.

# Sources, credits and access conditions

This folder is distributed with the installed R package. It records source
links and software dependencies without bundling or rehosting point clouds.

- [Complete source table](SOURCES.md): all catalogue entries, official links,
  access conditions and implementation status.
- [Datasets by region](../../docs/DATASETS.md): the same catalogue, grouped by
  continent, without the full access/licence text - for browsing at a glance.
- [Dataset policies](POLICIES.md): official licence references, reuse conditions
  and unresolved permissions; file accessibility is not legal clearance.
- [R dependency table](DEPENDENCIES.md): required and optional libraries and
  their official package records.
- [Live European checks](VALIDATION.md): France, Spain, Luxembourg and Wallonia,
  including failed requests and explicit limits of validation.
- [Third-party notices](../NOTICE): bundled cartography and basemap credits.
- [Machine-readable catalogue](../extdata/providers.csv): the same catalogue
  returned by `alsdownloader::provider_catalog()`.

From an installed package:

```r
system.file("sources", "README.md", package = "alsdownloader")
alsdownloader::provider_catalog()
```

## Data-use policy

The software licence does not license external data. A paper citation, a
working URL or a free download is not a substitute for the data provider's
permission and reuse conditions. Admit automatic downloads only through
authorised anonymous routes for explicitly open-licensed aerial laser data.
Retain each dataset's source, licence, DOI where supplied and required credit.
Never bypass identification, credentials, payment or service limits.

Catalogue entries marked portal-only, pending or requiring authentication
are references, not approved automatic downloads. An implemented generic
adapter does not approve every dataset that it can technically read.
Unresolved dataset permissions require review before inclusion in the approved
download collection. This catalogue is not a blanket legal clearance.

Downloads go from the provider to the user's selected destination; the package
does not contain a mirror of those data. Keep the accompanying selection
metadata and CITATIONS file with downloaded data. Publications and shared
figures must also retain applicable data and basemap credits; this folder alone
does not replace attribution required on an individual figure or dataset.

## Switzerland: reading is distinct from uploading

The swissSURFACE3D connector queries public STAC records and downloads public
assets without a user account or personal API key. The provider's
[authentication documentation](https://docs.geo.admin.ch/download-data/stac-api/authentication.html)
describes authentication for **write** operations. The package does not perform
those operations.

[swisstopo terms](https://www.swisstopo.admin.ch/en/terms-of-use-free-geodata-and-geoservices)
permit use, distribution and commercial use of its free geodata with mandatory
source credit. The connector records “Federal Office of Topography swisstopo”.
Service restrictions for excessive use still apply. If a public route becomes
restricted, stop using that route rather than circumventing the restriction.

Maintainers regenerate the two tables with `python tools/update_source_docs.py`
from the package root whenever `DESCRIPTION` or `inst/extdata/providers.csv`
changes. Neither table is an inventory of every survey tile worldwide.

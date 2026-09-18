# EU coverage tracker (all 27 member states)

Consolidates the country-by-country LiDAR access research already recorded
across `docs/EUROPE_ACCESS_REVIEW.md`, `docs/SOURCE_AUDIT.md`,
`docs/COVERAGE_AUDIT.md`, `inst/sources/VALIDATION.md`,
`docs/dataset-candidates.csv` and `docs/link-checks.csv`, into one table so
the same country is not re-investigated from scratch. This is a synthesis
of existing evidence, not a new live-verification pass: **this session has
no outbound network access beyond GitHub**, so nothing below marked
"candidate" has been (re)tested live today. All 27 EU member states already
had at least one prior investigation on record; there are no genuine
"no data at all" countries left in the EU.

## Status legend

- 🟦 **Done** - implemented in-app adapter, real anonymous file download confirmed (`inst/extdata/providers.csv`, `implemented=TRUE`). Do not re-investigate; extend or fix only if something breaks.
- 🟨 **No access** - actively investigated and closed: either a real negative (HTTP 403, registration/account required, paid-only route) or a screened search that found no qualifying open ALS chain. Do not re-investigate unless the provider publicly changes its terms.
- 🟧 **Candidate** - a real official portal/licence/index exists with some positive evidence, but no anonymous file download has been confirmed yet. Needs a live check (a real network session) before it can become 🟦 or 🟨.

**Summary: 2 done, 10 no access, 15 candidate.**

## Table

| Country | Status | Evidence | Source |
|---|---|---|---|
| France | 🟦 Done | IGN LiDAR HD: STAC search + anonymous COPC download confirmed live 17 Sep 2026 | `inst/sources/VALIDATION.md`, `public-access-checks.json` |
| Netherlands | 🟦 Done | AHN6: native OGC index + anonymous LAZ download, CC BY 4.0 | `inst/extdata/providers.csv`, `docs/EUROPE_INTEGRATION.md` |
| Luxembourg | 🟧 Candidate (strong) | CC0; official GeoJSON/Shapefile tile index downloaded and parsed (10,908 features); sample ZIP reachable (HTTP 206) | `inst/sources/VALIDATION.md` |
| Slovenia | 🟧 Candidate (strong) | GURS 2023-2025 national survey; public viewer/download, no registration (max 10 sheets/download); licence/API unconfirmed | `docs/EUROPE_ACCESS_REVIEW.md` |
| Belgium (Wallonia) | 🟧 Candidate | Public ArcGIS index confirmed (HTTP 200, real feature returned); no download URL in that feature yet, point-file transport untested | `inst/sources/VALIDATION.md` |
| Belgium (Flanders) | 🟧 Candidate | DHMV II: provider describes public airborne LAZ download; tile endpoint/licence/sample access not yet checked | `docs/EUROPE_ACCESS_REVIEW.md` |
| Poland | 🟧 Candidate | Official page reachable (HTTP 200, after a TLS certificate-chain retry); documents WMS/WFS download URLs; file explicitly untested | `docs/dataset-candidates.csv` |
| Estonia | 🟧 Candidate | Official elevation download service (select laser points + year); portal reachable (HTTP 200); file untested | `docs/link-checks.csv`, `docs/EUROPE_ACCESS_REVIEW.md` |
| Germany (Saxony only) | 🟧 Candidate | GeoSN: 2 km LAZ ZIP tiles with acquisition metadata documented; other 15 German states not reviewed at all; file untested | `docs/EUROPE_ACCESS_REVIEW.md` |
| Portugal | 🟧 Candidate | DGT catalogue includes point clouds; interactive basket with a 24-hour expiring download list; native mapping/limits unverified | `docs/EUROPE_ACCESS_REVIEW.md` |
| Ireland | 🟧 Candidate | OPW LiDAR release confirmed CC BY 4.0 (real open licence); GSI Phase-2 index described links deliver rasters, not point clouds - cloud route itself unresolved | `docs/EUROPE_ACCESS_REVIEW.md` |
| Italy (Tuscany / Emilia-Romagna only) | 🟧 Candidate | Regional surveys and footprint layers exist; anonymous original-cloud delivery not established; earlier Sila (Zenodo) record was reviewed and removed for lacking verified polygon coverage | `docs/EUROPE_ACCESS_REVIEW.md`, `docs/ITALY_PULETTI.md`, `docs/COVERAGE_AUDIT.md` |
| Latvia | 🟧 Candidate | LGIA classified ALS product with an open-data licence page documented; download/index mechanism untested | `docs/EUROPE_ACCESS_REVIEW.md` |
| Slovakia | 🟧 Candidate | GKU ALS via MAPKA; cloud sections capped at 4 km²; auth/licence/endpoint unconfirmed | `docs/EUROPE_ACCESS_REVIEW.md` |
| Denmark | 🟧 Candidate (auth likely required) | DHM point-cloud file service documented, but the Dataforsyningen API needs an account token - not confirmed anonymous | `docs/EUROPE_ACCESS_REVIEW.md` |
| Finland | 🟧 Candidate (scope-limited) | Open 0.5-point product exists, distinct from the paid 5-point product; no documented anonymous connector route confirmed yet | `docs/EUROPE_ACCESS_REVIEW.md` |
| Sweden | 🟧 Candidate (blocked pending terms) | Laserdata Skog page states CC0, but a 2026 provider update added account/acceptance conditions; delivery/account integration untested | `docs/EUROPE_ACCESS_REVIEW.md`, `docs/REVIEW_AND_OPTIONS.md` |
| Austria (Tirol only) | 🟧 Candidate (mostly blocked) | Tirol's original LAS/LAZ is request-and-pay only, not open; other 8 Austrian states not reviewed at all | `docs/EUROPE_ACCESS_REVIEW.md` |
| Spain | 🟨 No access | Official terms and tile polygon confirmed, but anonymous download-init returned HTTP 403 (real negative); removed from the active catalogue 18 Sep 2026 | `inst/sources/VALIDATION.md`, `docs/ACTIVE_SOURCES.md` |
| Lithuania | 🟨 No access | Registration required for the only described ordering route; no anonymous integration | `docs/EUROPE_ACCESS_REVIEW.md` |
| Romania | 🟨 No access | ANCPI LAKI-II is a LiDAR-*derived terrain model*, not raw point clouds - screened, does not qualify | `docs/EUROPE_ACCESS_REVIEW.md` |
| Malta | 🟨 No access | 2018 DSM derives from airborne LiDAR, but the original point-cloud download/index was never verified to exist publicly | `docs/EUROPE_ACCESS_REVIEW.md` |
| Bulgaria | 🟨 No access | National INSPIRE portal identified; screened, no qualifying original-cloud index/asset chain found | `docs/EUROPE_ACCESS_REVIEW.md` |
| Croatia | 🟨 No access | DGU access page and a LiDAR request category found; screened, anonymous cloud tile delivery unverified | `docs/EUROPE_ACCESS_REVIEW.md` |
| Cyprus | 🟨 No access | COASTLINE Zenodo research record describes aerial/UAV laser clouds, but lacks the required boundary/licence proof for admission | `docs/EUROPE_ACCESS_REVIEW.md` |
| Czechia | 🟨 No access | CUZK open-data policy exists, but the DMR5G product is terrain XYZ delivery, not original airborne point clouds | `docs/EUROPE_ACCESS_REVIEW.md` |
| Greece | 🟨 No access | Same COASTLINE Zenodo lead as Cyprus; screened, not admitted (no official national cloud tiles established) | `docs/EUROPE_ACCESS_REVIEW.md` |
| Hungary | 🟨 No access | Lechner Center describes ordered/contact-based elevation data delivery; no verified anonymous original ALS tile delivery | `docs/EUROPE_ACCESS_REVIEW.md` |

## What would move a 🟧 candidate to 🟦 done

For each candidate, someone with real network access needs to repeat the
same check already done for France/Canada/Luxembourg: request the actual
tile/index file, confirm it returns 200/206 anonymously with no login, and
confirm the file signature (LAS/LAZ magic bytes) - then, if positive, an
adapter can be written the same way `R/europe.R`/`R/canada.R` were. Priority
order by strength of existing evidence: **Luxembourg and Slovenia first**
(closest to confirmed), then Belgium-Wallonia, Poland and Estonia (official
index/portal already reachable), then the remaining candidates.

## What would move a 🟧 candidate to 🟨 no access

If the live check returns 403/401, requires an account, or the described
route turns out to deliver rasters/terrain models instead of original point
clouds (as already happened for Czechia and Romania), record that finding
here and move the row to 🟨 - don't leave it re-investigated indefinitely.

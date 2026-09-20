# OpenTopography verification — 2026-09-20

The default app now queries the audited hosted OpenTopography catalogue without
an administrator's personal TileIndex directory.

| Result | Count |
|---|---:|
| Global non-federated PointCloud catalogue records reviewed | 841 |
| Hosted collections | 502 |
| Hosted airborne-LiDAR collections | 476 |
| Integrated airborne-LiDAR collections | 428 |
| Distinct downloadable file URLs in those collections | 1,056,011 |
| Indexed polygon rows before file deduplication | 1,056,156 |
| Airborne collections retained as external access | 48 |
| Hosted collections outside airborne-LiDAR scope | 26 |
| Community Dataspace records retained as external links | 339 |

All indexed URLs were compared against the public object inventory, including
pagination and positive object sizes. Anonymous range requests checked the LAS
signature of a sample in every readable collection. This verifies the entire
link inventory, **not full decompression of every one of the million files**.

45 hosted airborne collections have no public ZIP at the documented TileIndex
path (HTTP 404); their official index routes redirect to sign-in. Another three
indexes repeat all files from differently described records:

- `IA14_Kumar_1064` repeats `IA14_Kumar_532` (186 files; dates also differ).
- `IL14_Kumar_1064` repeats `IL14_Kumar_532` (180 files; dates also differ).
- `CA14_Dietrich_G` repeats `CA14_Dietrich_A` (83 files).

The three ambiguous references are external-only rather than advertised as
independent campaigns. Their files remain searchable under the record matching
the source directory. Other coincident but distinct file URLs are not discarded.

## Complete-file tests

Live AOI search, full download, complete XYZ decoding, bounded preview and
checksum-verified resume passed for:

| Collection | Downloaded bytes | Decoded points |
|---|---:|---:|
| AUS11_Victor — Australia | 243,677 | 79,770 |
| BR17_SaoPaulo — Brazil | 302,799 | 94,761 |
| Auckland_2013 — New Zealand | 252,345 | 83,395 |
| CA24_Volcan — United States | 200,016 | 17,384 |

The full regression suite passed. Six report tests initially lacked the Pandoc
path; they also passed after using the existing local Pandoc installation.
OpenTopography-specific tests check index hashes, invalid links, all intersecting
campaigns, limits, spatial gaps, viewport clipping and agreement between packaged
access claims and the complete object audit.

Machine-readable evidence ships in `inst/extdata/opentopography-access-audit.csv`
and `inst/extdata/opentopography-verification.json`. Maintenance and scope are
documented in [OPENTOPOGRAPHY_ACCESS.md](OPENTOPOGRAPHY_ACCESS.md).

The final browser check used the complete default registry with no local index
folder: an Australian AOI returned one tile; selection and hosted download
reported **1 successful, 0 failed**, and the ZIP download reached the browser.

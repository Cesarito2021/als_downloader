# Zenodo integration: live checks, 19 September 2026

No deposit was created and no dataset was approved into the production catalogue.
Test proposals are stored only in a separate local review queue.

Submission policy: existing georeferenced coverage polygons and verified
polygon-to-file mapping are required. These are engineering checks, not a list
of ready-to-submit examples. Sabah remains a test fixture until its plot-to-cloud
coverage mapping is verified; it is not yet a fully qualified submission example.

| Record | Checks | Outcome |
| --- | --- | --- |
| [Sabah Biodiversity Experiment](https://doi.org/10.5281/zenodo.14917551) | Metadata, 14,363-byte zipped Shapefile, 124 plot polygons, explicit mapping to the 2013 and 2020 ZIPs, proposal generation, deduplication and pending-only storage | Passed after fixing nearly duplicate vertices created by polygon serialization. Two year-specific test proposals remain pending. |

For Sabah, HTTP range requests retrieved only 128 KiB from each archive's end.
Both returned HTTP 206 with the expected total sizes and central-directory LAZ
entries. This verifies links and archive listings, not complete point decoding,
checksums or within-file geographic coverage. The plot polygons are study plots,
not the full survey extent. No claim of complete tile coverage is made.

The original archives are approximately 4.02 and 4.05 GB; they were not downloaded
in full. Archive delivery requires full download and local extraction before 3D
preview. This record therefore does not demonstrate selective cloud streaming.

Source publication dates are not used as acquisition dates. Test proposals use
2013 and 2020 as explicitly stated by the depositor. Attribution and source
licence remain attached; a maintainer must still assess any additional source
acknowledgements and verify the coverage/file mapping before accepting them.

Remaining acceptance work: choose and approve a dataset after reviewing its
actual content and terms. Public deployment also needs persistent private queue
storage, restricted reviewer access and load/rate-limit testing. The small live
checks do not establish capacity for 100 submissions per day.

Historical excluded check: Brazil EBA record 7689693 was inspected for metadata
only. It is excluded from submission examples because no ready, validated
coverage/file mapping was available. No proposal from this record was approved.

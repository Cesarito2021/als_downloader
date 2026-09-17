# Running and hosting ALS Downloader

```r
alsdownloader::launch_app(mode = "hosted", tile_index_dir = "/srv/als/indexes")
```

The repository entry point accepts `ALS_MODE` and `ALS_TILE_INDEX_DIR`.
Multiple server processes must share `ALS_HOST_LOCK_DIR`; administrators configure storage quotas and cleanup.
This is not a distributed scheduler. Hosted batches use one worker, at most 10 tiles and 500 MB with known sizes;
uploads are limited to 200 MB. These are application controls, not universal Shiny limits.

Local mode recommends `min(10, max(1, available cores - 4))` workers. Effective concurrency also respects tile count
and the configured provider ceiling, initially two. Raise `provider_limit` only after checking service terms.
Background processes use `future.apply::future_lapply()` for transfers. Searches are synchronous;
large collections of local OpenTopography indexes may take time.

The optional Esri World Hillshade is external. Bundled country outlines work without it.
Terrain and country maps do not describe actual LiDAR coverage.

The active repository contains the current package application. Previous standalone scripts remain available in Git history.
# Temporary tile previews

The Explore preview accepts one selected LAS/LAZ result with a known HTTP file size up to 200 MB.
It downloads the complete source file temporarily and samples at most 100,000 points in a background process;
this is not a remote range/COPC streaming service. `lidR` is required. Temporary files are removed after processing or session shutdown.
In hosted mode, remote previews share the existing transfer lock with downloads; a session cannot begin a download and remote preview concurrently.
RGB imagery is requested directly by the browser from Esri and retains the map attribution and provider terms.
# Campaign comparison limits

The Compare campaigns tab uses a separate background worker and shares the hosted transfer lock.
It downloads sequentially, permits four tiles/200 MB per campaign (100 MB each) and clips to an AOI of at most 0.25 km².
Reader and accumulated AOI point limits and a grid-cell budget bound processing; display sampling does not replace the analysis points.
Cancelling, changing the AOI/campaign/grid, or closing the session stops the worker and removes its temporary directory.
Downloads and other previews in that session wait until comparison processing finishes or is cancelled.
[Scientific interpretation, eligibility and limits](TEMPORAL_COMPARISON.md).

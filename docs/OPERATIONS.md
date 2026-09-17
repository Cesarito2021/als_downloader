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

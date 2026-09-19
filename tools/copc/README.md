# Bounded browser COPC prototype

Developer prototype, excluded from the R source package by `.Rbuildignore`.
The Shiny preview continues to use the bounded temporary-download reader.

From this directory, install the lockfile with `pnpm install --frozen-lockfile`,
then run `node build.cjs` and `node test.cjs`. Serve the repository root with a
local static HTTP server and open `/tools/copc/index.html`. Do not open via
`file://`. Enter a public HTTPS COPC URL after checking its data terms.
Planetary Computer assets need a fresh signed URL; do not commit signed URLs.

The viewer reuses the package's existing canvas renderer. Dependencies are
pinned: copc 0.0.8 (MIT), laz-perf 0.0.7 (Apache-2.0, transitive), esbuild
0.25.9 (MIT, build only). Generated JS/WASM and node_modules are ignored.
Their original licences are supplied by the dependency packages. Sources:
https://github.com/connormanning/copc.js and https://github.com/hobuinc/laz-perf.

Limits: 16 MiB requested compressed bytes, 4 MiB per range, 128 requests,
750,000 decoded points, 100,000 displayed points, and a 60-second browser
timeout. HTTP 206 and exact response length are mandatory; visible
Content-Range headers must match. Some S3 CORS policies hide that header.
There is no whole-file fallback. Root hierarchy page only: no child-page
traversal, AOI clipping, camera-adaptive detail, analysis or derived rasters.
Cancellation aborts network reads; synchronous decoder work finishes its
current bounded node before the event loop can process cancellation.

Live headless Edge checks, 19 September 2026:

| Source | Displayed points | Requested bytes | Ranges |
|---|---:|---:|---:|
| NOAA 2017 Florida, `20170503_16RFU525135.copc.laz` | 100,000 | 6,056,795 | 10 |
| USGS Utah 2020, `USGS_LPC_UT_StatewideSouth_2020_A20_12SUH7021.copc.laz` | 100,000 | 1,155,372 | 11 |

Both rendered without JavaScript errors. Bytes exclude the local viewer/WASM.
These are sample-specific results, not a provider-wide performance guarantee.

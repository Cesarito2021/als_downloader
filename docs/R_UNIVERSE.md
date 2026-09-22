# Distribution, badges and metrics

## R-universe

The renamed package is `ALSdownloadeR`. The updated registry entry is kept in [tools/runiverse/packages.json](../tools/runiverse/packages.json); synchronize it with the [public registry](https://github.com/Cesarito2021/cesarito2021.r-universe.dev/blob/main/packages.json) and verify a successful build before advertising installation of the renamed package through R-universe. The earlier publication evidence below applies to the previous package name only.

**Publication verified on 17 September 2026:** [alsdownloader 0.1.0](https://cesarito2021.r-universe.dev/alsdownloader) is indexed and available, with 9 checks reported OK for source commit `93b143557098f255965860558fc8389070e73761` ([build](https://github.com/r-universe/cesarito2021/actions/runs/35279593002)). The official GitHub app is installed for Cesarito2021, limited to the registry and package repositories. R-universe provides an additional build/distribution channel; it does not confer CRAN acceptance or scientific peer review. [Official setup documentation](https://docs.r-universe.dev/publish/set-up.html).

After the renamed package is available, install with:

```r
install.packages("ALSdownloadeR", repos = c(
  "https://cesarito2021.r-universe.dev", "https://cloud.r-project.org"
))
```

Status endpoints for the renamed package:

- `https://cesarito2021.r-universe.dev/ALSdownloadeR/badges/version`
- `https://cesarito2021.r-universe.dev/ALSdownloadeR/badges/checks`

## What the badges measure

| Badge / metric | Meaning |
|---|---|
| R package checks | Latest GitHub workflow status on main; links to detailed results. Old failed runs remain in the audit history. |
| R, Shiny, web | Technologies used, not endorsements or certification. RStudio is an optional IDE, not an application dependency. |
| GPL-3 | Software license; source datasets retain their own terms. |
| CRAN | Currently not submitted. Activate a CRAN version/download badge only after acceptance and public availability. |
| GitHub stars | Repository interest, not downloads or citations. |
| Downloads | No release assets exist yet. GitHub release-asset counts would exclude clones, source ZIPs and `install_github()` installations. CRAN download statistics cover their reporting mirrors, not unique users. |
| Citation | Links to the package citation instructions. No verified scholarly citation count or package DOI is currently configured; a counter would require an identified publication and a named bibliographic source. |

Future download counters should label their source and scope separately. Never combine GitHub, CRAN and R-universe numbers into an unlabeled user count.

References: [GitHub release assets API](https://docs.github.com/en/rest/releases/assets), [CRAN download logs](https://cran.rstudio.com/), [CRANlogs API](https://github.com/r-hub/cranlogs.app).

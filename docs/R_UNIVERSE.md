# Distribution, badges and metrics

## R-universe

The [registry](https://github.com/Cesarito2021/cesarito2021.r-universe.dev/blob/main/packages.json) lists `alsdownloader` from its public GitHub repository. A copy is kept in [tools/runiverse/packages.json](../tools/runiverse/packages.json).

**Activation pending:** the account owner must install the [official R-universe GitHub app](https://github.com/apps/r-universe) for this account, allowing access to the registry and package repositories. After activation, inspect builds and the package page before advertising installation. R-universe provides an additional build/distribution channel; it does not confer CRAN acceptance or scientific peer review. [Official setup documentation](https://docs.r-universe.dev/publish/set-up.html).

Once deployment is confirmed, use:

```r
install.packages("alsdownloader", repos = c(
  "https://cesarito2021.r-universe.dev", "https://cloud.r-project.org"
))
```

Replace the pending badge with the deployed version and check-status endpoints:

- `https://cesarito2021.r-universe.dev/alsdownloader/badges/version`
- `https://cesarito2021.r-universe.dev/alsdownloader/badges/checks`

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

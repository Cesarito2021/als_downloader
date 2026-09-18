## Submission status

Draft only. This package has not been submitted to CRAN.

## Local test environment

Windows 11, R 4.4.0 (x86_64-w64-mingw32).

## R CMD check results

The full --as-cran run, including PDF and HTML manuals, reports 0 errors, 0 warnings and 2 notes:
- New submission.
- Unable to verify current time in the local environment.

[GitHub Actions run 35406911944](https://github.com/Cesarito2021/als_downloader/actions/runs/35406911944) (commit `cd2f4d1`, 18 September 2026) passed `--as-cran` with warnings treated as failures on Windows, macOS, Ubuntu-release and Ubuntu-devel - the current head of this branch, not a historical revision. This run also caught and fixed a real `R CMD check` ERROR (a stale test asserting the wrong set of implemented providers) that every prior run on this branch had been failing on; see docs/CRAN_READINESS.md for that fix and the remaining maintainer confirmations (an actual submission still needs the maintainer's own CRAN account/action). Examples and tests do not make network requests; the app and network workflows run only when explicitly invoked.

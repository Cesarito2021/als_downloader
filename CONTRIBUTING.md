# Contributing

Thanks for your interest in ALSdownloadeR.

## Code contributions

- Open an issue before a large change, so the approach can be agreed first.
- Run `devtools::document()`, `devtools::test()` and `R CMD check --as-cran`
  locally before opening a pull request; CI (`.github/workflows/R-CMD-check.yaml`)
  re-runs the same check on Windows, macOS and Linux (release and devel).
- Keep new network-dependent code behind an explicit function argument or
  `if (interactive())` in examples; CRAN checks must not require internet
  access or credentials.
- Match the existing style: base R plus the packages already listed in
  `DESCRIPTION`, no new hard dependencies without discussion.
- Add or update `testthat` tests for any behaviour change.

## Data source contributions

Suggesting a new LiDAR provider is a separate, non-code process: see
[docs/CONTRIBUTING_DATA.md](docs/CONTRIBUTING_DATA.md) and the in-app
**Share your dataset** form.

## Reporting problems

Open an issue at <https://github.com/Cesarito2021/als_downloader/issues>.
For access or licensing concerns about a specific dataset, see
[inst/sources/POLICIES.md](inst/sources/POLICIES.md) first.

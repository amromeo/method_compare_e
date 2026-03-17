# Method Compare

Packaged Shiny app for CHOP Coagulation method comparison & bias estimation.

## Quickstart (dev)
1. Install deps: `install.packages("devtools")`.
2. From the repo root run `devtools::load_all(); run_app()` to launch locally.
3. Tests: `devtools::test()` (uses `tests/testthat`).

## Posit Connect
Deploy to Posit Connect from the repo root (now with `app.R` at top-level) or
bundle the package; entry point is `app.R` which calls `shinyAppDir(\"inst/app\")`
or `methodCompare::run_app()`.

## Structure
- `inst/app` — Shiny app code (ui/server/global, modules, www assets, config).
- `R/` — package helpers (currently just `run_app()`).
- `tests/` — `testthat` tests targeting key render helpers.
- `.github/workflows` — CI for install + tests.

## Configuration
Runtime JSON/R configs live in `inst/app/config`. Add secrets/admin overrides
as `inst/app/config/admin_config.R` (git-ignored in deployments).

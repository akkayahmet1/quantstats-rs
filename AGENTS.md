# Repository Guidelines

## Project Structure & Module Organization

- `src/` – core Rust library:
  - `lib.rs` public API (`html`, `HtmlReportOptions`, `ReturnSeries`).
  - `reports.rs` metrics table + HTML generation (must match Python QuantStats).
  - `stats.rs` numerical routines (Sharpe, drawdowns, CPC, etc.).
  - `plots.rs` SVG plot helpers.
  - `report_template.html` copied from Python `report.html`.
- `examples/` – runnable binaries:
  - `html_report.rs` (strategy only).
  - `html_with_benchmark.rs` (strategy + benchmark).
  - `common.rs` auto‑generated from `data` by `scripts/gen_examples_common.py`.
- `third_party/quantstats/` – vendored Python QuantStats; used for reference and cross‑checking only.

## Build, Test, and Development Commands

- `cargo build` – build the library.
- `cargo test` – run Rust tests (add new tests alongside modules using `#[cfg(test)]`).
- `cargo run --example html_report` – generate `tearsheet.html`.
- `cargo run --example html_with_benchmark` – generate `tearsheet_with_benchmark.html`.
- `python3 scripts/gen_examples_common.py` – regenerate `examples/common.rs` from `data` after changing the dataset.

## Coding Style & Naming Conventions

- Rust edition 2021, 4‑space indentation, no tabs.
- Prefer `rustfmt` defaults (`cargo fmt`) and keep functions small and composable.
- Naming:
  - Types: `CamelCase` (`ReturnSeries`, `PerformanceMetrics`).
  - Functions: `snake_case` (`compute_performance_metrics`, `gain_to_pain_monthly`).
  - Modules/files mirror their main type or concern (`stats.rs`, `reports.rs`).
- Keep code ASCII; avoid Unicode in identifiers and strings unless mirroring HTML.

## Testing Guidelines

- Add unit tests inside the corresponding module file under `#[cfg(test)]` blocks.
- For metrics, prefer table‑driven tests comparing against known Python QuantStats outputs (e.g., values extracted from `SOL_USDT_breakout_quantstats.html`).
- Ensure `cargo test` passes before opening a PR.

## Commit & Pull Request Guidelines

- Commit messages follow the existing style:
  - `feat(metrics): align drawdown averages with QuantStats`
  - `fix(examples): skip first data point to match Python start date`
- Keep commits scoped to one logical change (e.g., one metric family or one example).
- PRs should:
  - Describe what changed and why (including which QuantStats metrics were aligned).
  - Mention how you validated (commands run, files inspected).
  - Include screenshots or diff snippets of key HTML sections when changing reports or plots.


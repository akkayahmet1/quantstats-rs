# quantstats-rs

`quantstats-rs` is a Rust library that generates QuantStats-style HTML performance tear sheets
from return time series. It aims to closely match the behaviour and visuals of the original
Python [QuantStats](https://github.com/ranaroussi/quantstats) project, using the vendored
Python implementation in `third_party/quantstats/` as a reference.

The repository also includes pre-generated example reports:

- [`tearsheet.html`](tearsheet.html) – strategy only
- [`tearsheet_with_benchmark.html`](tearsheet_with_benchmark.html) – strategy vs benchmark

You can open these directly in a browser to see what the library produces.

## Features

- Generate full HTML tear sheets from:
  - A single strategy return series; or
  - A strategy + benchmark pair.
- Metrics table (see `PerformanceMetrics`), including:
  - CAGR, Sharpe, Sortino, Calmar
  - Max / average drawdown, longest drawdown
  - Gain-to-Pain and related ratios
- Charts (SVG, embedded in the HTML):
  - Cumulative Returns / Cumulative Returns vs Benchmark  
    (including log-scaled and volatility-matched variants)
  - Daily Returns (Cumulative Sum)
  - Rolling Volatility / Rolling Sharpe / Rolling Sortino / Rolling Beta
  - Drawdown (Underwater)  
    (average drawdown red dashed line, filled area below 0%)
  - Strategy – Worst 5 Drawdown Periods
  - EOY Returns / EOY Returns vs Benchmark (with red dashed mean line)
  - Monthly Returns Heatmap
  - Returns Distribution / Monthly Distribution
- Visual alignment with Python QuantStats:
  - Matching colour scheme (strategy `#348dc1`, benchmark `#ff9933`, etc.)
  - Proportional grid, labelled y-axis, bottom time axis
  - Dashed zero / mean lines and filled areas (e.g. Underwater Plot)

## Repository Layout

- `src/`
  - `lib.rs` – public API: `html`, `HtmlReportOptions`, `ReturnSeries`, `PerformanceMetrics`.
  - `reports.rs` – report assembly, metrics table rendering, and template filling.
  - `stats.rs` – performance statistics and drawdown segment logic.
  - `plots.rs` – all SVG chart generation.
  - `report_template.html` – HTML template, roughly mirroring QuantStats’ `report.html`.
- `examples/`
  - `html_report.rs` – strategy only; writes `tearsheet.html`.
  - `html_with_benchmark.rs` – strategy + benchmark; writes `tearsheet_with_benchmark.html`.
  - `common.rs` – shared demo data, generated from `data/` by the script.
- `data/` – CSVs / time series used to build the example report.
- `scripts/gen_examples_common.py` – generates `examples/common.rs` from `data/`.
- `third_party/quantstats/` – vendored Python QuantStats, used only for reference / validation.

## Build & Run

From the repository root:

```bash
cargo build

# Strategy-only demo report
cargo run --example html_report
# Output: tearsheet.html

# Strategy + benchmark demo report
cargo run --example html_with_benchmark
# Output: tearsheet_with_benchmark.html

# Tests (if present)
cargo test
```

The generated HTML files can be opened directly in a browser and visually compared against
reports produced by the Python QuantStats library.

## Library Usage

Add a dependency (path here assumes your project is a sibling of this repo):

```toml
[dependencies]
quantstats-rs = { path = "../quantstats-rs" }
```

Basic usage in Rust:

```rust
use chrono::NaiveDate;
use quantstats_rs::{ReturnSeries, HtmlReportOptions, html};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // 1. Prepare dates and returns (e.g. daily returns)
    let dates: Vec<NaiveDate> = /* ... */;
    let values: Vec<f64> = /* ... */; // e.g. 0.01 means +1%

    let strategy = ReturnSeries::new(dates, values, Some("Strategy".to_string()))?;

    // 2. Configure report options
    let options = HtmlReportOptions::default()
        .with_title("My Strategy Tearsheet")
        .with_strategy_title("My Strategy")
        .with_output("tearsheet.html");

    // 3. Generate HTML string and write to file (if `output` is set)
    let html_string = html(&strategy, options)?;

    println!("Report generated ({} bytes)", html_string.len());
    Ok(())
}
```

With a benchmark:

```rust
let strategy: ReturnSeries = /* ... */;
let benchmark: ReturnSeries = /* ... */;

let options = HtmlReportOptions::default()
    .with_benchmark(&benchmark)
    .with_title("Strategy vs Benchmark")
    .with_strategy_title("Strategy")
    .with_benchmark_title("Benchmark")
    .with_output("tearsheet_with_benchmark.html");

let html_string = html(&strategy, options)?;
```

## Example Data & Regeneration

The example binaries use `examples/common.rs`, which is generated from `data/`:

```bash
python3 scripts/gen_examples_common.py
```

After changing the data under `data/`, re-run the script and then rerun the examples
to regenerate `tearsheet.html` and `tearsheet_with_benchmark.html`.

## Alignment with Python QuantStats

This implementation uses the vendored Python QuantStats code and its HTML output
as the reference. The following aspects are intentionally aligned:

- Rolling Vol / Sharpe / Sortino / Beta:
  - Axis ranges, grid density, and line styles (including red dashed mean lines).
- Underwater Plot:
  - Filled area below 0% in strategy colour.
  - Red dashed line at the average drawdown.
  - Solid underwater curve.
- Strategy – Worst 5 Drawdown Periods:
  - Y-axis and grid styling.
  - Label positions for the “N: Xd” drawdown annotations.
- EOY Returns vs Benchmark:
  - Bar layout and colours.
  - Red dashed mean line for strategy returns.
- Time axis handling:
  - Bottom-aligned date labels for all time-series charts.
  - Label thinning to avoid overlap (e.g. around dense months like 2025‑10 / 2025‑11).

If you notice discrepancies compared to a Python QuantStats report (especially in a
specific chart such as “Rolling Sortino” or “Underwater Plot”), please open an issue or
PR and mention:

- Which chart (title),
- Which part differs (axis range, grid, colours, etc.),
- Optionally, a snippet or screenshot from the Python-generated HTML.


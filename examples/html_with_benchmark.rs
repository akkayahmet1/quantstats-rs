mod common;

use quantstats_rs::{HtmlReportOptions, html};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Use shared demo data from examples/common.rs (generated from data file)
    let strategy = common::demo_strategy();
    let benchmark = common::demo_benchmark();

    let options = HtmlReportOptions::default()
        .with_benchmark(&benchmark)
        .with_title("Quantstats-rs Demo Tearsheet (with Benchmark)")
        .with_strategy_title("Strategy")
        .with_benchmark_title("Benchmark Index")
        .with_output("tearsheet_with_benchmark.html");

    let html = html(&strategy, options)?;

    println!(
        "Generated HTML report with benchmark ({} bytes) at tearsheet_with_benchmark.html",
        html.len()
    );

    Ok(())
}

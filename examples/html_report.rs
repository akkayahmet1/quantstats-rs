mod common;

use quantstats_rs::{HtmlReportOptions, html};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Use shared demo data from examples/common.rs (generated from data file)
    let series = common::demo_strategy();

    let options = HtmlReportOptions::default()
        .with_title("Quantstats-rs Demo Tearsheet")
        .with_strategy_title("Demo Strategy")
        .with_output("tearsheet.html");

    let html = html(&series, options)?;

    println!(
        "Generated HTML report ({} bytes) at tearsheet.html",
        html.len()
    );

    Ok(())
}

use chrono::NaiveDate;
use quantstats_rs::{html, HtmlReportOptions, ReturnSeries};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Build a simple fake daily return series for ~3 months
    let start = NaiveDate::from_ymd_opt(2024, 1, 1).expect("valid date");
    let mut dates = Vec::new();
    let mut returns = Vec::new();

    for i in 0..60 {
        let date = start + chrono::Days::new(i);
        dates.push(date);

        // Simple pattern: small positive drift with some noise-like variation
        let r = match i % 5 {
            0 => 0.005,
            1 => -0.002,
            2 => 0.003,
            3 => 0.0,
            _ => 0.001,
        };
        returns.push(r);
    }

    let series = ReturnSeries::new(dates, returns, Some("Demo Strategy".to_string()))?;

    let options = HtmlReportOptions::default()
        .with_title("Quantstats-rs Demo Tearsheet")
        .with_strategy_title("Demo Strategy")
        .with_output("tearsheet.html");

    let html = html(&series, options)?;

    println!("Generated HTML report ({} bytes) at tearsheet.html", html.len());

    Ok(())
}


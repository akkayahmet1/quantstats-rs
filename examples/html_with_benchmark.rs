use chrono::NaiveDate;
use quantstats_rs::{html, HtmlReportOptions, ReturnSeries};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Build strategy returns
    let start = NaiveDate::from_ymd_opt(2024, 1, 1).expect("valid date");
    let mut dates = Vec::new();
    let mut strat_returns = Vec::new();
    let mut bench_returns = Vec::new();

    for i in 0..60 {
        let date = start + chrono::Days::new(i);
        dates.push(date);

        // Strategy: slightly higher volatility and drift
        let s = match i % 5 {
            0 => 0.007,
            1 => -0.004,
            2 => 0.004,
            3 => 0.0,
            _ => 0.002,
        };

        // Benchmark: smoother, lower returns
        let b = match i % 5 {
            0 => 0.004,
            1 => -0.0015,
            2 => 0.0025,
            3 => 0.0,
            _ => 0.001,
        };

        strat_returns.push(s);
        bench_returns.push(b);
    }

    let strategy = ReturnSeries::new(dates.clone(), strat_returns, Some("Strategy".to_string()))?;
    let benchmark = ReturnSeries::new(dates, bench_returns, Some("Benchmark".to_string()))?;

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


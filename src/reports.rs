use std::path::{Path, PathBuf};

use chrono::Datelike;

use crate::plots;
use crate::stats::{compute_performance_metrics, top_drawdowns, Drawdown, PerformanceMetrics};
use crate::utils::{align_start_dates, DataError, ReturnSeries};

const DEFAULT_TITLE: &str = "Strategy Tearsheet";
const DEFAULT_PERIODS_PER_YEAR: u32 = 252;
const VERSION: &str = env!("CARGO_PKG_VERSION");
const DEFAULT_TEMPLATE: &str = include_str!("report_template.html");

#[derive(Debug)]
pub enum HtmlReportError {
    Data(DataError),
    Io(std::io::Error),
    EmptySeries,
}

impl From<DataError> for HtmlReportError {
    fn from(err: DataError) -> Self {
        HtmlReportError::Data(err)
    }
}

impl From<std::io::Error> for HtmlReportError {
    fn from(err: std::io::Error) -> Self {
        HtmlReportError::Io(err)
    }
}

impl std::fmt::Display for HtmlReportError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HtmlReportError::Data(e) => write!(f, "data error: {e}"),
            HtmlReportError::Io(e) => write!(f, "io error: {e}"),
            HtmlReportError::EmptySeries => write!(f, "returns series is empty"),
        }
    }
}

impl std::error::Error for HtmlReportError {}

pub struct HtmlReportOptions<'a> {
    pub benchmark: Option<&'a ReturnSeries>,
    pub rf: f64,
    pub grayscale: bool,
    pub title: String,
    pub output: Option<PathBuf>,
    pub compounded: bool,
    pub periods_per_year: u32,
    pub template_path: Option<PathBuf>,
    pub match_dates: bool,
    pub strategy_title: Option<String>,
    pub benchmark_title: Option<String>,
}

impl<'a> Default for HtmlReportOptions<'a> {
    fn default() -> Self {
        Self {
            benchmark: None,
            rf: 0.0,
            grayscale: false,
            title: DEFAULT_TITLE.to_string(),
            output: None,
            compounded: true,
            periods_per_year: DEFAULT_PERIODS_PER_YEAR,
            template_path: None,
            match_dates: true,
            strategy_title: Some("Strategy".to_string()),
            benchmark_title: None,
        }
    }
}

impl<'a> HtmlReportOptions<'a> {
    pub fn with_benchmark(mut self, benchmark: &'a ReturnSeries) -> Self {
        self.benchmark = Some(benchmark);
        self
    }

    pub fn with_output<P: AsRef<Path>>(mut self, path: P) -> Self {
        self.output = Some(path.as_ref().to_path_buf());
        self
    }

    pub fn with_title<S: Into<String>>(mut self, title: S) -> Self {
        self.title = title.into();
        self
    }

    pub fn with_strategy_title<S: Into<String>>(mut self, title: S) -> Self {
        self.strategy_title = Some(title.into());
        self
    }

    pub fn with_benchmark_title<S: Into<String>>(mut self, title: S) -> Self {
        self.benchmark_title = Some(title.into());
        self
    }

    pub fn with_template_path<P: AsRef<Path>>(mut self, path: P) -> Self {
        self.template_path = Some(path.as_ref().to_path_buf());
        self
    }
}

pub fn html<'a>(
    returns: &ReturnSeries,
    options: HtmlReportOptions<'a>,
) -> Result<String, HtmlReportError> {
    if returns.is_empty() {
        return Err(HtmlReportError::EmptySeries);
    }

    let (prepared_returns, prepared_benchmark) = match (options.benchmark, options.match_dates) {
        (Some(bench), true) => {
            let (aligned_r, aligned_b) = align_start_dates(returns, bench);
            (aligned_r, Some(aligned_b))
        }
        (Some(bench), false) => (returns.clone(), Some(bench.clone())),
        (None, _) => (returns.clone(), None),
    };

    let metrics = compute_performance_metrics(&prepared_returns, options.rf, options.periods_per_year);
    let benchmark_metrics = prepared_benchmark
        .as_ref()
        .map(|b| compute_performance_metrics(b, options.rf, options.periods_per_year));

    let mut tpl = if let Some(path) = &options.template_path {
        std::fs::read_to_string(path)?
    } else {
        DEFAULT_TEMPLATE.to_string()
    };

    let date_range = prepared_returns
        .date_range()
        .ok_or(HtmlReportError::EmptySeries)?;

    let start = date_range.0.format("%e %b, %Y").to_string();
    let end = date_range.1.format("%e %b, %Y").to_string();
    let date_range_str = format!("{} - {}", start.trim(), end.trim());

    tpl = tpl.replace("{{date_range}}", &date_range_str);
    tpl = tpl.replace("{{title}}", &options.title);
    tpl = tpl.replace("{{v}}", VERSION);

    let benchmark_prefix = build_benchmark_prefix(&options, prepared_benchmark.as_ref());
    tpl = tpl.replace("{{benchmark_title}}", &benchmark_prefix);

    let metrics_html = build_metrics_table(
        &metrics,
        benchmark_metrics.as_ref(),
        options.strategy_title.as_deref().unwrap_or("Strategy"),
        options
            .benchmark_title
            .as_deref()
            .unwrap_or("Benchmark"),
        options.rf,
    );
    tpl = tpl.replace("{{metrics}}", &metrics_html);

    let benchmark_ref = prepared_benchmark.as_ref();

    let returns_svg = plots::returns(&prepared_returns, benchmark_ref);
    tpl = tpl.replace("{{returns}}", &returns_svg);

    let log_returns_svg = plots::log_returns(&prepared_returns, benchmark_ref);
    tpl = tpl.replace("{{log_returns}}", &log_returns_svg);

    let vol_returns_svg = plots::vol_matched_returns(&prepared_returns, benchmark_ref);
    tpl = tpl.replace("{{vol_returns}}", &vol_returns_svg);

    let eoy_returns_svg = plots::returns(&prepared_returns, benchmark_ref);
    tpl = tpl.replace("{{eoy_returns}}", &eoy_returns_svg);

    let monthly_dist_svg = plots::histogram(&prepared_returns);
    tpl = tpl.replace("{{monthly_dist}}", &monthly_dist_svg);

    let daily_returns_svg = plots::daily_returns(&prepared_returns);
    tpl = tpl.replace("{{daily_returns}}", &daily_returns_svg);

    let rolling_beta_svg = plots::returns(&prepared_returns, benchmark_ref);
    tpl = tpl.replace("{{rolling_beta}}", &rolling_beta_svg);

    let rolling_vol_svg = plots::returns(&prepared_returns, benchmark_ref);
    tpl = tpl.replace("{{rolling_vol}}", &rolling_vol_svg);

    let rolling_sharpe_svg = plots::returns(&prepared_returns, benchmark_ref);
    tpl = tpl.replace("{{rolling_sharpe}}", &rolling_sharpe_svg);

    let rolling_sortino_svg = plots::returns(&prepared_returns, benchmark_ref);
    tpl = tpl.replace("{{rolling_sortino}}", &rolling_sortino_svg);

    let dd_periods_svg = plots::drawdown(&prepared_returns);
    tpl = tpl.replace("{{dd_periods}}", &dd_periods_svg);

    let dd_plot_svg = plots::drawdown(&prepared_returns);
    tpl = tpl.replace("{{dd_plot}}", &dd_plot_svg);

    let monthly_heatmap_svg = plots::histogram(&prepared_returns);
    tpl = tpl.replace("{{monthly_heatmap}}", &monthly_heatmap_svg);

    let returns_dist_svg = plots::returns_distribution(&prepared_returns);
    tpl = tpl.replace("{{returns_dist}}", &returns_dist_svg);

    let eoy_title = if prepared_benchmark.is_some() {
        "<h3>EOY Returns vs Benchmark</h3>"
    } else {
        "<h3>EOY Returns</h3>"
    };
    tpl = tpl.replace("{{eoy_title}}", eoy_title);

    let eoy_table_html = build_eoy_table(&prepared_returns, prepared_benchmark.as_ref());
    tpl = tpl.replace("{{eoy_table}}", &eoy_table_html);

    let dd_segments = top_drawdowns(&prepared_returns, 10);
    let dd_info_html = build_drawdown_info(&dd_segments);
    tpl = tpl.replace("{{dd_info}}", &dd_info_html);

    if let Some(path) = &options.output {
        std::fs::write(path, &tpl)?;
    }

    Ok(tpl)
}

fn build_benchmark_prefix(
    options: &HtmlReportOptions<'_>,
    prepared_benchmark: Option<&ReturnSeries>,
) -> String {
    if let Some(_) = prepared_benchmark {
        if let Some(ref title) = options.benchmark_title {
            format!("Benchmark is {} | ", title)
        } else {
            "Benchmark | ".to_string()
        }
    } else {
        String::new()
    }
}

fn build_metrics_table(
    strategy: &PerformanceMetrics,
    benchmark: Option<&PerformanceMetrics>,
    strategy_title: &str,
    benchmark_title: &str,
    rf: f64,
) -> String {
    let mut html = String::new();
    html.push_str("<table><thead><tr><th>Metric</th>");

    if let Some(_) = benchmark {
        html.push_str("<th>");
        html.push_str(benchmark_title);
        html.push_str("</th>");
    }

    html.push_str("<th>");
    html.push_str(strategy_title);
    html.push_str("</th></tr></thead><tbody>");

    // Risk-free rate
    html.push_str(&format!(
        "<tr><td>Risk-Free Rate</td>{}{}</tr>",
        benchmark
            .as_ref()
            .map(|_| format!("<td>{:.1}%</td>", rf * 100.0))
            .unwrap_or_default(),
        format!("<td>{:.1}%</td>", rf * 100.0),
    ));

    // Simple separator
    let colspan = if benchmark.is_some() { 3 } else { 2 };
    html.push_str(&format!(
        r#"<tr><td colspan="{}"><hr></td></tr>"#,
        colspan
    ));

    // Cumulative / annualized returns
    let bench_total = benchmark.map(|b| b.total_return * 100.0);
    html.push_str("<tr><td>Cumulative Return</td>");
    if let Some(b) = bench_total {
        html.push_str(&format!("<td>{:.2}%</td>", b));
    }
    html.push_str(&format!(
        "<td>{:.2}%</td></tr>",
        strategy.total_return * 100.0
    ));

    let bench_cagr = benchmark.map(|b| b.annualized_return * 100.0);
    html.push_str("<tr><td>CAGR﹪</td>");
    if let Some(b) = bench_cagr {
        html.push_str(&format!("<td>{:.2}%</td>", b));
    }
    html.push_str(&format!(
        "<td>{:.2}%</td></tr>",
        strategy.annualized_return * 100.0
    ));

    html.push_str(&format!(
        r#"<tr><td colspan="{}"><hr></td></tr>"#,
        colspan
    ));

    // Sharpe and volatility
    html.push_str("<tr><td>Sharpe</td>");
    if let Some(b) = benchmark {
        html.push_str(&format!("<td>{:.2}</td>", b.sharpe_ratio));
    }
    html.push_str(&format!("<td>{:.2}</td></tr>", strategy.sharpe_ratio));

    html.push_str("<tr><td>Volatility (ann.)</td>");
    if let Some(b) = benchmark {
        html.push_str(&format!(
            "<td>{:.2}%</td>",
            b.annualized_volatility * 100.0
        ));
    }
    html.push_str(&format!(
        "<td>{:.2}%</td></tr>",
        strategy.annualized_volatility * 100.0
    ));

    html.push_str(&format!(
        r#"<tr><td colspan="{}"><hr></td></tr>"#,
        colspan
    ));

    // Drawdown information
    html.push_str("<tr><td>Max Drawdown</td>");
    if let Some(b) = benchmark {
        html.push_str(&format!("<td>{:.2}%</td>", b.max_drawdown * 100.0));
    }
    html.push_str(&format!(
        "<td>{:.2}%</td></tr>",
        strategy.max_drawdown * 100.0
    ));

    html.push_str("<tr><td>Longest DD Days</td>");
    if let Some(b) = benchmark {
        html.push_str(&format!("<td>{}</td>", b.max_drawdown_duration));
    }
    html.push_str(&format!(
        "<td>{}</td></tr>",
        strategy.max_drawdown_duration
    ));

    // Best / worst days
    html.push_str(&format!(
        r#"<tr><td colspan="{}"><hr></td></tr>"#,
        colspan
    ));

    html.push_str("<tr><td>Best Day</td>");
    if let Some(b) = benchmark {
        html.push_str(&format!("<td>{:.2}%</td>", b.best_day * 100.0));
    }
    html.push_str(&format!(
        "<td>{:.2}%</td></tr>",
        strategy.best_day * 100.0
    ));

    html.push_str("<tr><td>Worst Day</td>");
    if let Some(b) = benchmark {
        html.push_str(&format!("<td>{:.2}%</td>", b.worst_day * 100.0));
    }
    html.push_str(&format!(
        "<td>{:.2}%</td></tr>",
        strategy.worst_day * 100.0
    ));

    html.push_str("</tbody></table>");
    html
}

fn build_drawdown_info(drawdowns: &[Drawdown]) -> String {
    let mut html = String::new();
    html.push_str("<table><thead><tr>");
    html.push_str("<th>Started</th>");
    html.push_str("<th>Recovered</th>");
    html.push_str("<th>Drawdown</th>");
    html.push_str("<th>Days</th>");
    html.push_str("</tr></thead><tbody>");
    for dd in drawdowns {
        html.push_str(&format!(
            "<tr><td>{}</td><td>{}</td><td>{:.2}</td><td>{}</td></tr>",
            dd.start.format("%Y-%m-%d"),
            dd.end.format("%Y-%m-%d"),
            dd.depth * 100.0,
            dd.duration
        ));
    }
    html.push_str("</tbody></table>");
    html
}

fn build_eoy_table(
    strategy: &ReturnSeries,
    benchmark: Option<&ReturnSeries>,
) -> String {
    use std::collections::BTreeMap;

    fn yearly_compounded(series: &ReturnSeries) -> BTreeMap<i32, f64> {
        let mut grouped: BTreeMap<i32, Vec<f64>> = BTreeMap::new();
        for (date, ret) in series.dates.iter().zip(series.values.iter()) {
            if ret.is_nan() {
                continue;
            }
            grouped.entry(date.year()).or_default().push(*ret);
        }

        let mut out = BTreeMap::new();
        for (year, vals) in grouped {
            if vals.is_empty() {
                continue;
            }
            let total = vals
                .iter()
                .fold(1.0_f64, |acc, r| acc * (1.0 + *r))
                - 1.0;
            out.insert(year, total);
        }
        out
    }

    let strat_years = yearly_compounded(strategy);
    let bench_years = benchmark.map(yearly_compounded);

    if strat_years.is_empty() {
        return "<p>No EOY data available.</p>".to_string();
    }

    let mut years: Vec<i32> = strat_years.keys().copied().collect();
    if let Some(ref b) = bench_years {
        for y in b.keys() {
            if !years.contains(y) {
                years.push(*y);
            }
        }
    }
    years.sort();

    let mut html = String::new();
    html.push_str("<table>\n<thead>\n<tr><th>Year</th>");
    if bench_years.is_some() {
        html.push_str("<th>Benchmark</th><th>Strategy</th><th>Multiplier</th><th>Won</th>");
    } else {
        html.push_str("<th>Strategy</th>");
    }
    html.push_str("</tr>\n</thead>\n<tbody>\n");

    for year in years {
        let strat = strat_years.get(&year).copied().unwrap_or(0.0) * 100.0;
        if let Some(ref bench_map) = bench_years {
            let bench = bench_map.get(&year).copied().unwrap_or(0.0) * 100.0;
            let multiplier = if bench.abs() > f64::EPSILON {
                strat / bench
            } else {
                0.0
            };
            let won = if strat > bench { "+" } else { "-" };
            html.push_str(&format!(
                "<tr><td>{}</td><td>{:.2}</td><td>{:.2}</td><td>{:.2}</td><td>{}</td></tr>\n",
                year, bench, strat, multiplier, won
            ));
        } else {
            html.push_str(&format!(
                "<tr><td>{}</td><td>{:.2}</td></tr>\n",
                year, strat
            ));
        }
    }

    html.push_str("</tbody>\n</table>");
    html
}

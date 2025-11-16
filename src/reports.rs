use std::path::{Path, PathBuf};

use crate::plots;
use crate::stats::{compute_performance_metrics, PerformanceMetrics};
use crate::utils::{align_start_dates, DataError, ReturnSeries};

const DEFAULT_TITLE: &str = "Strategy Tearsheet";
const DEFAULT_PERIODS_PER_YEAR: u32 = 252;
const VERSION: &str = env!("CARGO_PKG_VERSION");
const DEFAULT_TEMPLATE: &str = include_str!("../third_party/quantstats/quantstats/report.html");

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

    let metrics = compute_performance_metrics(
        &prepared_returns,
        options.rf,
        options.periods_per_year,
    );

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
        options.strategy_title.as_deref().unwrap_or("Strategy"),
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

    tpl = tpl.replace("{{eoy_title}}", "<h3>End-of-Year Returns</h3>");
    tpl = tpl.replace(
        "{{eoy_table}}",
        "<p>End-of-year return table not implemented in quantstats-rs yet.</p>",
    );

    let dd_info_html = build_drawdown_info(&metrics);
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

fn build_metrics_table(metrics: &PerformanceMetrics, strategy_title: &str) -> String {
    let mut html = String::new();
    html.push_str("<table><thead><tr><th>Metric</th><th>");
    html.push_str(strategy_title);
    html.push_str("</th></tr></thead><tbody>");

    html.push_str(&format!(
        "<tr><td>Total Return</td><td>{:.2}%</td></tr>",
        metrics.total_return * 100.0
    ));
    html.push_str(&format!(
        "<tr><td>Annualized Return</td><td>{:.2}%</td></tr>",
        metrics.annualized_return * 100.0
    ));
    html.push_str(&format!(
        "<tr><td>Annualized Volatility</td><td>{:.2}%</td></tr>",
        metrics.annualized_volatility * 100.0
    ));
    html.push_str(&format!(
        "<tr><td>Sharpe Ratio</td><td>{:.2}</td></tr>",
        metrics.sharpe_ratio
    ));
    html.push_str(&format!(
        "<tr><td>Max Drawdown</td><td>{:.2}%</td></tr>",
        metrics.max_drawdown * 100.0
    ));
    html.push_str(&format!(
        "<tr><td>Max Drawdown Duration</td><td>{}</td></tr>",
        metrics.max_drawdown_duration
    ));
    html.push_str(&format!(
        "<tr><td>Best Day</td><td>{:.2}%</td></tr>",
        metrics.best_day * 100.0
    ));
    html.push_str(&format!(
        "<tr><td>Worst Day</td><td>{:.2}%</td></tr>",
        metrics.worst_day * 100.0
    ));

    html.push_str("</tbody></table>");
    html
}

fn build_drawdown_info(metrics: &PerformanceMetrics) -> String {
    let mut html = String::new();
    html.push_str("<table><thead><tr>");
    html.push_str("<th>Max Drawdown</th>");
    html.push_str("<th>Duration</th>");
    html.push_str("</tr></thead><tbody>");
    html.push_str(&format!(
        "<tr><td>{:.2}%</td><td>{}</td></tr>",
        metrics.max_drawdown * 100.0,
        metrics.max_drawdown_duration
    ));
    html.push_str("</tbody></table>");
    html
}

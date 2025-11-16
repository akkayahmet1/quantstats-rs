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
        &prepared_returns,
        prepared_benchmark.as_ref(),
        options.strategy_title.as_deref().unwrap_or("Strategy"),
        options
            .benchmark_title
            .as_deref()
            .unwrap_or("Benchmark"),
        options.rf,
        options.periods_per_year,
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
    strategy_returns: &ReturnSeries,
    benchmark_returns: Option<&ReturnSeries>,
    strategy_title: &str,
    benchmark_title: &str,
    rf: f64,
    periods_per_year: u32,
) -> String {
    let mut html = String::new();
    html.push_str("<table><thead><tr><th>Metric</th>");

    if benchmark.is_some() {
        html.push_str("<th>");
        html.push_str(benchmark_title);
        html.push_str("</th>");
    }

    html.push_str("<th>");
    html.push_str(strategy_title);
    html.push_str("</th></tr></thead><tbody>");

    let colspan = if benchmark.is_some() { 3 } else { 2 };

    // Pre-compute basic stats for strategy and benchmark
    let strat_vals = clean_values(strategy_returns);
    let bench_vals = benchmark_returns.map(clean_values);

    let s_mean = mean(&strat_vals);
    let s_std = std_dev(&strat_vals);
    let (s_skew, s_kurt) = skew_kurtosis(&strat_vals);
    let s_downside = downside_std(&strat_vals, 0.0);
    let daily_rf = rf / periods_per_year as f64;
    let s_n = strat_vals.len().max(1) as f64;

    let b_stats = bench_vals.as_ref().map(|vals| {
        let m = mean(vals);
        let s = std_dev(vals);
        let (sk, ku) = skew_kurtosis(vals);
        let d = downside_std(vals, 0.0);
        let n = vals.len().max(1) as f64;
        (m, s, sk, ku, d, n)
    });

    // Risk-free rate
    html.push_str(&format!(
        "<tr><td>Risk-Free Rate</td>{}{}</tr>",
        benchmark
            .as_ref()
            .map(|_| format!("<td>{:.1}%</td>", rf * 100.0))
            .unwrap_or_default(),
        format!("<td>{:.1}%</td>", rf * 100.0),
    ));

    // Time in market
    html.push_str("<tr><td>Time in Market</td>");
    if let Some(b_ret) = benchmark_returns {
        html.push_str(&format!(
            "<td>{:.1}%</td>",
            time_in_market(b_ret) * 100.0
        ));
    }
    html.push_str(&format!(
        "<td>{:.1}%</td></tr>",
        time_in_market(strategy_returns) * 100.0
    ));

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

    // Sharpe-like ratios
    html.push_str("<tr><td>Sharpe</td>");
    if let Some(b) = benchmark {
        html.push_str(&format!("<td>{:.2}</td>", b.sharpe_ratio));
    }
    html.push_str(&format!("<td>{:.2}</td></tr>", strategy.sharpe_ratio));

    // Prob. Sharpe Ratio (simplified probabilistic Sharpe)
    fn probabilistic_sharpe(base_sr: f64, skew: f64, kurt: f64, n: f64) -> f64 {
        if n <= 1.0 {
            return 0.0;
        }
        let numerator = 1.0
            + (0.5 * base_sr * base_sr)
            - (skew * base_sr)
            + (((kurt - 3.0) / 4.0) * base_sr * base_sr);
        let sigma_sr = (numerator / (n - 1.0)).sqrt();
        if sigma_sr == 0.0 {
            return 0.0;
        }
        let ratio = base_sr / sigma_sr;
        // Approximate normal CDF via error function
        0.5 * (1.0 + erf(ratio / std::f64::consts::SQRT_2))
    }

    fn erf(x: f64) -> f64 {
        // Abramowitz & Stegun approximation
        let sign = if x < 0.0 { -1.0 } else { 1.0 };
        let x = x.abs();
        let t = 1.0 / (1.0 + 0.3275911 * x);
        let y = 1.0
            - (((((1.061405429 * t - 1.453152027) * t) + 1.421413741) * t
                - 0.284496736)
                * t
                + 0.254829592)
                * t
                * (-x * x).exp();
        sign * y
    }

    let base_sr_strat = if s_std > 0.0 {
        (s_mean - daily_rf) / s_std
    } else {
        0.0
    };
    let psr_strat = probabilistic_sharpe(base_sr_strat, s_skew, s_kurt, s_n);

    let psr_bench = b_stats.as_ref().map(|(m, s, sk, ku, _d, n)| {
        let base = if *s > 0.0 { (*m - daily_rf) / *s } else { 0.0 };
        probabilistic_sharpe(base, *sk, *ku, *n)
    });

    html.push_str("<tr><td>Prob. Sharpe Ratio</td>");
    if let Some(v) = psr_bench {
        html.push_str(&format!("<td>{:.2}%</td>", v * 100.0));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!("<td>{:.2}%</td></tr>", psr_strat * 100.0));

    // Smart Sharpe/Sortino approximated as standard ratios
    html.push_str("<tr><td>Smart Sharpe</td>");
    if let Some(b) = benchmark {
        html.push_str(&format!("<td>{:.2}</td>", b.sharpe_ratio));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!("<td>{:.2}</td></tr>", strategy.sharpe_ratio));

    // Sortino ratios
    let s_sortino = if s_downside > 0.0 {
        (s_mean - daily_rf) / s_downside * (periods_per_year as f64).sqrt()
    } else {
        0.0
    };
    let b_sortino = b_stats.as_ref().map(|(m, _s, _sk, _ku, d, _n)| {
        if *d > 0.0 {
            (m - daily_rf) / d * (periods_per_year as f64).sqrt()
        } else {
            0.0
        }
    });

    html.push_str("<tr><td>Sortino</td>");
    if let Some(v) = b_sortino {
        html.push_str(&format!("<td>{:.2}</td>", v));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!("<td>{:.2}</td></tr>", s_sortino));

    html.push_str("<tr><td>Smart Sortino</td>");
    if let Some(v) = b_sortino {
        html.push_str(&format!("<td>{:.2}</td>", v));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!("<td>{:.2}</td></tr>", s_sortino));

    html.push_str("<tr><td>Sortino/√2</td>");
    if let Some(v) = b_sortino {
        html.push_str(&format!("<td>{:.2}</td>", v / 2.0_f64.sqrt()));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!(
        "<td>{:.2}</td></tr>",
        s_sortino / 2.0_f64.sqrt()
    ));

    html.push_str("<tr><td>Smart Sortino/√2</td>");
    if let Some(v) = b_sortino {
        html.push_str(&format!("<td>{:.2}</td>", v / 2.0_f64.sqrt()));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!(
        "<td>{:.2}</td></tr>",
        s_sortino / 2.0_f64.sqrt()
    ));

    // Omega
    let omega_strat = omega_ratio(&strat_vals, 0.0);
    let omega_bench = bench_vals
        .as_ref()
        .map(|v| omega_ratio(v, 0.0));

    html.push_str("<tr><td>Omega</td>");
    if let Some(v) = omega_bench {
        html.push_str(&format!("<td>{:.2}</td>", v));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!("<td>{:.2}</td></tr>", omega_strat));

    html.push_str(&format!(
        r#"<tr><td colspan="{}"><hr></td></tr>"#,
        colspan
    ));

    // Drawdown information
    html.push_str("<tr><td>Max Drawdown</td>");
    if let Some(b) = benchmark {
        html.push_str(&format!("<td>{:.2}%</td>", b.max_drawdown * 100.0));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!(
        "<td>{:.2}%</td></tr>",
        strategy.max_drawdown * 100.0
    ));

    // Max DD dates: trough, start, end
    fn fmt_date(d: Option<chrono::NaiveDate>) -> String {
        d.map(|dt| dt.format("%Y-%m-%d").to_string())
            .unwrap_or_else(|| "-".to_string())
    }

    html.push_str("<tr><td>Max DD Date</td>");
    if let Some(b) = benchmark {
        html.push_str(&format!(
            "<td>{}</td>",
            fmt_date(b.max_drawdown_trough)
        ));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!(
        "<td>{}</td></tr>",
        fmt_date(strategy.max_drawdown_trough)
    ));

    html.push_str("<tr><td>Max DD Period Start</td>");
    if let Some(b) = benchmark {
        html.push_str(&format!(
            "<td>{}</td>",
            fmt_date(b.max_drawdown_start)
        ));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!(
        "<td>{}</td></tr>",
        fmt_date(strategy.max_drawdown_start)
    ));

    html.push_str("<tr><td>Max DD Period End</td>");
    if let Some(b) = benchmark {
        html.push_str(&format!(
            "<td>{}</td>",
            fmt_date(b.max_drawdown_end)
        ));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!(
        "<td>{}</td></tr>",
        fmt_date(strategy.max_drawdown_end)
    ));

    // Longest DD days from PerformanceMetrics
    html.push_str("<tr><td>Longest DD Days</td>");
    if let Some(b) = benchmark {
        html.push_str(&format!("<td>{}</td>", b.max_drawdown_duration));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!(
        "<td>{}</td></tr>",
        strategy.max_drawdown_duration
    ));

    // Volatility (ann.), R^2, Information Ratio, Calmar, Skew, Kurtosis
    html.push_str("<tr><td>Volatility (ann.)</td>");
    if let Some(b) = benchmark {
        html.push_str(&format!(
            "<td>{:.2}%</td>",
            b.annualized_volatility * 100.0
        ));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!(
        "<td>{:.2}%</td></tr>",
        strategy.annualized_volatility * 100.0
    ));

    // Regression vs benchmark if available
    let (r2, info_ratio, beta, alpha_ann, corr, treynor) = if let (Some(_bm), Some(b_vals)) =
        (benchmark_returns, bench_vals.as_ref())
    {
        regression_metrics(
            &strat_vals,
            b_vals,
            strategy.total_return,
            rf,
            periods_per_year,
        )
        .unwrap_or((0.0, 0.0, 0.0, 0.0, 0.0, 0.0))
    } else {
        (0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
    };

    html.push_str("<tr><td>R^2</td>");
    if benchmark.is_some() {
        html.push_str(&format!("<td>{:.2}</td>", r2));
    }
    html.push_str("<td>0.00</td></tr>");

    html.push_str("<tr><td>Information Ratio</td>");
    if benchmark.is_some() {
        html.push_str(&format!("<td>{:.2}</td>", info_ratio));
    }
    html.push_str("<td>0.00</td></tr>");

    // Calmar ratio: CAGR / |Max DD|
    let calmar_strat = if strategy.max_drawdown != 0.0 {
        strategy.annualized_return / strategy.max_drawdown.abs()
    } else {
        0.0
    };
    let calmar_bench = benchmark.map(|b| {
        if b.max_drawdown != 0.0 {
            b.annualized_return / b.max_drawdown.abs()
        } else {
            0.0
        }
    });

    html.push_str("<tr><td>Calmar</td>");
    if let Some(v) = calmar_bench {
        html.push_str(&format!("<td>{:.2}</td>", v));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!("<td>{:.2}</td></tr>", calmar_strat));

    html.push_str("<tr><td>Skew</td>");
    if let Some((_, _, sk, _, _, _)) = b_stats {
        html.push_str(&format!("<td>{:.2}</td>", sk));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!("<td>{:.2}</td></tr>", s_skew));

    html.push_str("<tr><td>Kurtosis</td>");
    if let Some((_, _, _, ku, _, _)) = b_stats {
        html.push_str(&format!("<td>{:.2}</td>", ku));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!("<td>{:.2}</td></tr>", s_kurt));

    html.push_str(&format!(
        r#"<tr><td colspan="{}"><hr></td></tr>"#,
        colspan
    ));

    // Expected returns
    // Expected daily ~ geometric mean of (1+ret) - 1
    fn expected_return(values: &[f64]) -> f64 {
        if values.is_empty() {
            return 0.0;
        }
        let prod = values
            .iter()
            .fold(1.0_f64, |acc, r| acc * (1.0 + *r));
        prod.powf(1.0 / values.len() as f64) - 1.0
    }

    let exp_daily_strat = expected_return(&strat_vals);
    let exp_daily_bench = bench_vals.as_ref().map(|v| expected_return(v));

    html.push_str("<tr><td>Expected Daily</td>");
    if let Some(v) = exp_daily_bench {
        html.push_str(&format!("<td>{:.2}%</td>", v * 100.0));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!("<td>{:.2}%</td></tr>", exp_daily_strat * 100.0));

    // For monthly/yearly, approximate by scaling daily expected return
    let exp_monthly_strat = (1.0 + exp_daily_strat).powf(21.0) - 1.0;
    let exp_monthly_bench =
        exp_daily_bench.map(|d| (1.0 + d).powf(21.0) - 1.0);

    html.push_str("<tr><td>Expected Monthly</td>");
    if let Some(v) = exp_monthly_bench {
        html.push_str(&format!("<td>{:.2}%</td>", v * 100.0));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!(
        "<td>{:.2}%</td></tr>",
        exp_monthly_strat * 100.0
    ));

    let exp_yearly_strat =
        (1.0 + exp_daily_strat).powf(periods_per_year as f64) - 1.0;
    let exp_yearly_bench =
        exp_daily_bench.map(|d| (1.0 + d).powf(periods_per_year as f64) - 1.0);

    html.push_str("<tr><td>Expected Yearly</td>");
    if let Some(v) = exp_yearly_bench {
        html.push_str(&format!("<td>{:.2}%</td>", v * 100.0));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!(
        "<td>{:.2}%</td></tr>",
        exp_yearly_strat * 100.0
    ));

    // Kelly criterion
    fn kelly(values: &[f64]) -> f64 {
        let wins: Vec<f64> = values.iter().copied().filter(|v| *v > 0.0).collect();
        let losses: Vec<f64> = values.iter().copied().filter(|v| *v < 0.0).collect();
        if wins.is_empty() || losses.is_empty() {
            return 0.0;
        }
        let avg_win = mean(&wins);
        let avg_loss = mean(&losses);
        if avg_loss == 0.0 {
            return 0.0;
        }
        let win_loss_ratio = avg_win / -avg_loss;
        let non_zero = values
            .iter()
            .filter(|v| **v != 0.0)
            .count()
            .max(1) as f64;
        let win_prob = wins.len() as f64 / non_zero;
        let lose_prob = 1.0 - win_prob;
        if win_loss_ratio == 0.0 {
            0.0
        } else {
            ((win_loss_ratio * win_prob) - lose_prob) / win_loss_ratio
        }
    }

    let kelly_strat = kelly(&strat_vals);
    let kelly_bench = bench_vals.as_ref().map(|v| kelly(v));

    html.push_str("<tr><td>Kelly Criterion</td>");
    if let Some(v) = kelly_bench {
        html.push_str(&format!("<td>{:.2}%</td>", v * 100.0));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!(
        "<td>{:.2}%</td></tr>",
        kelly_strat * 100.0
    ));

    // Risk of Ruin
    fn risk_of_ruin(values: &[f64]) -> f64 {
        let non_zero: Vec<f64> =
            values.iter().copied().filter(|v| *v != 0.0).collect();
        if non_zero.is_empty() {
            return 0.0;
        }
        let wins = non_zero.iter().filter(|v| **v > 0.0).count() as f64;
        let win_rate = wins / non_zero.len() as f64;
        if win_rate == 0.0 {
            1.0
        } else {
            let ratio = (1.0 - win_rate) / (1.0 + win_rate);
            ratio.powf(non_zero.len() as f64)
        }
    }

    let ror_strat = risk_of_ruin(&strat_vals);
    let ror_bench = bench_vals.as_ref().map(|v| risk_of_ruin(v));

    html.push_str("<tr><td>Risk of Ruin</td>");
    if let Some(v) = ror_bench {
        html.push_str(&format!("<td>{:.2}%</td>", v * 100.0));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!(
        "<td>{:.2}%</td></tr>",
        ror_strat * 100.0
    ));

    // VaR and cVaR (ES)
    let var_strat = empirical_var(&strat_vals, 0.95);
    let cvar_strat = empirical_cvar(&strat_vals, 0.95);
    let var_bench = bench_vals.as_ref().map(|v| empirical_var(v, 0.95));
    let cvar_bench = bench_vals.as_ref().map(|v| empirical_cvar(v, 0.95));

    html.push_str("<tr><td>Daily Value-at-Risk</td>");
    if let Some(v) = var_bench {
        html.push_str(&format!("<td>{:.2}%</td>", v * 100.0));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!(
        "<td>{:.2}%</td></tr>",
        var_strat * 100.0
    ));

    html.push_str("<tr><td>Expected Shortfall (cVaR)</td>");
    if let Some(v) = cvar_bench {
        html.push_str(&format!("<td>{:.2}%</td>", v * 100.0));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!(
        "<td>{:.2}%</td></tr>",
        cvar_strat * 100.0
    ));

    html.push_str(&format!(
        r#"<tr><td colspan="{}"><hr></td></tr>"#,
        colspan
    ));

    // Consecutive wins/losses and gain/pain
    let max_wins_strat = max_consecutive_streak(&strat_vals, true);
    let max_losses_strat = max_consecutive_streak(&strat_vals, false);
    let max_wins_bench =
        bench_vals.as_ref().map(|v| max_consecutive_streak(v, true));
    let max_losses_bench =
        bench_vals.as_ref().map(|v| max_consecutive_streak(v, false));

    html.push_str("<tr><td>Max Consecutive Wins</td>");
    if let Some(v) = max_wins_bench {
        html.push_str(&format!("<td>{}</td>", v));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!("<td>{}</td></tr>", max_wins_strat));

    html.push_str("<tr><td>Max Consecutive Losses</td>");
    if let Some(v) = max_losses_bench {
        html.push_str(&format!("<td>{}</td>", v));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!(
        "<td>{}</td></tr>",
        max_losses_strat
    ));

    let gp_strat = gain_to_pain(&strat_vals);
    let gp_bench = bench_vals.as_ref().map(|v| gain_to_pain(v));

    html.push_str("<tr><td>Gain/Pain Ratio</td>");
    if let Some(v) = gp_bench {
        html.push_str(&format!("<td>{:.2}</td>", v));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!("<td>{:.2}</td></tr>", gp_strat));

    html.push_str("<tr><td>Gain/Pain (1M)</td>");
    // For simplicity, reuse daily Gain/Pain for now
    if let Some(v) = gp_bench {
        html.push_str(&format!("<td>{:.2}</td>", v));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!("<td>{:.2}</td></tr>", gp_strat));

    html.push_str(&format!(
        r#"<tr><td colspan="{}"><hr></td></tr>"#,
        colspan
    ));

    // Payoff / profit factor / tail metrics
    let payoff_strat = payoff_ratio(&strat_vals);
    let payoff_bench = bench_vals.as_ref().map(|v| payoff_ratio(v));
    html.push_str("<tr><td>Payoff Ratio</td>");
    if let Some(v) = payoff_bench {
        html.push_str(&format!("<td>{:.2}</td>", v));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!("<td>{:.2}</td></tr>", payoff_strat));

    let pf_strat = profit_factor(&strat_vals);
    let pf_bench = bench_vals.as_ref().map(|v| profit_factor(v));
    html.push_str("<tr><td>Profit Factor</td>");
    if let Some(v) = pf_bench {
        html.push_str(&format!("<td>{:.2}</td>", v));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!("<td>{:.2}</td></tr>", pf_strat));

    let tail_strat = tail_ratio(&strat_vals);
    let tail_bench = bench_vals.as_ref().map(|v| tail_ratio(v));
    let csr_strat = pf_strat * tail_strat;
    let csr_bench = pf_bench
        .zip(tail_bench)
        .map(|(pf, t)| pf * t);

    html.push_str("<tr><td>Common Sense Ratio</td>");
    if let Some(v) = csr_bench {
        html.push_str(&format!("<td>{:.2}</td>", v));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!("<td>{:.2}</td></tr>", csr_strat));

    // Tail Ratio row (after CPC Index in Python version)
    html.push_str("<tr><td>Tail Ratio</td>");
    if let Some(v) = tail_bench {
        html.push_str(&format!("<td>{:.2}</td>", v));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!("<td>{:.2}</td></tr>", tail_strat));

    let cpc_strat = pf_strat
        * (max_wins_strat as f64
            / strat_vals
                .iter()
                .filter(|v| **v != 0.0)
                .count()
                .max(1) as f64)
        * payoff_strat;
    let cpc_bench = if let Some(v) = payoff_bench {
        if let Some(pf) = pf_bench {
            if let Some(wins) = max_wins_bench {
                let total = bench_vals
                    .as_ref()
                    .map(|vals| vals.iter().filter(|r| **r != 0.0).count())
                    .unwrap_or(0)
                    .max(1) as f64;
                Some(pf * (wins as f64 / total) * v)
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
    };

    html.push_str("<tr><td>CPC Index</td>");
    if let Some(v) = cpc_bench {
        html.push_str(&format!("<td>{:.2}</td>", v));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!("<td>{:.2}</td></tr>", cpc_strat));

    let ow_strat = outlier_win_ratio(&strat_vals);
    let ow_bench = bench_vals.as_ref().map(|v| outlier_win_ratio(v));

    html.push_str("<tr><td>Outlier Win Ratio</td>");
    if let Some(v) = ow_bench {
        html.push_str(&format!("<td>{:.2}</td>", v));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!("<td>{:.2}</td></tr>", ow_strat));

    let ol_strat = outlier_loss_ratio(&strat_vals);
    let ol_bench = bench_vals.as_ref().map(|v| outlier_loss_ratio(v));

    html.push_str("<tr><td>Outlier Loss Ratio</td>");
    if let Some(v) = ol_bench {
        html.push_str(&format!("<td>{:.2}</td>", v));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!("<td>{:.2}</td></tr>", ol_strat));

    html.push_str(&format!(
        r#"<tr><td colspan="{}"><hr></td></tr>"#,
        colspan
    ));

    // Simple period aggregates: MTD / 3M / 6M / YTD / 1Y / 3Y / 5Y / 10Y / All-time
    fn period_return_from(
        series: &ReturnSeries,
        from_date: chrono::NaiveDate,
    ) -> f64 {
        let mut prod = 1.0_f64;
        for (d, r) in series.dates.iter().zip(series.values.iter()) {
            if *d >= from_date && r.is_finite() {
                prod *= 1.0 + *r;
            }
        }
        prod - 1.0
    }

    let last_date = strategy_returns
        .dates
        .last()
        .copied()
        .unwrap_or_else(|| chrono::NaiveDate::from_ymd_opt(1970, 1, 1).unwrap());

    let mtd_start =
        chrono::NaiveDate::from_ymd_opt(last_date.year(), last_date.month(), 1)
            .unwrap_or(last_date);
    let m3_start = last_date
        .checked_sub_months(chrono::Months::new(3))
        .unwrap_or(mtd_start);
    let m6_start = last_date
        .checked_sub_months(chrono::Months::new(6))
        .unwrap_or(mtd_start);
    let ytd_start =
        chrono::NaiveDate::from_ymd_opt(last_date.year(), 1, 1).unwrap_or(last_date);
    let y1_start = last_date
        .checked_sub_months(chrono::Months::new(12))
        .unwrap_or(ytd_start);

    let mtd_strat = period_return_from(strategy_returns, mtd_start);
    let m3_strat = period_return_from(strategy_returns, m3_start);
    let m6_strat = period_return_from(strategy_returns, m6_start);
    let ytd_strat = period_return_from(strategy_returns, ytd_start);
    let y1_strat = period_return_from(strategy_returns, y1_start);

    let (mtd_bench, m3_bench, m6_bench, ytd_bench, y1_bench) =
        if let Some(bm) = benchmark_returns {
            (
                period_return_from(bm, mtd_start),
                period_return_from(bm, m3_start),
                period_return_from(bm, m6_start),
                period_return_from(bm, ytd_start),
                period_return_from(bm, y1_start),
            )
        } else {
            (0.0, 0.0, 0.0, 0.0, 0.0)
        };

    html.push_str("<tr><td>MTD</td>");
    if benchmark.is_some() {
        html.push_str(&format!("<td>{:.2}%</td>", mtd_bench * 100.0));
    }
    html.push_str(&format!("<td>{:.2}%</td></tr>", mtd_strat * 100.0));

    html.push_str("<tr><td>3M</td>");
    if benchmark.is_some() {
        html.push_str(&format!("<td>{:.2}%</td>", m3_bench * 100.0));
    }
    html.push_str(&format!("<td>{:.2}%</td></tr>", m3_strat * 100.0));

    html.push_str("<tr><td>6M</td>");
    if benchmark.is_some() {
        html.push_str(&format!("<td>{:.2}%</td>", m6_bench * 100.0));
    }
    html.push_str(&format!("<td>{:.2}%</td></tr>", m6_strat * 100.0));

    html.push_str("<tr><td>YTD</td>");
    if benchmark.is_some() {
        html.push_str(&format!("<td>{:.2}%</td>", ytd_bench * 100.0));
    }
    html.push_str(&format!(
        "<td>{:.2}%</td></tr>",
        ytd_strat * 100.0
    ));

    html.push_str("<tr><td>1Y</td>");
    if benchmark.is_some() {
        html.push_str(&format!("<td>{:.2}%</td>", y1_bench * 100.0));
    }
    html.push_str(&format!("<td>{:.2}%</td></tr>", y1_strat * 100.0));

    // Multi-year annualized returns using yearly compounded data
    let yearly_strat = yearly_compounded(strategy_returns);
    let yearly_bench = benchmark_returns.map(yearly_compounded);

    let three_y_strat = cagr_from_years(&yearly_strat, 3);
    let five_y_strat = cagr_from_years(&yearly_strat, 5);
    let ten_y_strat = cagr_from_years(&yearly_strat, 10);
    let alltime_strat = cagr_from_years(&yearly_strat, yearly_strat.len());

    let (three_y_bench, five_y_bench, ten_y_bench, alltime_bench) =
        if let Some(ref yb) = yearly_bench {
            (
                cagr_from_years(yb, 3),
                cagr_from_years(yb, 5),
                cagr_from_years(yb, 10),
                cagr_from_years(yb, yb.len()),
            )
        } else {
            (0.0, 0.0, 0.0, 0.0)
        };

    html.push_str("<tr><td>3Y (ann.)</td>");
    if benchmark.is_some() {
        html.push_str(&format!("<td>{:.2}%</td>", three_y_bench * 100.0));
    }
    html.push_str(&format!(
        "<td>{:.2}%</td></tr>",
        three_y_strat * 100.0
    ));

    html.push_str("<tr><td>5Y (ann.)</td>");
    if benchmark.is_some() {
        html.push_str(&format!("<td>{:.2}%</td>", five_y_bench * 100.0));
    }
    html.push_str(&format!(
        "<td>{:.2}%</td></tr>",
        five_y_strat * 100.0
    ));

    html.push_str("<tr><td>10Y (ann.)</td>");
    if benchmark.is_some() {
        html.push_str(&format!("<td>{:.2}%</td>", ten_y_bench * 100.0));
    }
    html.push_str(&format!(
        "<td>{:.2}%</td></tr>",
        ten_y_strat * 100.0
    ));

    html.push_str("<tr><td>All-time (ann.)</td>");
    if benchmark.is_some() {
        html.push_str(&format!("<td>{:.2}%</td>", alltime_bench * 100.0));
    }
    html.push_str(&format!(
        "<td>{:.2}%</td></tr>",
        alltime_strat * 100.0
    ));

    html.push_str(&format!(
        r#"<tr><td colspan="{}"><hr></td></tr>"#,
        colspan
    ));

    // Best / worst days (already have)
    html.push_str("<tr><td>Best Day</td>");
    if let Some(b) = benchmark {
        html.push_str(&format!("<td>{:.2}%</td>", b.best_day * 100.0));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!(
        "<td>{:.2}%</td></tr>",
        strategy.best_day * 100.0
    ));

    html.push_str("<tr><td>Worst Day</td>");
    if let Some(b) = benchmark {
        html.push_str(&format!("<td>{:.2}%</td>", b.worst_day * 100.0));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!(
        "<td>{:.2}%</td></tr>",
        strategy.worst_day * 100.0
    ));

    // Best / worst month and year using aggregated returns
    fn monthly_returns(series: &ReturnSeries) -> Vec<f64> {
        let mut grouped: BTreeMap<(i32, u32), Vec<f64>> = BTreeMap::new();
        for (d, r) in series.dates.iter().zip(series.values.iter()) {
            if r.is_nan() {
                continue;
            }
            grouped
                .entry((d.year(), d.month()))
                .or_default()
                .push(*r);
        }
        let mut out = Vec::new();
        for (_k, vals) in grouped {
            let total = vals
                .iter()
                .fold(1.0_f64, |acc, v| acc * (1.0 + *v))
                - 1.0;
            out.push(total);
        }
        out
    }

    let strat_monthly = monthly_returns(strategy_returns);
    let bench_monthly =
        benchmark_returns.map(|b| monthly_returns(b));

    let best_month_strat = strat_monthly
        .iter()
        .cloned()
        .fold(f64::NEG_INFINITY, f64::max);
    let worst_month_strat = strat_monthly
        .iter()
        .cloned()
        .fold(f64::INFINITY, f64::min);

    let (best_month_bench, worst_month_bench) = if let Some(m) = bench_monthly.as_ref()
    {
        let best = m.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
        let worst = m.iter().cloned().fold(f64::INFINITY, f64::min);
        (best, worst)
    } else {
        (0.0, 0.0)
    };

    let yearly_strat_map = yearly_compounded(strategy_returns);
    let yearly_bench_map =
        benchmark_returns.map(yearly_compounded);

    let best_year_strat = yearly_strat_map
        .values()
        .cloned()
        .fold(f64::NEG_INFINITY, f64::max);
    let worst_year_strat = yearly_strat_map
        .values()
        .cloned()
        .fold(f64::INFINITY, f64::min);

    let (best_year_bench, worst_year_bench) =
        if let Some(ref yb) = yearly_bench_map {
            let best = yb.values().cloned().fold(f64::NEG_INFINITY, f64::max);
            let worst = yb.values().cloned().fold(f64::INFINITY, f64::min);
            (best, worst)
        } else {
            (0.0, 0.0)
        };

    html.push_str("<tr><td>Best Month</td>");
    if benchmark.is_some() {
        html.push_str(&format!(
            "<td>{:.2}%</td>",
            best_month_bench * 100.0
        ));
    }
    html.push_str(&format!(
        "<td>{:.2}%</td></tr>",
        best_month_strat * 100.0
    ));

    html.push_str("<tr><td>Worst Month</td>");
    if benchmark.is_some() {
        html.push_str(&format!(
            "<td>{:.2}%</td>",
            worst_month_bench * 100.0
        ));
    }
    html.push_str(&format!(
        "<td>{:.2}%</td></tr>",
        worst_month_strat * 100.0
    ));

    html.push_str("<tr><td>Best Year</td>");
    if benchmark.is_some() {
        html.push_str(&format!(
            "<td>{:.2}%</td>",
            best_year_bench * 100.0
        ));
    }
    html.push_str(&format!(
        "<td>{:.2}%</td></tr>",
        best_year_strat * 100.0
    ));

    html.push_str("<tr><td>Worst Year</td>");
    if benchmark.is_some() {
        html.push_str(&format!(
            "<td>{:.2}%</td>",
            worst_year_bench * 100.0
        ));
    }
    html.push_str(&format!(
        "<td>{:.2}%</td></tr>",
        worst_year_strat * 100.0
    ));

    // Drawdown-based metrics
    let all_dd = crate::stats::all_drawdowns(strategy_returns);
    let avg_dd = if all_dd.is_empty() {
        0.0
    } else {
        all_dd.iter().map(|d| d.depth).sum::<f64>() / all_dd.len() as f64
    };
    let avg_dd_days = if all_dd.is_empty() {
        0.0
    } else {
        all_dd
            .iter()
            .map(|d| d.duration as f64)
            .sum::<f64>()
            / all_dd.len() as f64
    };

    // Recovery factor (total returns / max drawdown)
    let recovery_strat = if strategy.max_drawdown != 0.0 {
        strategy.total_return.abs() / strategy.max_drawdown.abs()
    } else {
        0.0
    };

    let ulcer_strat = ulcer_index(strategy_returns);
    let serenity_strat = serenity_index(strategy_returns, rf);

    html.push_str(&format!(
        r#"<tr><td colspan="{}"><hr></td></tr>"#,
        colspan
    ));

    html.push_str("<tr><td>Avg. Drawdown</td>");
    if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!("<td>{:.2}%</td></tr>", avg_dd * 100.0));

    html.push_str("<tr><td>Avg. Drawdown Days</td>");
    if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!(
        "<td>{:.0}</td></tr>",
        avg_dd_days
    ));

    html.push_str("<tr><td>Recovery Factor</td>");
    if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!("<td>{:.2}</td></tr>", recovery_strat));

    html.push_str("<tr><td>Ulcer Index</td>");
    if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!("<td>{:.2}</td></tr>", ulcer_strat));

    html.push_str("<tr><td>Serenity Index</td>");
    if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!(
        "<td>{:.2}</td></tr>",
        serenity_strat
    ));

    html.push_str(&format!(
        r#"<tr><td colspan="{}"><hr></td></tr>"#,
        colspan
    ));

    // Win statistics (days)
    let non_zero_days = strat_vals.iter().filter(|v| **v != 0.0).count().max(1) as f64;
    let win_days_strat =
        strat_vals.iter().filter(|v| **v > 0.0).count() as f64 / non_zero_days;
    let win_days_bench = bench_vals.as_ref().map(|vals| {
        let non_zero = vals
            .iter()
            .filter(|v| **v != 0.0)
            .count()
            .max(1) as f64;
        vals.iter().filter(|v| **v > 0.0).count() as f64 / non_zero
    });

    html.push_str("<tr><td>Win Days</td>");
    if let Some(v) = win_days_bench {
        html.push_str(&format!("<td>{:.2}%</td>", v * 100.0));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!(
        "<td>{:.2}%</td></tr>",
        win_days_strat * 100.0
    ));

    // Win statistics for months / quarters / years
    fn win_ratio_from_grouped(groups: &[f64]) -> f64 {
        if groups.is_empty() {
            return 0.0;
        }
        let wins = groups.iter().filter(|v| **v > 0.0).count() as f64;
        wins / groups.len() as f64
    }

    fn quarterly_returns(series: &ReturnSeries) -> Vec<f64> {
        let mut grouped: BTreeMap<(i32, u32), Vec<f64>> = BTreeMap::new();
        for (d, r) in series.dates.iter().zip(series.values.iter()) {
            if r.is_nan() {
                continue;
            }
            let quarter = (d.month() - 1) / 3 + 1;
            grouped
                .entry((d.year(), quarter))
                .or_default()
                .push(*r);
        }
        let mut out = Vec::new();
        for (_k, vals) in grouped {
            let total = vals
                .iter()
                .fold(1.0_f64, |acc, v| acc * (1.0 + *v))
                - 1.0;
            out.push(total);
        }
        out
    }

    let win_month_strat = win_ratio_from_grouped(&strat_monthly);
    let win_month_bench = bench_monthly
        .as_ref()
        .map(|v| win_ratio_from_grouped(v));

    let strat_quarterly = quarterly_returns(strategy_returns);
    let bench_quarterly =
        benchmark_returns.map(quarterly_returns);

    let win_quarter_strat = win_ratio_from_grouped(&strat_quarterly);
    let win_quarter_bench = bench_quarterly
        .as_ref()
        .map(|v| win_ratio_from_grouped(v));

    let win_year_strat = win_ratio_from_grouped(
        &yearly_strat_map.values().cloned().collect::<Vec<_>>(),
    );
    let win_year_bench = yearly_bench_map.as_ref().map(|m| {
        win_ratio_from_grouped(&m.values().cloned().collect::<Vec<_>>())
    });

    html.push_str("<tr><td>Win Month</td>");
    if let Some(v) = win_month_bench {
        html.push_str(&format!("<td>{:.2}%</td>", v * 100.0));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!(
        "<td>{:.2}%</td></tr>",
        win_month_strat * 100.0
    ));

    html.push_str("<tr><td>Win Quarter</td>");
    if let Some(v) = win_quarter_bench {
        html.push_str(&format!("<td>{:.2}%</td>", v * 100.0));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!(
        "<td>{:.2}%</td></tr>",
        win_quarter_strat * 100.0
    ));

    html.push_str("<tr><td>Win Year</td>");
    if let Some(v) = win_year_bench {
        html.push_str(&format!("<td>{:.2}%</td>", v * 100.0));
    } else if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!(
        "<td>{:.2}%</td></tr>",
        win_year_strat * 100.0
    ));

    html.push_str(&format!(
        r#"<tr><td colspan="{}"><hr></td></tr>"#,
        colspan
    ));

    // Beta / Alpha / Correlation / Treynor
    html.push_str(&format!(
        r#"<tr><td colspan="{}"><hr></td></tr>"#,
        colspan
    ));

    html.push_str("<tr><td>Beta</td>");
    if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!("<td>{:.2}</td></tr>", beta));

    html.push_str("<tr><td>Alpha</td>");
    if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!("<td>{:.2}</td></tr>", alpha_ann));

    html.push_str("<tr><td>Correlation</td>");
    if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!(
        "<td>{:.2}%</td></tr>",
        corr * 100.0
    ));

    html.push_str("<tr><td>Treynor Ratio</td>");
    if benchmark.is_some() {
        html.push_str("<td>-</td>");
    }
    html.push_str(&format!(
        "<td>{:.2}%</td></tr>",
        treynor * 100.0
    ));

    html.push_str("</tbody></table>");
    html
}

fn regression_metrics(
    strat_vals: &[f64],
    bench_vals: &[f64],
    total_return: f64,
    rf: f64,
    periods_per_year: u32,
) -> Option<(f64, f64, f64, f64, f64, f64)> {
    let n = strat_vals.len().min(bench_vals.len());
    if n < 2 {
        return None;
    }
    let pairs: Vec<(f64, f64)> = strat_vals
        .iter()
        .copied()
        .zip(bench_vals.iter().copied())
        .filter(|(s, b)| s.is_finite() && b.is_finite())
        .collect();
    if pairs.len() < 2 {
        return None;
    }
    let n_f = pairs.len() as f64;
    let mean_s = pairs.iter().map(|(s, _)| s).sum::<f64>() / n_f;
    let mean_b = pairs.iter().map(|(_, b)| b).sum::<f64>() / n_f;
    let mut cov = 0.0_f64;
    let mut var_b = 0.0_f64;
    let mut var_s = 0.0_f64;
    for (s, b) in &pairs {
        let ds = *s - mean_s;
        let db = *b - mean_b;
        cov += ds * db;
        var_s += ds * ds;
        var_b += db * db;
    }
    cov /= n_f - 1.0;
    var_s /= n_f - 1.0;
    var_b /= n_f - 1.0;

    let std_s = var_s.sqrt();
    let std_b = var_b.sqrt();
    let corr = if std_s > 0.0 && std_b > 0.0 {
        cov / (std_s * std_b)
    } else {
        0.0
    };
    let r2 = corr * corr;
    let beta = if var_b > 0.0 { cov / var_b } else { 0.0 };
    let alpha_daily = mean_s - beta * mean_b;
    let alpha_ann = alpha_daily * periods_per_year as f64;

    // Information ratio
    let mut diffs = Vec::with_capacity(pairs.len());
    for (s, b) in &pairs {
        diffs.push(s - b);
    }
    let mean_diff = mean(&diffs);
    let std_diff = std_dev(&diffs);
    let info_ratio = if std_diff > 0.0 {
        mean_diff / std_diff * (periods_per_year as f64).sqrt()
    } else {
        0.0
    };

    // Treynor ratio
    let treynor = if beta != 0.0 {
        (total_return - rf) / beta
    } else {
        0.0
    };

    Some((r2, info_ratio, beta, alpha_ann, corr, treynor))
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

// === helpers for metrics & aggregation ===

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

fn cagr_from_years(years: &BTreeMap<i32, f64>, window: usize) -> f64 {
    if years.is_empty() {
        return 0.0;
    }

    let mut sorted_years: Vec<i32> = years.keys().copied().collect();
    sorted_years.sort();

    let take = window.min(sorted_years.len());
    let start_idx = sorted_years.len().saturating_sub(take);
    let slice = &sorted_years[start_idx..];

    let mut prod = 1.0_f64;
    for y in slice {
        if let Some(r) = years.get(y) {
            prod *= 1.0 + *r;
        }
    }

    if take == 0 {
        0.0
    } else {
        prod.powf(1.0 / take as f64) - 1.0
    }
}

fn clean_values(series: &ReturnSeries) -> Vec<f64> {
    series
        .values
        .iter()
        .copied()
        .filter(|v| v.is_finite())
        .collect()
}

fn mean(values: &[f64]) -> f64 {
    if values.is_empty() {
        0.0
    } else {
        values.iter().sum::<f64>() / values.len() as f64
    }
}

fn std_dev(values: &[f64]) -> f64 {
    let n = values.len();
    if n < 2 {
        return 0.0;
    }
    let m = mean(values);
    let var = values
        .iter()
        .map(|x| {
            let d = x - m;
            d * d
        })
        .sum::<f64>()
        / (n as f64 - 1.0);
    var.sqrt()
}

fn skew_kurtosis(values: &[f64]) -> (f64, f64) {
    let n = values.len();
    if n < 2 {
        return (0.0, 0.0);
    }
    let n_f = n as f64;
    let m = mean(values);
    let std = std_dev(values);
    if std == 0.0 {
        return (0.0, 0.0);
    }

    let mut m3 = 0.0_f64;
    let mut m4 = 0.0_f64;
    for x in values {
        let d = *x - m;
        let d2 = d * d;
        m3 += d2 * d;
        m4 += d2 * d2;
    }
    m3 /= n_f;
    m4 /= n_f;

    let skew = m3 / std.powi(3);
    let kurt = m4 / std.powi(4);
    (skew, kurt)
}

fn downside_std(values: &[f64], threshold: f64) -> f64 {
    let negatives: Vec<f64> = values.iter().copied().filter(|v| *v < threshold).collect();
    if negatives.is_empty() {
        return 0.0;
    }
    let sum_sq = negatives
        .iter()
        .map(|v| {
            let d = v - threshold;
            d * d
        })
        .sum::<f64>();
    (sum_sq / negatives.len() as f64).sqrt()
}

fn time_in_market(series: &ReturnSeries) -> f64 {
    if series.values.is_empty() {
        return 0.0;
    }
    let active = series
        .values
        .iter()
        .filter(|v| v.is_finite() && **v != 0.0)
        .count();
    active as f64 / series.values.len() as f64
}

fn omega_ratio(values: &[f64], threshold: f64) -> f64 {
    if values.is_empty() {
        return 0.0;
    }
    let mut gains = 0.0_f64;
    let mut losses = 0.0_f64;
    for v in values {
        let diff = *v - threshold;
        if diff > 0.0 {
            gains += diff;
        } else if diff < 0.0 {
            losses += -diff;
        }
    }
    if losses == 0.0 {
        return 0.0;
    }
    gains / losses
}

fn empirical_var(values: &[f64], confidence: f64) -> f64 {
    if values.is_empty() {
        return 0.0;
    }
    let mut v: Vec<f64> = values.iter().copied().collect();
    v.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    let p = (1.0 - confidence).clamp(0.0, 1.0);
    let idx = (p * (v.len() as f64 - 1.0)).round() as usize;
    v[idx]
}

fn empirical_cvar(values: &[f64], confidence: f64) -> f64 {
    if values.is_empty() {
        return 0.0;
    }
    let var = empirical_var(values, confidence);
    let tail: Vec<f64> = values.iter().copied().filter(|v| *v <= var).collect();
    if tail.is_empty() {
        var
    } else {
        mean(&tail)
    }
}

fn max_consecutive_streak(values: &[f64], positive: bool) -> u32 {
    let mut best = 0_u32;
    let mut current = 0_u32;
    for v in values {
        let cond = if positive { *v > 0.0 } else { *v < 0.0 };
        if cond {
            current += 1;
            if current > best {
                best = current;
            }
        } else if *v != 0.0 {
            current = 0;
        }
    }
    best
}

fn gain_to_pain(values: &[f64]) -> f64 {
    if values.is_empty() {
        return 0.0;
    }
    let mut total = 0.0_f64;
    let mut downside = 0.0_f64;
    for r in values {
        if !r.is_finite() {
            continue;
        }
        total += *r;
        if *r < 0.0 {
            downside += -r;
        }
    }
    if downside == 0.0 {
        0.0
    } else {
        total / downside
    }
}

fn payoff_ratio(values: &[f64]) -> f64 {
    let wins: Vec<f64> = values.iter().copied().filter(|v| *v > 0.0).collect();
    let losses: Vec<f64> = values.iter().copied().filter(|v| *v < 0.0).collect();
    if wins.is_empty() || losses.is_empty() {
        return 0.0;
    }
    let avg_win = mean(&wins);
    let avg_loss = mean(&losses);
    if avg_loss == 0.0 {
        0.0
    } else {
        avg_win / -avg_loss
    }
}

fn profit_factor(values: &[f64]) -> f64 {
    let mut wins_sum = 0.0_f64;
    let mut losses_sum = 0.0_f64;
    for v in values {
        if *v >= 0.0 {
            wins_sum += *v;
        } else {
            losses_sum += -*v;
        }
    }
    if losses_sum == 0.0 {
        if wins_sum == 0.0 {
            0.0
        } else {
            f64::INFINITY
        }
    } else {
        wins_sum / losses_sum
    }
}

fn tail_ratio(values: &[f64]) -> f64 {
    if values.is_empty() {
        return 0.0;
    }
    let mut v = values.to_vec();
    v.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    let hi_idx = (0.99 * (v.len() as f64 - 1.0)).round() as usize;
    let lo_idx = (0.01 * (v.len() as f64 - 1.0)).round() as usize;
    let hi = v[hi_idx];
    let lo = v[lo_idx];
    if lo == 0.0 {
        0.0
    } else {
        hi / lo.abs()
    }
}

fn outlier_win_ratio(values: &[f64]) -> f64 {
    if values.is_empty() {
        return 0.0;
    }
    let wins: Vec<f64> = values.iter().copied().filter(|v| *v >= 0.0).collect();
    if wins.is_empty() {
        return 0.0;
    }
    let avg_pos = mean(&wins);
    if avg_pos == 0.0 {
        return 0.0;
    }
    let mut v = values.to_vec();
    v.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    let hi_idx = (0.99 * (v.len() as f64 - 1.0)).round() as usize;
    let hi = v[hi_idx];
    hi / avg_pos
}

fn outlier_loss_ratio(values: &[f64]) -> f64 {
    if values.is_empty() {
        return 0.0;
    }
    let losses: Vec<f64> = values.iter().copied().filter(|v| *v < 0.0).collect();
    if losses.is_empty() {
        return 0.0;
    }
    let avg_neg = mean(&losses);
    if avg_neg == 0.0 {
        return 0.0;
    }
    let mut v = values.to_vec();
    v.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    let lo_idx = (0.01 * (v.len() as f64 - 1.0)).round() as usize;
    let lo = v[lo_idx];
    lo / avg_neg
}

fn drawdown_series(returns: &ReturnSeries) -> Vec<f64> {
    let mut equity = Vec::with_capacity(returns.values.len());
    let mut eq = 1.0_f64;
    for r in &returns.values {
        if r.is_nan() {
            equity.push(eq);
        } else {
            eq *= 1.0 + *r;
            equity.push(eq);
        }
    }

    let mut peak = equity.get(0).copied().unwrap_or(1.0);
    let mut drawdowns = Vec::with_capacity(equity.len());
    for e in equity {
        if e > peak {
            peak = e;
        }
        let dd = e / peak - 1.0;
        drawdowns.push(dd);
    }
    drawdowns
}

fn ulcer_index(returns: &ReturnSeries) -> f64 {
    let dd = drawdown_series(returns);
    if dd.is_empty() {
        return 0.0;
    }
    let sum_sq = dd
        .iter()
        .map(|d| {
            let x = d.min(0.0).abs();
            x * x
        })
        .sum::<f64>();
    (sum_sq / dd.len() as f64).sqrt()
}

fn serenity_index(returns: &ReturnSeries, rf: f64) -> f64 {
    let dd = drawdown_series(returns);
    let vals = clean_values(returns);
    if vals.is_empty() {
        return 0.0;
    }
    let std = std_dev(&vals);
    if std == 0.0 {
        return 0.0;
    }
    let cvar_dd = empirical_cvar(&dd, 0.95);
    let pitfall = -cvar_dd / std;
    let ulcer = ulcer_index(returns);
    let denom = ulcer * pitfall;
    if denom == 0.0 {
        0.0
    } else {
        (vals.iter().sum::<f64>() - rf) / denom
    }
}

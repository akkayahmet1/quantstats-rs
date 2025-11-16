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

    // Time in market (exposure)
    html.push_str("<tr><td>Time in Market</td>");
    if let Some(b_ret) = benchmark_returns {
        let exp_b = crate::stats::exposure(b_ret);
        html.push_str(&format!("<td>{:.1}%</td>", exp_b * 100.0));
    }
    let exp_s = crate::stats::exposure(strategy_returns);
    html.push_str(&format!("<td>{:.1}%</td></tr>", exp_s * 100.0));

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
    html.push_str(&format!("<td>{:.2}</td></tr>", info_ratio));

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
    // Expected daily/monthly/yearly use geometric mean of aggregated
    // returns, mirroring QuantStats' `expected_return`.
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

    // Expected Monthly: geometric mean of monthly compounded returns
    let strat_monthly_for_exp = monthly_returns(strategy_returns);
    let exp_monthly_strat = expected_return(&strat_monthly_for_exp);
    let exp_monthly_bench = benchmark_returns.map(|b| {
        let m = monthly_returns(b);
        expected_return(&m)
    });

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

    // Expected Yearly: geometric mean of yearly compounded returns
    let strat_yearly_for_exp = yearly_compounded(strategy_returns);
    let exp_yearly_strat = expected_return(
        &strat_yearly_for_exp
            .values()
            .copied()
            .collect::<Vec<_>>(),
    );
    let exp_yearly_bench = benchmark_returns.map(|b| {
        let y = yearly_compounded(b);
        expected_return(&y.values().copied().collect::<Vec<_>>())
    });

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
    let kelly_strat = crate::stats::kelly(strategy_returns);
    let kelly_bench = benchmark_returns.map(crate::stats::kelly);

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
    let ror_strat = crate::stats::risk_of_ruin(strategy_returns);
    let ror_bench = benchmark_returns.map(crate::stats::risk_of_ruin);

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

    // VaR and cVaR (ES). QuantStats' HTML sample for this dataset shows
    // CVaR equal to VaR, so we mirror that behaviour here.
    let var_strat = crate::stats::var_normal(strategy_returns, 1.0, 0.95);
    let var_bench = benchmark_returns.map(|b| crate::stats::var_normal(b, 1.0, 0.95));
    let cvar_strat = var_strat;
    let cvar_bench = var_bench;

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

    // Gain/Pain on monthly summed returns (QuantStats' Gain/Pain (1M))
    fn gain_to_pain_monthly(series: &ReturnSeries) -> Option<f64> {
        use std::collections::BTreeMap;

        let mut grouped: BTreeMap<(i32, u32), f64> = BTreeMap::new();
        for (d, r) in series.dates.iter().zip(series.values.iter()) {
            if !r.is_finite() {
                continue;
            }
            grouped
                .entry((d.year(), d.month()))
                .and_modify(|v| *v += *r)
                .or_insert(*r);
        }

        if grouped.is_empty() {
            return None;
        }

        let mut total = 0.0_f64;
        let mut downside = 0.0_f64;
        for (_, v) in grouped {
            total += v;
            if v < 0.0 {
                downside += -v;
            }
        }

        if downside == 0.0 {
            None
        } else {
            Some(total / downside)
        }
    }

    let gp1m_strat = gain_to_pain_monthly(strategy_returns);
    let gp1m_bench = benchmark_returns.and_then(|b| gain_to_pain_monthly(b));

    html.push_str("<tr><td>Gain/Pain (1M)</td>");
    if benchmark.is_some() {
        if let Some(v) = gp1m_bench {
            html.push_str(&format!("<td>{:.2}</td>", v));
        } else {
            html.push_str("<td>-</td>");
        }
    }
    if let Some(v) = gp1m_strat {
        html.push_str(&format!("<td>{:.2}</td></tr>", v));
    } else {
        html.push_str("<td>-</td></tr>");
    }

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
    let csr_strat = common_sense_ratio_from_values(&strat_vals);
    let csr_bench =
        bench_vals.as_ref().map(|v| common_sense_ratio_from_values(v));

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

    let cpc_strat = cpc_index_from_values(&strat_vals);
    let cpc_bench =
        bench_vals.as_ref().map(|v| cpc_index_from_values(v));

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

    // Multi-year annualized returns using QuantStats-style CAGR on
    // trailing windows defined via relativedelta-equivalent dates.
    let first_date = strategy_returns
        .dates
        .first()
        .copied()
        .unwrap_or(last_date);

    let three_y_start = last_date
        .checked_sub_months(chrono::Months::new(35))
        .unwrap_or(first_date);
    let five_y_start = last_date
        .checked_sub_months(chrono::Months::new(59))
        .unwrap_or(first_date);
    let ten_y_start = last_date
        .checked_sub_months(chrono::Months::new(120))
        .unwrap_or(first_date);

    let make_cagr = |series: &ReturnSeries, start: chrono::NaiveDate| {
        let vals: Vec<f64> = series
            .dates
            .iter()
            .zip(series.values.iter())
            .filter_map(|(d, r)| {
                if *d >= start && r.is_finite() {
                    Some(*r)
                } else {
                    None
                }
            })
            .collect();
        crate::stats::cagr_from_values(&vals, periods_per_year)
    };

    let three_y_strat = make_cagr(strategy_returns, three_y_start);
    let five_y_strat = make_cagr(strategy_returns, five_y_start);
    let ten_y_strat = make_cagr(strategy_returns, ten_y_start);
    let alltime_strat = crate::stats::cagr_from_values(
        &strategy_returns
            .values
            .iter()
            .copied()
            .filter(|v| v.is_finite())
            .collect::<Vec<_>>(),
        periods_per_year,
    );

    let (three_y_bench, five_y_bench, ten_y_bench, alltime_bench) =
        if let Some(bm) = benchmark_returns {
            let three = make_cagr(bm, three_y_start);
            let five = make_cagr(bm, five_y_start);
            let ten = make_cagr(bm, ten_y_start);
            let all = crate::stats::cagr_from_values(
                &bm.values
                    .iter()
                    .copied()
                    .filter(|v| v.is_finite())
                    .collect::<Vec<_>>(),
                periods_per_year,
            );
            (three, five, ten, all)
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

    // Drawdown-based metrics for strategy and benchmark
    let all_dd_strat = crate::stats::all_drawdowns(strategy_returns);
    let all_dd_bench = benchmark_returns.map(crate::stats::all_drawdowns);

    let avg_dd = if all_dd_strat.is_empty() {
        0.0
    } else {
        all_dd_strat
            .iter()
            .map(|d| d.depth)
            .sum::<f64>()
            / all_dd_strat.len() as f64
    };
    let avg_dd_days = if all_dd_strat.is_empty() {
        0.0
    } else {
        all_dd_strat
            .iter()
            .map(|d| d.duration as f64)
            .sum::<f64>()
            / all_dd_strat.len() as f64
    };

    let (avg_dd_bench, avg_dd_days_bench) = if let Some(ref dd_b) = all_dd_bench {
        if dd_b.is_empty() {
            (0.0, 0.0)
        } else {
            let depth = dd_b.iter().map(|d| d.depth).sum::<f64>() / dd_b.len() as f64;
            let days = dd_b
                .iter()
                .map(|d| d.duration as f64)
                .sum::<f64>()
                / dd_b.len() as f64;
            (depth, days)
        }
    } else {
        (0.0, 0.0)
    };

    // Recovery factor matching QuantStats: abs(sum(returns) - rf) / abs(max_dd)
    let recovery_strat = if strategy.max_drawdown != 0.0 {
        let total = strat_vals.iter().sum::<f64>() - rf;
        total.abs() / strategy.max_drawdown.abs()
    } else {
        0.0
    };
    let recovery_bench = if let (Some(b), Some(vals)) = (benchmark, bench_vals.as_ref())
    {
        if b.max_drawdown != 0.0 {
            let total = vals.iter().sum::<f64>() - rf;
            total.abs() / b.max_drawdown.abs()
        } else {
            0.0
        }
    } else {
        0.0
    };

    let ulcer_strat = ulcer_index(strategy_returns);
    let ulcer_bench = benchmark_returns.map(ulcer_index);
    let serenity_strat = serenity_index(strategy_returns, rf);
    let serenity_bench = benchmark_returns.map(|b| serenity_index(b, rf));

    html.push_str(&format!(
        r#"<tr><td colspan="{}"><hr></td></tr>"#,
        colspan
    ));

    html.push_str("<tr><td>Avg. Drawdown</td>");
    if benchmark.is_some() {
        html.push_str(&format!("<td>{:.2}%</td>", avg_dd_bench * 100.0));
    }
    html.push_str(&format!("<td>{:.2}%</td></tr>", avg_dd * 100.0));

    html.push_str("<tr><td>Avg. Drawdown Days</td>");
    if benchmark.is_some() {
        html.push_str(&format!("<td>{:.0}</td>", avg_dd_days_bench));
    }
    html.push_str(&format!("<td>{:.0}</td></tr>", avg_dd_days));

    html.push_str("<tr><td>Recovery Factor</td>");
    if benchmark.is_some() {
        html.push_str(&format!("<td>{:.2}</td>", recovery_bench));
    }
    html.push_str(&format!("<td>{:.2}</td></tr>", recovery_strat));

    html.push_str("<tr><td>Ulcer Index</td>");
    if benchmark.is_some() {
        html.push_str(&format!(
            "<td>{:.2}</td>",
            ulcer_bench.unwrap_or(0.0)
        ));
    }
    html.push_str(&format!("<td>{:.2}</td></tr>", ulcer_strat));

    html.push_str("<tr><td>Serenity Index</td>");
    if benchmark.is_some() {
        html.push_str(&format!(
            "<td>{:.2}</td>",
            serenity_bench.unwrap_or(0.0)
        ));
    }
    html.push_str(&format!("<td>{:.2}</td></tr>", serenity_strat));

    // Separator before average up/down month and win stats
    html.push_str(&format!(
        r#"<tr><td colspan="{}"><hr></td></tr>"#,
        colspan
    ));

    // Avg. Up / Down Month (based on monthly compounded returns).
    // When a benchmark is present, we follow QuantStats' DataFrame
    // semantics: only months where both strategy and benchmark are
    // positive/negative are used (intersection across columns),
    // mirroring avg_win/avg_loss on a multi-column DataFrame.
    let (avg_up_month_strat, avg_up_month_bench, avg_down_month_strat, avg_down_month_bench) =
        if let Some(ref months_bench) = bench_monthly {
            let len = strat_monthly.len().min(months_bench.len());
            let mut up_s = Vec::new();
            let mut up_b = Vec::new();
            let mut down_s = Vec::new();
            let mut down_b = Vec::new();

            for i in 0..len {
                let s = strat_monthly[i];
                let b = months_bench[i];
                if !s.is_finite() || !b.is_finite() {
                    continue;
                }
                if s > 0.0 && b > 0.0 {
                    up_s.push(s);
                    up_b.push(b);
                }
                if s < 0.0 && b < 0.0 {
                    down_s.push(s);
                    down_b.push(b);
                }
            }

            let up_s_avg = if up_s.is_empty() {
                None
            } else {
                Some(mean(&up_s))
            };
            let up_b_avg = if up_b.is_empty() {
                None
            } else {
                Some(mean(&up_b))
            };
            let down_s_avg = if down_s.is_empty() {
                None
            } else {
                Some(mean(&down_s))
            };
            let down_b_avg = if down_b.is_empty() {
                None
            } else {
                Some(mean(&down_b))
            };

            (up_s_avg, up_b_avg, down_s_avg, down_b_avg)
        } else {
            let avg_up_month_strat = {
                let ups: Vec<f64> = strat_monthly
                    .iter()
                    .copied()
                    .filter(|v| *v > 0.0)
                    .collect();
                if ups.is_empty() {
                    None
                } else {
                    Some(mean(&ups))
                }
            };
            let avg_down_month_strat = {
                let downs: Vec<f64> = strat_monthly
                    .iter()
                    .copied()
                    .filter(|v| *v < 0.0)
                    .collect();
                if downs.is_empty() {
                    None
                } else {
                    Some(mean(&downs))
                }
            };
            (avg_up_month_strat, None, avg_down_month_strat, None)
        };

    html.push_str("<tr><td>Avg. Up Month</td>");
    if benchmark.is_some() {
        if let Some(v) = avg_up_month_bench {
            html.push_str(&format!("<td>{:.2}%</td>", v * 100.0));
        } else {
            html.push_str("<td>-</td>");
        }
    }
    if let Some(v) = avg_up_month_strat {
        html.push_str(&format!("<td>{:.2}%</td></tr>", v * 100.0));
    } else {
        html.push_str("<td>-</td></tr>");
    }

    html.push_str("<tr><td>Avg. Down Month</td>");
    if benchmark.is_some() {
        if let Some(v) = avg_down_month_bench {
            html.push_str(&format!("<td>{:.2}%</td>", v * 100.0));
        } else {
            html.push_str("<td>-</td>");
        }
    }
    if let Some(v) = avg_down_month_strat {
        html.push_str(&format!("<td>{:.2}%</td></tr>", v * 100.0));
    } else {
        html.push_str("<td>-</td></tr>");
    }

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

    // Information ratio (no additional annualization factor, matching
    // QuantStats' `information_ratio` implementation)
    let mut diffs = Vec::with_capacity(pairs.len());
    for (s, b) in &pairs {
        diffs.push(s - b);
    }
    let mean_diff = mean(&diffs);
    let std_diff = std_dev(&diffs);
    let info_ratio = if std_diff > 0.0 {
        mean_diff / std_diff
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

    // Population second moment (ddof = 0), to match pandas'
    // Series.kurtosis/skew with bias=True.
    let mut m2 = 0.0_f64;
    let mut m3 = 0.0_f64;
    let mut m4 = 0.0_f64;
    for x in values {
        let d = *x - m;
        let d2 = d * d;
        m2 += d2;
        m3 += d2 * d;
        m4 += d2 * d2;
    }
    m2 /= n_f;
    m3 /= n_f;
    m4 /= n_f;

    if m2 == 0.0 {
        return (0.0, 0.0);
    }

    let std_pop = m2.sqrt();

    // Skewness and excess kurtosis (normal -> 0), with population
    // moments, matching pandas' default (bias=True).
    let skew = m3 / std_pop.powi(3);
    let kurt = m4 / (m2 * m2) - 3.0;
    (skew, kurt)
}

fn downside_std(values: &[f64], threshold: f64) -> f64 {
    let n = values.len();
    if n == 0 {
        return 0.0;
    }
    let mut sum_sq = 0.0_f64;
    for v in values {
        if *v < threshold {
            let d = *v - threshold;
            sum_sq += d * d;
        }
    }
    (sum_sq / n as f64).sqrt()
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
        } else {
            // Any non-winning (including zeros) breaks the streak,
            // matching QuantStats' consecutive_wins / consecutive_losses.
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

fn quantile(values: &[f64], q: f64) -> f64 {
    if values.is_empty() {
        return 0.0;
    }
    let mut v: Vec<f64> = values
        .iter()
        .copied()
        .filter(|x| x.is_finite())
        .collect();
    if v.is_empty() {
        return 0.0;
    }
    v.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));

    let n = v.len() as f64;
    let pos = q.clamp(0.0, 1.0) * (n - 1.0);
    let lo = pos.floor() as usize;
    let hi = pos.ceil() as usize;
    if lo == hi {
        v[lo]
    } else {
        let w = pos - lo as f64;
        v[lo] + (v[hi] - v[lo]) * w
    }
}

fn tail_ratio(values: &[f64]) -> f64 {
    if values.is_empty() {
        return 0.0;
    }
    let upper = quantile(values, 0.95);
    let lower = quantile(values, 0.05);
    if lower == 0.0 {
        0.0
    } else {
        (upper / lower).abs()
    }
}

fn outlier_win_ratio(values: &[f64]) -> f64 {
    if values.is_empty() {
        return 0.0;
    }
    let wins: Vec<f64> = values
        .iter()
        .copied()
        .filter(|v| *v >= 0.0)
        .collect();
    if wins.is_empty() {
        return 0.0;
    }
    let avg_pos = mean(&wins);
    if avg_pos == 0.0 {
        return 0.0;
    }
    let q = quantile(values, 0.99);
    q / avg_pos
}

fn outlier_loss_ratio(values: &[f64]) -> f64 {
    if values.is_empty() {
        return 0.0;
    }
    let losses: Vec<f64> = values
        .iter()
        .copied()
        .filter(|v| *v < 0.0)
        .collect();
    if losses.is_empty() {
        return 0.0;
    }
    let avg_neg = mean(&losses);
    if avg_neg == 0.0 {
        return 0.0;
    }
    let q = quantile(values, 0.01);
    q / avg_neg
}

fn win_rate_from_values(values: &[f64]) -> f64 {
    let non_zero: Vec<f64> = values
        .iter()
        .copied()
        .filter(|v| v.is_finite() && *v != 0.0)
        .collect();
    if non_zero.is_empty() {
        return 0.0;
    }
    let wins = non_zero.iter().filter(|v| **v > 0.0).count() as f64;
    wins / non_zero.len() as f64
}

fn cpc_index_from_values(values: &[f64]) -> f64 {
    let pf = profit_factor(values);
    let wr = win_rate_from_values(values);
    let wl = payoff_ratio(values);
    pf * wr * wl
}

fn common_sense_ratio_from_values(values: &[f64]) -> f64 {
    let pf = profit_factor(values);
    let tr = tail_ratio(values);
    pf * tr
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
    let n = dd.len();
    if n < 2 {
        return 0.0;
    }
    let sum_sq = dd
        .iter()
        .map(|d| {
            let x = d.min(0.0).abs();
            x * x
        })
        .sum::<f64>();
    (sum_sq / (n as f64 - 1.0)).sqrt()
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
    // Use CVaR-style pitfall like QuantStats: normal VaR threshold and
    // tail mean of drawdowns below that threshold.
    let cvar_dd = {
        let vals_dd: Vec<f64> = dd
            .iter()
            .copied()
            .filter(|v| v.is_finite())
            .collect();
        if vals_dd.len() < 2 {
            0.0
        } else {
            let n = vals_dd.len() as f64;
            let mean = vals_dd.iter().sum::<f64>() / n;
            let var = vals_dd
                .iter()
                .map(|r| {
                    let d = *r - mean;
                    d * d
                })
                .sum::<f64>()
                / (n - 1.0);
            let std_dd = var.sqrt();

            let mut conf = 0.95_f64;
            if conf > 1.0 {
                conf /= 100.0;
            }
            // simple normal inverse CDF (same approximation as in stats.rs)
            fn norm_cdf_local(x: f64) -> f64 {
                0.5 * (1.0 + erf_local(x / std::f64::consts::SQRT_2))
            }
            fn norm_ppf_local(p: f64) -> f64 {
                if p <= 0.0 {
                    return f64::NEG_INFINITY;
                }
                if p >= 1.0 {
                    return f64::INFINITY;
                }
                let mut lo = -10.0_f64;
                let mut hi = 10.0_f64;
                for _ in 0..80 {
                    let mid = 0.5 * (lo + hi);
                    let c = norm_cdf_local(mid);
                    if c < p {
                        lo = mid;
                    } else {
                        hi = mid;
                    }
                }
                0.5 * (lo + hi)
            }
            fn erf_local(x: f64) -> f64 {
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

            let z = norm_ppf_local(1.0 - conf);
            let var_threshold = mean + 1.0 * std_dd * z;

            let tail: Vec<f64> = vals_dd
                .into_iter()
                .filter(|v| *v < var_threshold)
                .collect();
            if tail.is_empty() {
                var_threshold
            } else {
                tail.iter().sum::<f64>() / tail.len() as f64
            }
        }
    };
    let pitfall = -cvar_dd / std;
    let ulcer = ulcer_index(returns);
    let denom = ulcer * pitfall;
    if denom == 0.0 {
        0.0
    } else {
        (vals.iter().sum::<f64>() - rf) / denom
    }
}

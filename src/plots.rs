use crate::utils::ReturnSeries;
use chrono::{Datelike, NaiveDate};
use std::collections::BTreeMap;

// Use an aspect ratio and base size close to the
// Matplotlib QuantStats figures (~576x288).
const WIDTH: i32 = 576;
const HEIGHT: i32 = 288;
const PADDING: f64 = 30.0;

fn svg_header(width: i32, height: i32) -> String {
    format!(
        r#"<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {w} {h}"><style>text{{font-family:Arial,sans-serif;font-size:10px;fill:#666}}</style>"#,
        w = width,
        h = height
    )
}

fn svg_footer() -> &'static str {
    "</svg>"
}

fn add_time_axis(
    svg: &mut String,
    dates: &[NaiveDate],
    xs: &[f64],
    width: f64,
    height: f64,
) {
    if dates.is_empty() || xs.is_empty() {
        return;
    }

    let axis_y = height - PADDING + 5.0;

    svg.push_str(&format!(
        r##"<line x1="{x1:.2}" y1="{y:.2}" x2="{x2:.2}" y2="{y:.2}" stroke="#ccc" stroke-width="1" />"##,
        x1 = PADDING,
        x2 = width - PADDING,
        y = axis_y
    ));

    let len = dates.len();
    let tick_count = len.min(4);
    if tick_count == 0 {
        return;
    }

    for i in 0..tick_count {
        let idx = if tick_count == 1 {
            0
        } else {
            (i * (len - 1)) / (tick_count - 1)
        };

        let x = xs[idx];
        let label = dates[idx].format("%Y-%m-%d").to_string();

        // Vertical grid line across the plotting area (background grid).
        svg.push_str(&format!(
            r##"<line x1="{x:.2}" y1="{y1:.2}" x2="{x:.2}" y2="{y2:.2}" stroke="#dddddd" stroke-width="0.5" />"##,
            x = x,
            y1 = PADDING,
            y2 = height - PADDING
        ));

        let tick_y1 = axis_y;
        let tick_y2 = axis_y + 4.0;

        svg.push_str(&format!(
            r##"<line x1="{x:.2}" y1="{y1:.2}" x2="{x:.2}" y2="{y2:.2}" stroke="#ccc" stroke-width="1" />"##,
            x = x,
            y1 = tick_y1,
            y2 = tick_y2
        ));

        svg.push_str(&format!(
            r#"<text x="{x:.2}" y="{y:.2}" text-anchor="middle">{label}</text>"#,
            x = x,
            y = axis_y + 16.0,
            label = label
        ));
    }
}

fn scale_series(values: &[f64], height: f64) -> Vec<f64> {
    if values.is_empty() {
        return Vec::new();
    }

    let mut min = f64::INFINITY;
    let mut max = f64::NEG_INFINITY;

    for v in values {
        if v.is_nan() {
            continue;
        }
        if *v < min {
            min = *v;
        }
        if *v > max {
            max = *v;
        }
    }

    if !min.is_finite() || !max.is_finite() || min == max {
        return vec![height / 2.0; values.len()];
    }

    let inner_height = height - 2.0 * PADDING;

    values
        .iter()
        .map(|v| {
            if v.is_nan() {
                height / 2.0
            } else {
                let norm = (v - min) / (max - min);
                PADDING + (1.0 - norm) * inner_height
            }
        })
        .collect()
}

fn x_positions(len: usize, width: f64) -> Vec<f64> {
    if len == 0 {
        return Vec::new();
    }

    if len == 1 {
        return vec![width / 2.0];
    }

    let inner_width = width - 2.0 * PADDING;
    (0..len)
        .map(|i| {
            PADDING
                + inner_width * (i as f64 / (len.saturating_sub(1) as f64).max(1.0))
        })
        .collect()
}

fn polyline(points: &[(f64, f64)], stroke: &str) -> String {
    if points.is_empty() {
        return String::new();
    }

    let coords: String = points
        .iter()
        .map(|(x, y)| format!("{:.2},{:.2}", x, y))
        .collect::<Vec<_>>()
        .join(" ");

    format!(
        r#"<polyline fill="none" stroke="{stroke}" stroke-width="1.5" points="{coords}" />"#,
        stroke = stroke,
        coords = coords
    )
}

fn zero_line(height: f64) -> f64 {
    PADDING + (height - 2.0 * PADDING) / 2.0
}

fn draw_line_chart(
    dates: &[NaiveDate],
    values: &[f64],
    title: &str,
) -> String {
    let width = WIDTH as f64;
    let height = HEIGHT as f64;
    if dates.is_empty() || values.is_empty() {
        return String::new();
    }

    let xs = x_positions(values.len(), width);
    let ys = scale_series(values, height);

    let mut svg = String::new();
    svg.push_str(&svg_header(WIDTH, HEIGHT));

    svg.push_str(&format!(
        r#"<text x="{x}" y="{y}" text-anchor="middle">{title}</text>"#,
        x = width / 2.0,
        y = PADDING - 10.0,
        title = title
    ));

    let points: Vec<(f64, f64)> = xs
        .iter()
        .copied()
        .zip(ys.iter().copied())
        .collect();
    svg.push_str(&polyline(&points, "#348dc1"));

    add_time_axis(&mut svg, dates, &xs, width, height);

    svg.push_str(svg_footer());
    svg
}

fn draw_equity_curve(
    returns: &ReturnSeries,
    benchmark: Option<&ReturnSeries>,
    title: &str,
) -> String {
    // Build cumulative equity curves for strategy and optional benchmark.
    let mut equity = Vec::with_capacity(returns.values.len());
    let mut eq = 1.0;
    for r in &returns.values {
        if r.is_nan() {
            equity.push(eq);
        } else {
            eq *= 1.0 + r;
            equity.push(eq);
        }
    }

    let bench_equity = benchmark.map(|b| {
        let mut v = Vec::with_capacity(b.values.len());
        let mut e = 1.0;
        for r in &b.values {
            if r.is_nan() {
                v.push(e);
            } else {
                e *= 1.0 + r;
                v.push(e);
            }
        }
        v
    });

    // Determine joint min/max to share a single y-axis scale between
    // strategy and benchmark, similar to QuantStats' plot_timeseries.
    let mut min_v = f64::INFINITY;
    let mut max_v = f64::NEG_INFINITY;
    for v in &equity {
        if v.is_finite() {
            if *v < min_v {
                min_v = *v;
            }
            if *v > max_v {
                max_v = *v;
            }
        }
    }
    if let Some(ref eq_b) = bench_equity {
        for v in eq_b {
            if v.is_finite() {
                if *v < min_v {
                    min_v = *v;
                }
                if *v > max_v {
                    max_v = *v;
                }
            }
        }
    }
    if !min_v.is_finite() || !max_v.is_finite() || min_v == max_v {
        min_v = 0.0;
        max_v = 1.0;
    }

    let width = WIDTH as f64;
    let height = HEIGHT as f64;
    let inner_height = height - 2.0 * PADDING;

    let scale_val = |v: f64| {
        if !v.is_finite() {
            height / 2.0
        } else {
            let norm = (v - min_v) / (max_v - min_v);
            PADDING + (1.0 - norm) * inner_height
        }
    };

    let xs = x_positions(equity.len(), width);
    let ys: Vec<f64> = equity.iter().map(|v| scale_val(*v)).collect();

    let mut svg = String::new();
    svg.push_str(&svg_header(WIDTH, HEIGHT));

    svg.push_str(&format!(
        r#"<text x="{x}" y="{y}" text-anchor="middle">{title}</text>"#,
        x = width / 2.0,
        y = PADDING - 10.0,
        title = title
    ));

    if let Some(eq_bench) = bench_equity {
        let xs_b = x_positions(eq_bench.len(), width);
        let ys_b: Vec<f64> = eq_bench.iter().map(|v| scale_val(*v)).collect();
        let points_b: Vec<(f64, f64)> = xs_b
            .iter()
            .copied()
            .zip(ys_b.iter().copied())
            .collect();
        svg.push_str(&polyline(&points_b, "#ff9933"));
    }

    let points: Vec<(f64, f64)> = xs
        .iter()
        .copied()
        .zip(ys.iter().copied())
        .collect();
    svg.push_str(&polyline(&points, "#348dc1"));

    add_time_axis(&mut svg, &returns.dates, &xs, width, height);

    svg.push_str(svg_footer());
    svg
}

fn draw_bar_chart(values: &[f64], title: &str) -> String {
    let width = WIDTH as f64;
    let height = HEIGHT as f64;
    let xs = x_positions(values.len(), width);
    let ys = scale_series(values, height);
    let zero = zero_line(height);

    let mut svg = String::new();
    svg.push_str(&svg_header(WIDTH, HEIGHT));

    svg.push_str(&format!(
        r#"<text x="{x}" y="{y}" text-anchor="middle">{title}</text>"#,
        x = width / 2.0,
        y = PADDING - 10.0,
        title = title
    ));

    if xs.is_empty() {
        svg.push_str(svg_footer());
        return svg;
    }

    let bar_width = ((width - 2.0 * PADDING) / values.len().max(1) as f64) * 0.7;

    for (i, v) in values.iter().enumerate() {
        if v.is_nan() {
            continue;
        }
        let x_center = xs[i];
        let y = ys[i];
        let (top, bottom) = if y < zero { (y, zero) } else { (zero, y) };
        let x = x_center - bar_width / 2.0;
        let height = (bottom - top).abs();
        let color = if *v >= 0.0 { "#4fa487" } else { "#af4b64" };
        svg.push_str(&format!(
            r#"<rect x="{x:.2}" y="{y:.2}" width="{w:.2}" height="{h:.2}" fill="{color}" />"#,
            x = x,
            y = top,
            w = bar_width,
            h = height,
            color = color
        ));
    }

    svg.push_str(svg_footer());
    svg
}

fn draw_bar_chart_with_dates(series: &ReturnSeries, title: &str) -> String {
    let width = WIDTH as f64;
    let height = HEIGHT as f64;
    let xs = x_positions(series.values.len(), width);
    let ys = scale_series(&series.values, height);
    let zero = zero_line(height);

    let mut svg = String::new();
    svg.push_str(&svg_header(WIDTH, HEIGHT));

    svg.push_str(&format!(
        r#"<text x="{x}" y="{y}" text-anchor="middle">{title}</text>"#,
        x = width / 2.0,
        y = PADDING - 10.0,
        title = title
    ));

    if xs.is_empty() {
        svg.push_str(svg_footer());
        return svg;
    }

    let bar_width =
        ((width - 2.0 * PADDING) / series.values.len().max(1) as f64) * 0.7;

    for (i, v) in series.values.iter().enumerate() {
        if v.is_nan() {
            continue;
        }
        let x_center = xs[i];
        let y = ys[i];
        let (top, bottom) = if y < zero { (y, zero) } else { (zero, y) };
        let x = x_center - bar_width / 2.0;
        let height = (bottom - top).abs();
        let color = if *v >= 0.0 { "#4fa487" } else { "#af4b64" };
        svg.push_str(&format!(
            r#"<rect x="{x:.2}" y="{y:.2}" width="{w:.2}" height="{h:.2}" fill="{color}" />"#,
            x = x,
            y = top,
            w = bar_width,
            h = height,
            color = color
        ));
    }

    add_time_axis(&mut svg, &series.dates, &xs, width, height);

    svg.push_str(svg_footer());
    svg
}

fn draw_histogram(values: &[f64], bins: usize, title: &str) -> String {
    if values.is_empty() {
        return draw_bar_chart(values, title);
    }

    let mut min = f64::INFINITY;
    let mut max = f64::NEG_INFINITY;
    for v in values {
        if v.is_nan() {
            continue;
        }
        if *v < min {
            min = *v;
        }
        if *v > max {
            max = *v;
        }
    }

    if !min.is_finite() || !max.is_finite() || min == max {
        return draw_bar_chart(values, title);
    }

    let bins = bins.max(5).min(50);
    let mut counts = vec![0usize; bins];
    let width_bin = (max - min) / bins as f64;

    for v in values {
        if v.is_nan() {
            continue;
        }
        let mut idx = ((v - min) / width_bin).floor() as i32;
        if idx < 0 {
            idx = 0;
        }
        if idx as usize >= bins {
            idx = bins as i32 - 1;
        }
        counts[idx as usize] += 1;
    }

    let max_count = *counts.iter().max().unwrap_or(&1) as f64;
    let heights: Vec<f64> = counts
        .iter()
        .map(|c| {
            if max_count == 0.0 {
                0.0
            } else {
                *c as f64 / max_count
            }
        })
        .collect();

    draw_bar_chart(&heights, title)
}

fn compute_drawdown(returns: &ReturnSeries) -> Vec<f64> {
    let mut equity = Vec::with_capacity(returns.values.len());
    let mut eq = 1.0;
    for r in &returns.values {
        if r.is_nan() {
            equity.push(eq);
        } else {
            eq *= 1.0 + r;
            equity.push(eq);
        }
    }

    let mut peak = 1.0;
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

pub fn returns(returns: &ReturnSeries, benchmark: Option<&ReturnSeries>) -> String {
    let title = if benchmark.is_some() {
        "Cumulative Returns vs Benchmark"
    } else {
        "Cumulative Returns"
    };
    draw_equity_curve(returns, benchmark, title)
}

pub fn log_returns(returns: &ReturnSeries, benchmark: Option<&ReturnSeries>) -> String {
    let mut log_ret = returns.clone();
    let mut eq = 1.0;
    for v in &mut log_ret.values {
        if v.is_nan() {
            continue;
        }
        eq *= 1.0 + *v;
        *v = eq.ln();
    }
    let title = if benchmark.is_some() {
        "Cumulative Returns vs Benchmark (Log Scaled)"
    } else {
        "Cumulative Returns (Log Scaled)"
    };
    draw_equity_curve(&log_ret, benchmark, title)
}

pub fn vol_matched_returns(
    returns: &ReturnSeries,
    benchmark: Option<&ReturnSeries>,
) -> String {
    let title = if benchmark.is_some() {
        "Cumulative Returns vs Benchmark (Volatility Matched)"
    } else {
        "Cumulative Returns (Volatility Matched)"
    };
    if let Some(bm) = benchmark {
        let len = returns.values.len().min(bm.values.len());
        if len < 2 {
            return draw_equity_curve(returns, benchmark, title);
        }

        let strat_vals: Vec<f64> = returns.values[..len]
            .iter()
            .copied()
            .filter(|v| v.is_finite())
            .collect();
        let bench_vals: Vec<f64> = bm.values[..len]
            .iter()
            .copied()
            .filter(|v| v.is_finite())
            .collect();

        if strat_vals.len() < 2 || bench_vals.len() < 2 {
            return draw_equity_curve(returns, benchmark, title);
        }

        let s_std = {
            let n = strat_vals.len() as f64;
            let m = strat_vals.iter().sum::<f64>() / n;
            let var = strat_vals
                .iter()
                .map(|x| {
                    let d = x - m;
                    d * d
                })
                .sum::<f64>()
                / (n - 1.0);
            var.sqrt()
        };
        let b_std = {
            let n = bench_vals.len() as f64;
            let m = bench_vals.iter().sum::<f64>() / n;
            let var = bench_vals
                .iter()
                .map(|x| {
                    let d = x - m;
                    d * d
                })
                .sum::<f64>()
                / (n - 1.0);
            var.sqrt()
        };

        if s_std == 0.0 || b_std == 0.0 {
            return draw_equity_curve(returns, benchmark, title);
        }

        let scale = b_std / s_std;
        let mut scaled = returns.clone();
        scaled.values = returns
            .values
            .iter()
            .map(|v| if v.is_finite() { *v * scale } else { *v })
            .collect();
        draw_equity_curve(&scaled, benchmark, title)
    } else {
        draw_equity_curve(returns, benchmark, title)
    }
}

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

pub fn eoy_returns(
    strategy: &ReturnSeries,
    benchmark: Option<&ReturnSeries>,
) -> String {
    let strat_years = yearly_compounded(strategy);
    if strat_years.is_empty() {
        return String::new();
    }
    let bench_years = benchmark.map(yearly_compounded);

    let mut years: Vec<i32> = strat_years.keys().copied().collect();
    if let Some(ref b) = bench_years {
        for y in b.keys() {
            if !years.contains(y) {
                years.push(*y);
            }
        }
    }
    years.sort();
    if years.is_empty() {
        return String::new();
    }

    let width = WIDTH as f64;
    let height = HEIGHT as f64;
    let left_pad = 50.0;
    let right_pad = 20.0;
    let top_pad = 40.0;
    let bottom_pad = 40.0;

    // Collect all values to determine vertical scale.
    let mut min_v = 0.0_f64;
    let mut max_v = 0.0_f64;
    for year in &years {
        if let Some(v) = strat_years.get(year) {
            if *v < min_v {
                min_v = *v;
            }
            if *v > max_v {
                max_v = *v;
            }
        }
        if let Some(ref bmap) = bench_years {
            if let Some(v) = bmap.get(year) {
                if *v < min_v {
                    min_v = *v;
                }
                if *v > max_v {
                    max_v = *v;
                }
            }
        }
    }
    if min_v > 0.0 {
        min_v = 0.0;
    }
    if max_v < 0.0 {
        max_v = 0.0;
    }
    let range = max_v - min_v;
    if range == 0.0 {
        return String::new();
    }

    let inner_height = height - top_pad - bottom_pad;
    let value_to_y = |v: f64| {
        let norm = (v - min_v) / range;
        top_pad + (1.0 - norm) * inner_height
    };
    let zero_y = value_to_y(0.0);

    let groups = years.len();
    let group_width =
        (width - left_pad - right_pad) / (groups as f64).max(1.0);
    let bar_width = group_width * 0.35;

    let mut svg = String::new();
    svg.push_str(&svg_header(WIDTH, HEIGHT));
    let title = if benchmark.is_some() {
        "EOY Returns vs Benchmark"
    } else {
        "EOY Returns"
    };
    svg.push_str(&format!(
        r#"<text x="{x}" y="{y}" text-anchor="middle">{title}</text>"#,
        x = width / 2.0,
        y = PADDING - 10.0,
        title = title
    ));

    // Zero line
    svg.push_str(&format!(
        r##"<line x1="{x1:.2}" y1="{y:.2}" x2="{x2:.2}" y2="{y:.2}" stroke="#ccc" stroke-width="1" />"##,
        x1 = left_pad,
        x2 = width - right_pad,
        y = zero_y
    ));

    for (idx, year) in years.iter().enumerate() {
        let group_left = left_pad + idx as f64 * group_width;
        let cx = group_left + group_width / 2.0;

        // Year label
        svg.push_str(&format!(
            r##"<text x="{x:.2}" y="{y:.2}" text-anchor="middle" fill="#808080">{year}</text>"##,
            x = cx,
            y = height - bottom_pad + 14.0,
            year = year
        ));

        if let Some(ref bmap) = bench_years {
            if let Some(v) = bmap.get(year) {
                let y_val = value_to_y(*v);
                let (top, bottom) = if *v >= 0.0 {
                    (y_val, zero_y)
                } else {
                    (zero_y, y_val)
                };
                let x = cx - bar_width * 0.6;
                let h = (bottom - top).abs();
                svg.push_str(&format!(
                    r##"<rect x="{x:.2}" y="{y:.2}" width="{w:.2}" height="{h:.2}" fill="#ff9933" />"##,
                    x = x,
                    y = top,
                    w = bar_width,
                    h = h
                ));
            }
        }

        if let Some(v) = strat_years.get(year) {
            let y_val = value_to_y(*v);
            let (top, bottom) = if *v >= 0.0 {
                (y_val, zero_y)
            } else {
                (zero_y, y_val)
            };
            let x = cx + bar_width * 0.6 - bar_width;
            let h = (bottom - top).abs();
            svg.push_str(&format!(
                r##"<rect x="{x:.2}" y="{y:.2}" width="{w:.2}" height="{h:.2}" fill="#348dc1" />"##,
                x = x,
                y = top,
                w = bar_width,
                h = h
            ));
        }
    }

    svg.push_str(svg_footer());
    svg
}

pub fn histogram(returns: &ReturnSeries) -> String {
    draw_histogram(&returns.values, 20, "Distribution of Daily Returns")
}

pub fn daily_returns(returns: &ReturnSeries) -> String {
    let mut cum = Vec::with_capacity(returns.values.len());
    let mut acc = 0.0_f64;
    for r in &returns.values {
        if r.is_nan() {
            cum.push(acc);
        } else {
            acc += *r;
            cum.push(acc);
        }
    }
    draw_line_chart(&returns.dates, &cum, "Daily Returns (Cumulative Sum)")
}

pub fn drawdown(returns: &ReturnSeries) -> String {
    let drawdowns = compute_drawdown(returns);
    let mut dd_series = returns.clone();
    dd_series.values = drawdowns;
    draw_equity_curve(&dd_series, None, "Drawdown (Underwater)")
}

pub fn returns_distribution(returns: &ReturnSeries) -> String {
    draw_histogram(&returns.values, 30, "Return Quantiles")
}

fn monthly_compounded(series: &ReturnSeries) -> BTreeMap<(i32, u32), f64> {
    let mut grouped: BTreeMap<(i32, u32), Vec<f64>> = BTreeMap::new();
    for (date, ret) in series.dates.iter().zip(series.values.iter()) {
        if ret.is_nan() {
            continue;
        }
        grouped
            .entry((date.year(), date.month()))
            .or_default()
            .push(*ret);
    }

    let mut out = BTreeMap::new();
    for (key, vals) in grouped {
        if vals.is_empty() {
            continue;
        }
        let total = vals
            .iter()
            .fold(1.0_f64, |acc, r| acc * (1.0 + *r))
            - 1.0;
        out.insert(key, total);
    }
    out
}

pub fn monthly_distribution(returns: &ReturnSeries) -> String {
    let monthly = monthly_compounded(returns);
    if monthly.is_empty() {
        return String::new();
    }
    let values: Vec<f64> = monthly.values().copied().collect();
    draw_histogram(&values, 20, "Distribution of Monthly Returns")
}

fn rolling_apply(values: &[f64], window: usize, f: impl Fn(&[f64]) -> f64) -> Vec<f64> {
    if window == 0 || values.len() < window {
        return Vec::new();
    }
    let mut out = Vec::with_capacity(values.len() - window + 1);
    for i in 0..=values.len() - window {
        out.push(f(&values[i..i + window]));
    }
    out
}

pub fn rolling_volatility(returns: &ReturnSeries, periods_per_year: u32) -> String {
    let window = ((periods_per_year + 1) / 2) as usize;
    let vols = rolling_apply(&returns.values, window, |win| {
        let clean: Vec<f64> = win.iter().copied().filter(|v| v.is_finite()).collect();
        if clean.len() < 2 {
            return 0.0;
        }
        let n = clean.len() as f64;
        let m = clean.iter().sum::<f64>() / n;
        let var = clean
            .iter()
            .map(|x| {
                let d = x - m;
                d * d
            })
            .sum::<f64>()
            / (n - 1.0);
        var.sqrt() * (periods_per_year as f64).sqrt()
    });

    if vols.is_empty() {
        return String::new();
    }
    let dates = &returns.dates[window - 1..window - 1 + vols.len()];
    draw_line_chart(dates, &vols, "Rolling Volatility (6-Months)")
}

pub fn rolling_sharpe(
    returns: &ReturnSeries,
    rf: f64,
    periods_per_year: u32,
) -> String {
    let window = ((periods_per_year + 1) / 2) as usize;
    let sharpe_vals = rolling_apply(&returns.values, window, |win| {
        let clean: Vec<f64> = win.iter().copied().filter(|v| v.is_finite()).collect();
        if clean.len() < 2 {
            return 0.0;
        }
        let n = clean.len() as f64;
        let rf_per_period = if rf != 0.0 {
            (1.0 + rf).powf(1.0 / periods_per_year as f64) - 1.0
        } else {
            0.0
        };
        let excess: Vec<f64> = clean.iter().map(|v| *v - rf_per_period).collect();
        let mean = excess.iter().sum::<f64>() / n;
        let var = excess
            .iter()
            .map(|x| {
                let d = *x - mean;
                d * d
            })
            .sum::<f64>()
            / (n - 1.0);
        let std = var.sqrt();
        if std == 0.0 {
            0.0
        } else {
            mean / std * (periods_per_year as f64).sqrt()
        }
    });

    if sharpe_vals.is_empty() {
        return String::new();
    }
    let dates = &returns.dates[window - 1..window - 1 + sharpe_vals.len()];
    draw_line_chart(dates, &sharpe_vals, "Rolling Sharpe (6-Months)")
}

pub fn rolling_sortino(
    returns: &ReturnSeries,
    rf: f64,
    periods_per_year: u32,
) -> String {
    let window = ((periods_per_year + 1) / 2) as usize;
    let sortino_vals = rolling_apply(&returns.values, window, |win| {
        let clean: Vec<f64> = win.iter().copied().filter(|v| v.is_finite()).collect();
        if clean.len() < 2 {
            return 0.0;
        }
        let n = clean.len() as f64;
        let rf_per_period = if rf != 0.0 {
            (1.0 + rf).powf(1.0 / periods_per_year as f64) - 1.0
        } else {
            0.0
        };
        let excess: Vec<f64> = clean.iter().map(|v| *v - rf_per_period).collect();
        let mean = excess.iter().sum::<f64>() / n;
        let mut sum_sq_down = 0.0_f64;
        for x in &excess {
            if *x < 0.0 {
                sum_sq_down += x * x;
            }
        }
        if sum_sq_down == 0.0 {
            return 0.0;
        }
        let downside = (sum_sq_down / n).sqrt();
        mean / downside * (periods_per_year as f64).sqrt()
    });

    if sortino_vals.is_empty() {
        return String::new();
    }
    let dates = &returns.dates[window - 1..window - 1 + sortino_vals.len()];
    draw_line_chart(dates, &sortino_vals, "Rolling Sortino (6-Months)")
}

pub fn rolling_beta(
    returns: &ReturnSeries,
    benchmark: &ReturnSeries,
    periods_per_year: u32,
) -> String {
    let window = ((periods_per_year + 1) / 2) as usize;
    let len = returns.values.len().min(benchmark.values.len());
    if window == 0 || len < window {
        return String::new();
    }

    let strat = &returns.values[..len];
    let bench = &benchmark.values[..len];

    // Slide a window over the aligned strategy/benchmark returns and
    // compute beta in each window using the same covariance / variance
    // logic as the regression_metrics helper.
    let mut betas = Vec::with_capacity(len - window + 1);
    for start in 0..=len - window {
        let end = start + window;
        let mut pairs: Vec<(f64, f64)> = Vec::with_capacity(window);
        for i in start..end {
            let s = strat[i];
            let b = bench[i];
            if s.is_finite() && b.is_finite() {
                pairs.push((s, b));
            }
        }
        if pairs.len() < 2 {
            betas.push(0.0);
            continue;
        }
        let n = pairs.len() as f64;
        let mean_s = pairs.iter().map(|(s, _)| s).sum::<f64>() / n;
        let mean_b = pairs.iter().map(|(_, b)| b).sum::<f64>() / n;
        let mut cov = 0.0_f64;
        let mut var_b = 0.0_f64;
        for (s, b) in &pairs {
            let ds = *s - mean_s;
            let db = *b - mean_b;
            cov += ds * db;
            var_b += db * db;
        }
        cov /= n - 1.0;
        var_b /= n - 1.0;
        let beta = if var_b == 0.0 { 0.0 } else { cov / var_b };
        betas.push(beta);
    }

    if betas.is_empty() {
        return String::new();
    }
    let dates = &returns.dates[window - 1..window - 1 + betas.len()];
    draw_line_chart(dates, &betas, "Rolling Beta (6-Months)")
}

pub fn monthly_heatmap(returns: &ReturnSeries) -> String {
    let monthly = monthly_compounded(returns);
    if monthly.is_empty() {
        return String::new();
    }

    let mut years: Vec<i32> = monthly.keys().map(|(y, _)| *y).collect();
    years.sort();
    years.dedup();
    if years.is_empty() {
        return String::new();
    }

    let month_labels = [
        "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
        "JUL", "AUG", "SEP", "OCT", "NOV", "DEC",
    ];

    let width = WIDTH as f64;
    let height = HEIGHT as f64;
    let left_pad = 55.0;
    let right_pad = 20.0;
    let top_pad = 40.0;
    let bottom_pad = 30.0;
    let cols = 12usize;
    let rows = years.len();

    let cell_w = (width - left_pad - right_pad) / cols as f64;
    let cell_h = (height - top_pad - bottom_pad) / rows as f64;

    // Find max abs monthly return to scale colors.
    let mut max_abs = 0.0_f64;
    for v in monthly.values() {
        let a = v.abs();
        if a > max_abs {
            max_abs = a;
        }
    }
    if max_abs == 0.0 {
        max_abs = 1.0;
    }

    let mut svg = String::new();
    svg.push_str(&svg_header(WIDTH, HEIGHT));

    svg.push_str(&format!(
        r#"<text x="{x}" y="{y}" text-anchor="middle">Strategy - Monthly Returns (%)</text>"#,
        x = width / 2.0,
        y = PADDING - 10.0,
    ));

    // Month labels along x-axis.
    for (i, label) in month_labels.iter().enumerate() {
        let x = left_pad + (i as f64 + 0.5) * cell_w;
        let y = height - bottom_pad + 12.0;
        svg.push_str(&format!(
            r##"<text x="{x:.2}" y="{y:.2}" text-anchor="middle" fill="#808080">{label}</text>"##,
            x = x,
            y = y,
            label = label
        ));
    }

    // Year labels and cells.
    for (row, year) in years.iter().enumerate() {
        let y_center = top_pad + (row as f64 + 0.5) * cell_h;
        let y_top = y_center - cell_h / 2.0;

        // Year label on the left.
        svg.push_str(&format!(
            r##"<text x="{x:.2}" y="{y:.2}" text-anchor="end" fill="#808080">{year}</text>"##,
            x = left_pad - 5.0,
            y = y_center + 3.0,
            year = year
        ));

        for month_idx in 0..cols {
            let month = (month_idx + 1) as u32;
            let key = (*year, month);
            let x_left = left_pad + month_idx as f64 * cell_w;

            if let Some(v) = monthly.get(&key) {
                let t_raw = (v.abs() / max_abs).min(1.0);
                let t = 0.2 + 0.8 * t_raw; // keep a minimum intensity
                let (br, bg, bb) = if *v >= 0.0 {
                    (79.0, 164.0, 135.0) // greenish
                } else {
                    (175.0, 75.0, 100.0) // reddish
                };
                let r = 255.0 * (1.0 - t) + br * t;
                let g = 255.0 * (1.0 - t) + bg * t;
                let b = 255.0 * (1.0 - t) + bb * t;

                svg.push_str(&format!(
                    r##"<rect x="{x:.2}" y="{y:.2}" width="{w:.2}" height="{h:.2}" fill="rgb({r:.0},{g:.0},{b:.0})" />"##,
                    x = x_left,
                    y = y_top,
                    w = cell_w,
                    h = cell_h,
                    r = r,
                    g = g,
                    b = b
                ));

                // Percentage text inside the cell.
                let val = *v * 100.0;
                let text_color = if t > 0.6 { "#ffffff" } else { "#262626" };
                let tx = x_left + cell_w / 2.0;
                let ty = y_center + 4.0;
                svg.push_str(&format!(
                    r##"<text x="{x:.2}" y="{y:.2}" text-anchor="middle" font-size="9" fill="{color}">{val:.1}</text>"##,
                    x = tx,
                    y = ty,
                    color = text_color,
                    val = val
                ));
            } else {
                // Empty month cell.
                svg.push_str(&format!(
                    r##"<rect x="{x:.2}" y="{y:.2}" width="{w:.2}" height="{h:.2}" fill="#f5f5f5" />"##,
                    x = x_left,
                    y = y_top,
                    w = cell_w,
                    h = cell_h
                ));
            }
        }
    }

    svg.push_str(svg_footer());
    svg
}

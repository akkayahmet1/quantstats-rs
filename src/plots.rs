use crate::utils::ReturnSeries;
use chrono::NaiveDate;

const WIDTH: i32 = 800;
const HEIGHT: i32 = 300;
const PADDING: f64 = 30.0;

fn svg_header(width: i32, height: i32) -> String {
    format!(
        r#"<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {w} {h}" width="100%" height="100%"><style>text{{font-family:Arial,sans-serif;font-size:11px;fill:#555}}</style>"#,
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

fn draw_equity_curve(
    returns: &ReturnSeries,
    benchmark: Option<&ReturnSeries>,
    title: &str,
) -> String {
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

    let width = WIDTH as f64;
    let height = HEIGHT as f64;
    let xs = x_positions(equity.len(), width);
    let ys = scale_series(&equity, height);

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
        let ys_b = scale_series(&eq_bench, height);
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
    draw_equity_curve(returns, benchmark, "Cumulative Returns")
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
    draw_equity_curve(&log_ret, benchmark, "Log Cumulative Returns")
}

pub fn vol_matched_returns(
    returns: &ReturnSeries,
    benchmark: Option<&ReturnSeries>,
) -> String {
    draw_equity_curve(returns, benchmark, "Volatility Matched Returns")
}

pub fn histogram(returns: &ReturnSeries) -> String {
    draw_histogram(&returns.values, 20, "Returns Distribution")
}

pub fn daily_returns(returns: &ReturnSeries) -> String {
    draw_bar_chart_with_dates(returns, "Daily Returns")
}

pub fn drawdown(returns: &ReturnSeries) -> String {
    let drawdowns = compute_drawdown(returns);
    let mut dd_series = returns.clone();
    dd_series.values = drawdowns;
    draw_equity_curve(&dd_series, None, "Drawdown (Underwater)")
}

pub fn returns_distribution(returns: &ReturnSeries) -> String {
    draw_histogram(&returns.values, 30, "Returns Distribution")
}

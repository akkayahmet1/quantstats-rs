use crate::stats::all_drawdowns;
use crate::utils::ReturnSeries;
use chrono::{Datelike, NaiveDate};
use std::collections::{BTreeMap, HashMap};

// Use an aspect ratio and base size close to the
// Matplotlib QuantStats figures (~576x288).
const WIDTH: i32 = 576;
const HEIGHT: i32 = 288;
const PADDING: f64 = 36.0;
const STRATEGY_COLOR: &str = "#348dc1";
const BENCHMARK_COLOR: &str = "#ff9933";
const ACCENT_COLOR: &str = "#8c8c8c";

struct IndexedSeries {
    label: Option<String>,
    color: &'static str,
    stroke_width: f64,
    dash: bool,
    points: Vec<(usize, f64)>,
}

struct HorizontalGuide {
    value: f64,
    color: &'static str,
    width: f64,
    dash: bool,
    label: Option<String>,
}

struct LegendEntry {
    label: String,
    color: &'static str,
    dash: bool,
}

struct BoxStats {
    min: f64,
    q1: f64,
    median: f64,
    q3: f64,
    max: f64,
}

fn resolve_label(series: &ReturnSeries, fallback: &str) -> String {
    series.name.clone().unwrap_or_else(|| fallback.to_string())
}

fn format_percentage(value: f64) -> String {
    let pct = value * 100.0;
    if pct.abs() >= 1000.0 {
        format!("{:.0}K%", pct / 1000.0)
    } else {
        format!("{:.0}%", pct)
    }
}

fn nice_step(step: f64) -> f64 {
    if step <= 0.0 {
        return 1.0;
    }
    let mag = 10f64.powf(step.log10().floor());
    let norm = step / mag;
    let nice = if norm < 1.5 {
        1.0
    } else if norm < 3.0 {
        2.0
    } else if norm < 7.0 {
        5.0
    } else {
        10.0
    };
    nice * mag
}

fn generate_ticks(min_v: f64, max_v: f64) -> Vec<f64> {
    let min = min_v.min(0.0);
    let mut max = max_v.max(0.0);
    if (max - min).abs() < 1e-9 {
        max = min + 0.1;
    }
    let step = nice_step((max - min) / 4.0);
    let start = (min / step).floor() * step;
    let end = (max / step).ceil() * step;
    let mut ticks = Vec::new();
    let mut val = start;
    while val <= end + 1e-9 {
        ticks.push(val);
        val += step;
    }
    ticks
}

fn clean_values(values: &[f64]) -> Vec<f64> {
    values.iter().copied().filter(|v| v.is_finite()).collect()
}

fn extent_from_series(
    series_list: &[IndexedSeries],
    guides: &[HorizontalGuide],
    include_zero: bool,
) -> Option<(f64, f64)> {
    let mut min_v = f64::INFINITY;
    let mut max_v = f64::NEG_INFINITY;

    for series in series_list {
        for (_, value) in &series.points {
            if value.is_finite() {
                if *value < min_v {
                    min_v = *value;
                }
                if *value > max_v {
                    max_v = *value;
                }
            }
        }
    }

    for guide in guides {
        if guide.value.is_finite() {
            if guide.value < min_v {
                min_v = guide.value;
            }
            if guide.value > max_v {
                max_v = guide.value;
            }
        }
    }

    if include_zero {
        if 0.0 < min_v {
            min_v = 0.0;
        }
        if 0.0 > max_v {
            max_v = 0.0;
        }
    }

    if !min_v.is_finite() || !max_v.is_finite() {
        return None;
    }

    if min_v == max_v {
        let adjust = if min_v == 0.0 { 1.0 } else { min_v.abs() * 0.1 }; // widen flat ranges
        min_v -= adjust;
        max_v += adjust;
    }

    Some((min_v, max_v))
}

fn scale_value(value: f64, min_v: f64, max_v: f64, height: f64) -> f64 {
    if (max_v - min_v).abs() < f64::EPSILON {
        return height / 2.0;
    }

    let inner_height = height - 2.0 * PADDING;
    let norm = (value - min_v) / (max_v - min_v);
    PADDING + (1.0 - norm) * inner_height
}

fn draw_line_legend(svg: &mut String, entries: &[LegendEntry], _width: f64) {
    if entries.is_empty() {
        return;
    }

    let mut y = PADDING + 14.0;
    let x = PADDING + 10.0;
    for entry in entries {
        let dash = if entry.dash { "4 3" } else { "0" };
        svg.push_str(&format!(
            r##"<line x1="{x1:.2}" y1="{y:.2}" x2="{x2:.2}" y2="{y:.2}" stroke="{color}" stroke-width="1.5" stroke-dasharray="{dash}" />"##,
            x1 = x,
            x2 = x + 20.0,
            y = y - 4.0,
            color = entry.color,
            dash = dash
        ));
        svg.push_str(&format!(
            r##"<text x="{x:.2}" y="{y:.2}" text-anchor="start" fill="#333">{label}</text>"##,
            x = x + 26.0,
            y = y,
            label = entry.label.as_str()
        ));
        y += 16.0;
    }
}

fn format_axis_value(value: f64, percent: bool) -> String {
    if percent {
        format_percentage(value)
    } else {
        format!("{value:.2}")
    }
}

fn render_indexed_line_chart(
    dates: &[NaiveDate],
    series_list: &[IndexedSeries],
    guides: &[HorizontalGuide],
    title: &str,
    include_zero: bool,
    show_zero_line: bool,
    show_legend: bool,
    format_percent: bool,
) -> String {
    if dates.is_empty() || series_list.is_empty() {
        return String::new();
    }

    let width = WIDTH as f64;
    let height = HEIGHT as f64;

    let mut min_idx = usize::MAX;
    let mut max_idx = 0usize;
    for series in series_list {
        for (idx, value) in &series.points {
            if !value.is_finite() {
                continue;
            }
            if *idx < min_idx {
                min_idx = *idx;
            }
            if *idx > max_idx {
                max_idx = *idx;
            }
        }
    }
    if min_idx == usize::MAX {
        return String::new();
    }
    if min_idx >= dates.len() {
        min_idx = 0;
    }
    if max_idx >= dates.len() {
        max_idx = dates.len().saturating_sub(1);
    }
    if max_idx < min_idx {
        max_idx = min_idx;
    }
    let range_dates = &dates[min_idx..=max_idx];
    let axis_xs = x_positions(range_dates.len(), width);

    let (min_v, max_v) = match extent_from_series(series_list, guides, include_zero) {
        Some(extent) => extent,
        None => return String::new(),
    };
    let ticks = generate_ticks(min_v, max_v);

    let mut svg = String::new();
    svg.push_str(&svg_header(WIDTH, HEIGHT));

    // horizontal grid + y-axis labels
    let axis_left = PADDING;
    let axis_right = width - PADDING;
    let axis_top = PADDING;
    let axis_bottom = height - PADDING;
    svg.push_str(&format!(
        r##"<line x1=\"{x:.2}\" y1=\"{y1:.2}\" x2=\"{x:.2}\" y2=\"{y2:.2}\" stroke=\"#000\" stroke-width=\"1\" />"##,
        x = axis_left,
        y1 = axis_top,
        y2 = axis_bottom
    ));
    for tick in &ticks {
        let y = scale_value(*tick, min_v, max_v, height);
        let stroke = if tick.abs() < 1e-9 { "#000" } else { "#eeeeee" };
        svg.push_str(&format!(
            r##"<line x1=\"{x1:.2}\" y1=\"{y:.2}\" x2=\"{x2:.2}\" y2=\"{y:.2}\" stroke=\"{stroke}\" stroke-width=\"1\" />"##,
            x1 = axis_left,
            x2 = axis_right,
            y = y,
            stroke = stroke
        ));
        svg.push_str(&format!(
            r##"<line x1=\"{x1:.2}\" y1=\"{y:.2}\" x2=\"{x2:.2}\" y2=\"{y:.2}\" stroke=\"#000\" stroke-width=\"1\" />"##,
            x1 = axis_left - 4.0,
            x2 = axis_left,
            y = y
        ));
        svg.push_str(&format!(
            r##"<text x=\"{x:.2}\" y=\"{y:.2}\" text-anchor=\"end\" fill=\"#333\">{label}</text>"##,
            x = axis_left - 6.0,
            y = y - 2.0,
            label = format_axis_value(*tick, format_percent)
        ));
    }

    // vertical month grid lines
    let mut last_month: Option<(i32, u32)> = None;
    for (idx, date) in range_dates.iter().enumerate() {
        let key = (date.year(), date.month());
        if Some(key) == last_month {
            continue;
        }
        last_month = Some(key);
        if idx >= axis_xs.len() {
            break;
        }
        let x = axis_xs[idx];
        svg.push_str(&format!(
            r##"<line x1=\"{x:.2}\" y1=\"{y1:.2}\" x2=\"{x:.2}\" y2=\"{y2:.2}\" stroke=\"#f4f4f4\" stroke-width=\"1\" />"##,
            x = x,
            y1 = axis_top,
            y2 = axis_bottom
        ));
    }

    for guide in guides {
        let y = scale_value(guide.value, min_v, max_v, height);
        svg.push_str(&format!(
            r#"<line x1="{x1:.2}" y1="{y:.2}" x2="{x2:.2}" y2="{y:.2}" stroke="{color}" stroke-width="{width}" stroke-dasharray="{dash}" />"#,
            x1 = PADDING,
            x2 = width - PADDING,
            y = y,
            color = guide.color,
            width = guide.width,
            dash = if guide.dash { "4 3" } else { "0" }
        ));
        if let Some(label) = &guide.label {
            svg.push_str(&format!(
                r#"<text x="{x:.2}" y="{y:.2}" text-anchor="end" fill="{color}" font-size="9">{label}</text>"#,
                x = width - PADDING,
                y = y - 4.0,
                color = guide.color,
                label = label
            ));
        }
    }

    if show_zero_line {
        let y = scale_value(0.0, min_v, max_v, height);
        svg.push_str(&format!(
            r##"<line x1="{x1:.2}" y1="{y:.2}" x2="{x2:.2}" y2="{y:.2}" stroke="#bbbbbb" stroke-width="1" stroke-dasharray="4 3" />"##,
            x1 = PADDING,
            x2 = width - PADDING,
            y = y
        ));
    }

    let mut legend_entries = Vec::new();
    for series in series_list {
        if series.points.is_empty() {
            continue;
        }
        let mut coords = Vec::with_capacity(series.points.len());
        for (idx, value) in &series.points {
            if !value.is_finite() {
                continue;
            }
            if *idx < min_idx || *idx > max_idx {
                continue;
            }
            let rel_idx = idx.saturating_sub(min_idx);
            if rel_idx >= axis_xs.len() {
                continue;
            }
            coords.push((axis_xs[rel_idx], scale_value(*value, min_v, max_v, height)));
        }
        if coords.is_empty() {
            continue;
        }
        let points_attr = coords
            .iter()
            .map(|(x, y)| format!("{x:.2},{y:.2}"))
            .collect::<Vec<_>>()
            .join(" ");
        let dash_attr = if series.dash { "4 3" } else { "0" };
        svg.push_str(&format!(
            r#"<polyline fill="none" stroke="{color}" stroke-width="{width}" stroke-dasharray="{dash}" points="{points}" />"#,
            color = series.color,
            width = series.stroke_width,
            dash = dash_attr,
            points = points_attr
        ));

        if let Some(label) = &series.label {
            legend_entries.push(LegendEntry {
                label: label.clone(),
                color: series.color,
                dash: series.dash,
            });
        }
    }

    add_time_axis(&mut svg, range_dates, &axis_xs, width, height, None);

    if show_legend && !legend_entries.is_empty() {
        draw_line_legend(&mut svg, &legend_entries, width);
    }

    svg.push_str(svg_footer());
    wrap_plot(title, svg)
}

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

fn wrap_plot(title: &str, svg_body: String) -> String {
    format!(
        r#"<div class="qs-plot"><div class="qs-plot-title" style="text-align:center">{title}</div>{svg}</div>"#,
        title = title,
        svg = svg_body
    )
}

fn add_time_axis(
    svg: &mut String,
    dates: &[NaiveDate],
    xs: &[f64],
    width: f64,
    height: f64,
    axis_override: Option<f64>,
) {
    if dates.is_empty() || xs.is_empty() {
        return;
    }

    let axis_y = axis_override.unwrap_or(height - PADDING + 5.0);

    // Draw axis line
    svg.push_str(&format!(
        r##"<line x1="{x1:.2}" y1="{y:.2}" x2="{x2:.2}" y2="{y:.2}" stroke="#000" stroke-width="1" />"##,
        x1 = PADDING,
        x2 = width - PADDING,
        y = axis_y
    ));

    // Monthly labels
    let mut last_month: Option<(i32, u32)> = None;
    for (idx, date) in dates.iter().enumerate() {
        let key = (date.year(), date.month());
        if last_month == Some(key) {
            continue;
        }
        last_month = Some(key);

        if idx >= xs.len() {
            break;
        }
        let x = xs[idx];
        let label = date.format("%Y-%m").to_string();

        svg.push_str(&format!(
            r##"<line x1="{x:.2}" y1="{y1:.2}" x2="{x:.2}" y2="{y2:.2}" stroke="#dddddd" stroke-width="0.5" />"##,
            x = x,
            y1 = PADDING,
            y2 = height - PADDING
        ));

        svg.push_str(&format!(
            r##"<line x1="{x:.2}" y1="{y1:.2}" x2="{x:.2}" y2="{y2:.2}" stroke="#ccc" stroke-width="1" />"##,
            x = x,
            y1 = axis_y,
            y2 = axis_y + 4.0
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
        .map(|i| PADDING + inner_width * (i as f64 / (len.saturating_sub(1) as f64).max(1.0)))
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

fn draw_line_chart(dates: &[NaiveDate], values: &[f64], title: &str) -> String {
    let width = WIDTH as f64;
    let height = HEIGHT as f64;
    if dates.is_empty() || values.is_empty() {
        return String::new();
    }

    let xs = x_positions(values.len(), width);
    let mut min_v = f64::INFINITY;
    let mut max_v = f64::NEG_INFINITY;
    for v in values {
        if !v.is_finite() {
            continue;
        }
        if *v < min_v {
            min_v = *v;
        }
        if *v > max_v {
            max_v = *v;
        }
    }
    if !min_v.is_finite() || !max_v.is_finite() {
        min_v = -1.0;
        max_v = 1.0;
    }
    if (max_v - min_v).abs() < 1e-9 {
        let adjust = if min_v.abs() < 1e-3 {
            1.0
        } else {
            min_v.abs() * 0.1
        };
        min_v -= adjust;
        max_v += adjust;
    }

    let inner_height = height - 2.0 * PADDING;
    let value_to_y = |value: f64| {
        let norm = (value - min_v) / (max_v - min_v);
        PADDING + (1.0 - norm) * inner_height
    };

    let ticks = generate_ticks(min_v, max_v);

    let mut svg = String::new();
    svg.push_str(&svg_header(WIDTH, HEIGHT));

    // horizontal grid and y-axis labels
    let axis_left = PADDING;
    let axis_top = PADDING;
    let axis_bottom = height - PADDING;
    svg.push_str(&format!(
        r##"<line x1="{x:.2}" y1="{y1:.2}" x2="{x:.2}" y2="{y2:.2}" stroke="#000" stroke-width="1" />"##,
        x = axis_left,
        y1 = axis_top,
        y2 = axis_bottom
    ));

    for (idx, tick) in ticks.iter().enumerate() {
        let y = value_to_y(*tick);
        let stroke = if tick.abs() < 1e-9 { "#000" } else { "#eeeeee" };
        let is_zero = tick.abs() < 1e-9;
        if !is_zero && idx + 1 != ticks.len() {
            svg.push_str(&format!(
                r##"<line x1="{x1:.2}" y1="{y:.2}" x2="{x2:.2}" y2="{y:.2}" stroke="{stroke}" stroke-width="1" />"##,
                x1 = axis_left,
                x2 = width - PADDING,
                y = y,
                stroke = stroke
            ));
        }
        svg.push_str(&format!(
            r##"<line x1="{x1:.2}" y1="{y:.2}" x2="{x2:.2}" y2="{y:.2}" stroke="#000" stroke-width="1" />"##,
            x1 = axis_left - 4.0,
            x2 = axis_left,
            y = y
        ));
        svg.push_str(&format!(
            r##"<text x="{x:.2}" y="{y:.2}" text-anchor="end" fill="#333">{label}</text>"##,
            x = axis_left - 6.0,
            y = y - 2.0,
            label = format_percentage(*tick)
        ));
    }

    let mut points = Vec::new();
    for (idx, value) in values.iter().enumerate() {
        if !value.is_finite() || idx >= xs.len() {
            continue;
        }
        points.push((xs[idx], value_to_y(*value)));
    }
    svg.push_str(&polyline(&points, "#348dc1"));
    let axis_override = if min_v <= 0.0 && max_v >= 0.0 {
        Some(value_to_y(0.0))
    } else {
        None
    };

    add_time_axis(&mut svg, dates, &xs, width, height, axis_override);

    svg.push_str(svg_footer());
    wrap_plot(title, svg)
}

fn cumulative_growth(values: &[f64]) -> Vec<f64> {
    let mut out = Vec::with_capacity(values.len());
    let mut eq = 1.0;
    for r in values {
        if r.is_finite() {
            eq *= 1.0 + *r;
        }
        out.push(eq);
    }
    out
}

fn cumulative_return(values: &[f64]) -> Vec<f64> {
    cumulative_growth(values)
        .into_iter()
        .map(|v| v - 1.0)
        .collect()
}

fn draw_equity_curve(
    returns: &ReturnSeries,
    benchmark: Option<&ReturnSeries>,
    title: &str,
    log_scale: bool,
) -> String {
    let strat_curve = if log_scale {
        cumulative_growth(&returns.values)
    } else {
        cumulative_return(&returns.values)
    };
    let bench_curve = benchmark.map(|b| {
        if log_scale {
            cumulative_growth(&b.values)
        } else {
            cumulative_return(&b.values)
        }
    });

    let xs = x_positions(strat_curve.len(), WIDTH as f64);
    if xs.is_empty() {
        return String::new();
    }

    let width = WIDTH as f64;
    let height = HEIGHT as f64;
    let inner_height = height - 2.0 * PADDING;
    let mut svg = String::new();
    svg.push_str(&svg_header(WIDTH, HEIGHT));
    let mut axis_override: Option<f64> = None;

    if log_scale {
        let mut min_v = strat_curve
            .iter()
            .copied()
            .filter(|v| v.is_finite())
            .fold(f64::INFINITY, f64::min);
        let mut max_v = strat_curve
            .iter()
            .copied()
            .filter(|v| v.is_finite())
            .fold(f64::NEG_INFINITY, f64::max);
        if let Some(ref bench_vals) = bench_curve {
            for v in bench_vals {
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
        if !min_v.is_finite() || !max_v.is_finite() || min_v <= 0.0 || max_v <= 0.0 {
            min_v = 0.5;
            max_v = 1.5;
        }
        let log_min = min_v.max(1e-6).ln();
        let log_max = max_v.max(1e-6).ln();
        let log_span = (log_max - log_min).max(1e-6);
        let value_to_y = |v: f64| {
            let lv = v.max(1e-6).ln();
            PADDING + (1.0 - (lv - log_min) / log_span) * inner_height
        };

        let mut max_return = strat_curve
            .iter()
            .copied()
            .map(|v| v - 1.0)
            .fold(f64::NEG_INFINITY, f64::max);
        if let Some(ref bench_vals) = bench_curve {
            for v in bench_vals {
                if v.is_finite() {
                    let ret = *v - 1.0;
                    if ret > max_return {
                        max_return = ret;
                    }
                }
            }
        }
        if !max_return.is_finite() || max_return <= 0.0 {
            max_return = 0.1;
        }
        let step = max_return / 4.0;
        for i in 0..=4 {
            let ret = step * i as f64;
            let value = 1.0 + ret;
            let y = value_to_y(value);
            let color = if i == 0 { "#000" } else { "#eeeeee" };
            svg.push_str(&format!(
                r##"<line x1="{x1:.2}" y1="{y:.2}" x2="{x2:.2}" y2="{y:.2}" stroke="{color}" stroke-width="1" />"##,
                x1 = PADDING,
                x2 = width - PADDING,
                y = y,
                color = color
            ));
            svg.push_str(&format!(
                r##"<text x="{x:.2}" y="{y:.2}" text-anchor="end" fill="#666" dy="-4">{label}</text>"##,
                x = PADDING - 8.0,
                y = y,
                label = format_percentage(ret)
            ));
        }

        let zero_y = value_to_y(1.0);
        axis_override = Some(zero_y);
        svg.push_str(&format!(
            r##"<line x1="{x1:.2}" y1="{y:.2}" x2="{x2:.2}" y2="{y:.2}" stroke="#000" stroke-width="1" />"##,
            x1 = PADDING,
            x2 = width - PADDING,
            y = zero_y
        ));
        svg.push_str(&format!(
            r##"<text x="{x:.2}" y="{y:.2}" text-anchor="end" fill="#333" dy="-4">0%</text>"##,
            x = PADDING - 8.0,
            y = zero_y
        ));

        if let Some(eq_bench) = bench_curve {
            let xs_b = x_positions(eq_bench.len(), width);
            let mut coords = Vec::with_capacity(eq_bench.len());
            for (idx, val) in eq_bench.iter().enumerate() {
                if idx >= xs_b.len() {
                    break;
                }
                coords.push((xs_b[idx], value_to_y(*val)));
            }
            svg.push_str(&format!(
                r#"<polyline fill="none" stroke="{color}" stroke-width="1.5" points="{points}" />"#,
                color = BENCHMARK_COLOR,
                points = coords
                    .iter()
                    .map(|(x, y)| format!("{x:.2},{y:.2}"))
                    .collect::<Vec<_>>()
                    .join(" ")
            ));
        }

        let strat_coords: Vec<(f64, f64)> = strat_curve
            .iter()
            .enumerate()
            .map(|(idx, val)| (xs[idx], value_to_y(*val)))
            .collect();
        svg.push_str(&format!(
            r#"<polyline fill="none" stroke="{color}" stroke-width="1.8" points="{points}" />"#,
            color = STRATEGY_COLOR,
            points = strat_coords
                .iter()
                .map(|(x, y)| format!("{x:.2},{y:.2}"))
                .collect::<Vec<_>>()
                .join(" ")
        ));
    } else {
        let mut min_v = strat_curve
            .iter()
            .copied()
            .filter(|v| v.is_finite())
            .fold(f64::INFINITY, f64::min);
        let mut max_v = strat_curve
            .iter()
            .copied()
            .filter(|v| v.is_finite())
            .fold(f64::NEG_INFINITY, f64::max);
        if let Some(ref bench_vals) = bench_curve {
            for v in bench_vals {
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
        let mut max_return = strat_curve
            .iter()
            .copied()
            .filter(|v| v.is_finite())
            .fold(f64::NEG_INFINITY, |acc, v| acc.max(v));
        if let Some(ref bench_vals) = bench_curve {
            for v in bench_vals {
                if v.is_finite() {
                    if *v > max_return {
                        max_return = *v;
                    }
                }
            }
        }
        if !max_return.is_finite() {
            max_return = 1.0;
        }
        min_v = 0.0;
        max_v = max_return;
        let span = (max_v - min_v).max(1e-6);
        let value_to_y = |v: f64| {
            let norm = (v - min_v) / span;
            PADDING + (1.0 - norm) * inner_height
        };

        for i in 0..=4 {
            let value = min_v + span * (i as f64 / 4.0);
            let y = value_to_y(value);
            let color = if value.abs() < 1e-9 {
                "#000"
            } else {
                "#eeeeee"
            };
            svg.push_str(&format!(
            r##"<line x1="{x1:.2}" y1="{y:.2}" x2="{x2:.2}" y2="{y:.2}" stroke="{color}" stroke-width="1" />"##,
            x1 = PADDING,
            x2 = width - PADDING,
            y = y,
            color = color
        ));
            svg.push_str(&format!(
                r##"<text x="{x:.2}" y="{y:.2}" text-anchor="end" fill="#666" dy="-4">{label}</text>"##,
                x = PADDING - 8.0,
                y = y,
                label = format_percentage(value)
            ));
        }

        if min_v <= 0.0 && max_v >= 0.0 {
            let zero_y = value_to_y(0.0);
            axis_override = Some(zero_y);
            svg.push_str(&format!(
                r##"<line x1="{x1:.2}" y1="{y:.2}" x2="{x2:.2}" y2="{y:.2}" stroke="#000" stroke-width="1" />"##,
                x1 = PADDING,
                x2 = width - PADDING,
                y = zero_y,
            ));
            svg.push_str(&format!(
                r##"<text x="{x:.2}" y="{y:.2}" text-anchor="end" fill="#333" dy="-4">0%</text>"##,
                x = PADDING - 8.0,
                y = zero_y
            ));
        }

        if let Some(eq_bench) = bench_curve {
            let xs_b = x_positions(eq_bench.len(), width);
            let mut coords = Vec::with_capacity(eq_bench.len());
            for (idx, val) in eq_bench.iter().enumerate() {
                if idx >= xs_b.len() {
                    break;
                }
                coords.push((xs_b[idx], value_to_y(*val)));
            }
            svg.push_str(&format!(
                r#"<polyline fill="none" stroke="{color}" stroke-width="1.5" points="{points}" />"#,
                color = BENCHMARK_COLOR,
                points = coords
                    .iter()
                    .map(|(x, y)| format!("{x:.2},{y:.2}"))
                    .collect::<Vec<_>>()
                    .join(" ")
            ));
        }

        let strat_coords: Vec<(f64, f64)> = strat_curve
            .iter()
            .enumerate()
            .map(|(idx, val)| (xs[idx], value_to_y(*val)))
            .collect();
        svg.push_str(&format!(
            r#"<polyline fill="none" stroke="{color}" stroke-width="1.8" points="{points}" />"#,
            color = STRATEGY_COLOR,
            points = strat_coords
                .iter()
                .map(|(x, y)| format!("{x:.2},{y:.2}"))
                .collect::<Vec<_>>()
                .join(" ")
        ));
    }

    // For cumulative charts, keep the x-axis aligned to the zero (0%) line when visible.
    add_time_axis(&mut svg, &returns.dates, &xs, width, height, axis_override);

    if benchmark.is_some() {
        let mut legend = Vec::new();
        legend.push(LegendEntry {
            label: resolve_label(returns, "Strategy"),
            color: STRATEGY_COLOR,
            dash: false,
        });
        legend.push(LegendEntry {
            label: benchmark
                .and_then(|b| b.name.clone())
                .unwrap_or_else(|| "Benchmark".to_string()),
            color: BENCHMARK_COLOR,
            dash: false,
        });
        draw_line_legend(&mut svg, &legend, width);
    }

    svg.push_str(svg_footer());
    wrap_plot(title, svg)
}

fn draw_bar_chart(values: &[f64], title: &str) -> String {
    let width = WIDTH as f64;
    let height = HEIGHT as f64;
    let xs = x_positions(values.len(), width);
    let ys = scale_series(values, height);
    let zero = zero_line(height);

    let mut svg = String::new();
    svg.push_str(&svg_header(WIDTH, HEIGHT));

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
    wrap_plot(title, svg)
}

fn draw_bar_chart_with_dates(series: &ReturnSeries, title: &str) -> String {
    let width = WIDTH as f64;
    let height = HEIGHT as f64;
    let xs = x_positions(series.values.len(), width);
    let ys = scale_series(&series.values, height);
    let zero = zero_line(height);

    let mut svg = String::new();
    svg.push_str(&svg_header(WIDTH, HEIGHT));

    if xs.is_empty() {
        svg.push_str(svg_footer());
        return svg;
    }

    let bar_width = ((width - 2.0 * PADDING) / series.values.len().max(1) as f64) * 0.7;

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

    add_time_axis(&mut svg, &series.dates, &xs, width, height, None);

    svg.push_str(svg_footer());
    wrap_plot(title, svg)
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
    draw_equity_curve(returns, benchmark, title, false)
}

pub fn log_returns(returns: &ReturnSeries, benchmark: Option<&ReturnSeries>) -> String {
    let title = if benchmark.is_some() {
        "Cumulative Returns vs Benchmark (Log Scaled)"
    } else {
        "Cumulative Returns (Log Scaled)"
    };
    draw_equity_curve(returns, benchmark, title, true)
}

pub fn vol_matched_returns(returns: &ReturnSeries, benchmark: Option<&ReturnSeries>) -> String {
    let title = if benchmark.is_some() {
        "Cumulative Returns vs Benchmark (Volatility Matched)"
    } else {
        "Cumulative Returns (Volatility Matched)"
    };
    if let Some(bm) = benchmark {
        let len = returns.values.len().min(bm.values.len());
        if len < 2 {
            return draw_equity_curve(returns, benchmark, title, false);
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
            return draw_equity_curve(returns, benchmark, title, false);
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
            return draw_equity_curve(returns, benchmark, title, false);
        }

        let scale = b_std / s_std;
        let mut scaled = returns.clone();
        scaled.values = returns
            .values
            .iter()
            .map(|v| if v.is_finite() { *v * scale } else { *v })
            .collect();
        draw_equity_curve(&scaled, benchmark, title, false)
    } else {
        draw_equity_curve(returns, benchmark, title, false)
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
        let total = vals.iter().fold(1.0_f64, |acc, r| acc * (1.0 + *r)) - 1.0;
        out.insert(year, total);
    }
    out
}

pub fn eoy_returns(strategy: &ReturnSeries, benchmark: Option<&ReturnSeries>) -> String {
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
    let left_pad = 65.0;
    let right_pad = 30.0;
    let top_pad = 50.0;
    let bottom_pad = 50.0;

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

    let ticks = generate_ticks(min_v, max_v);
    if ticks.len() < 2 {
        return String::new();
    }
    let tick_min = *ticks.first().unwrap();
    let tick_max = *ticks.last().unwrap();
    let span = tick_max - tick_min;
    let inner_height = height - top_pad - bottom_pad;
    let value_to_y = |v: f64| {
        let norm = (v - tick_min) / span;
        top_pad + (1.0 - norm) * inner_height
    };
    let zero_y = value_to_y(0.0);

    let groups = years.len();
    let group_width = (width - left_pad - right_pad) / (groups as f64).max(1.0);
    let series_count = if benchmark.is_some() { 2 } else { 1 };
    let bar_area = group_width * 0.7;
    let bar_width = bar_area / series_count.max(1) as f64;

    let mut svg = String::new();
    svg.push_str(&svg_header(WIDTH, HEIGHT));
    let title = if benchmark.is_some() {
        "EOY Returns vs Benchmark"
    } else {
        "EOY Returns"
    };

    // Grid lines and labels
    for tick in ticks {
        let y = value_to_y(tick);
        let color = if (tick).abs() < 1e-9 {
            "#000"
        } else {
            "#e0e0e0"
        };
        svg.push_str(&format!(
            r##"<line x1="{x1:.2}" y1="{y:.2}" x2="{x2:.2}" y2="{y:.2}" stroke="{color}" stroke-width="1" />"##,
            x1 = left_pad,
            x2 = width - right_pad,
            y = y,
            color = color
        ));
        if tick >= 0.0 || color == "#000" {
            svg.push_str(&format!(
                r##"<text x="{x:.2}" y="{y:.2}" text-anchor="end" fill="#666" dy="-4">{label}</text>"##,
                x = left_pad - 6.0,
                y = y,
                label = format_percentage(tick)
            ));
        }
    }

    for (idx, year) in years.iter().enumerate() {
        let group_left = left_pad + idx as f64 * group_width;
        let cx = group_left + group_width / 2.0;

        // Year label
        svg.push_str(&format!(
            r##"<text x="{x:.2}" y="{y:.2}" text-anchor="middle" fill="#808080">{year}</text>"##,
            x = cx,
            y = height - bottom_pad + 16.0,
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
                let x = cx - bar_area / 2.0;
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
            let x = if series_count == 1 {
                cx - bar_width / 2.0
            } else {
                cx - bar_area / 2.0 + bar_width
            };
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

    // Legend
    let legend_y = top_pad - 24.0;
    let legend_x = left_pad;
    let entries = if benchmark.is_some() {
        vec![("Benchmark", BENCHMARK_COLOR), ("Strategy", STRATEGY_COLOR)]
    } else {
        vec![("Strategy", STRATEGY_COLOR)]
    };
    for (idx, (label, color)) in entries.iter().enumerate() {
        let y = legend_y + idx as f64 * 16.0;
        svg.push_str(&format!(
            r##"<rect x="{x:.2}" y="{y:.2}" width="12" height="12" fill="{color}" />"##,
            x = legend_x,
            y = y
        ));
        svg.push_str(&format!(
            r##"<text x="{x:.2}" y="{y:.2}" text-anchor="start" fill="#333">{label}</text>"##,
            x = legend_x + 18.0,
            y = y + 10.0,
            label = label
        ));
    }

    svg.push_str(svg_footer());
    wrap_plot(title, svg)
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
    draw_equity_curve(&dd_series, None, "Drawdown (Underwater)", false)
}

pub fn drawdown_periods(returns: &ReturnSeries) -> String {
    if returns.values.is_empty() {
        return String::new();
    }

    let mut segments = all_drawdowns(returns);
    if segments.is_empty() {
        return String::new();
    }
    segments.sort_by(|a, b| b.duration.cmp(&a.duration));
    segments.truncate(5);
    segments.sort_by(|a, b| a.start.cmp(&b.start));

    let mut cumulative = Vec::with_capacity(returns.values.len());
    let mut equity = 1.0_f64;
    for r in &returns.values {
        if r.is_finite() {
            equity *= 1.0 + *r;
        }
        cumulative.push(equity - 1.0);
    }

    let width = WIDTH as f64;
    let height = HEIGHT as f64;
    let xs = x_positions(returns.values.len(), width);
    if xs.is_empty() {
        return String::new();
    }

    let mut min_v = cumulative
        .iter()
        .copied()
        .filter(|v| v.is_finite())
        .fold(f64::INFINITY, f64::min);
    let mut max_v = cumulative
        .iter()
        .copied()
        .filter(|v| v.is_finite())
        .fold(f64::NEG_INFINITY, f64::max);
    if !min_v.is_finite() || !max_v.is_finite() {
        min_v = -0.5;
        max_v = 0.5;
    }
    if min_v > 0.0 {
        min_v = 0.0;
    }
    if max_v < 0.0 {
        max_v = 0.0;
    }
    if (max_v - min_v).abs() < f64::EPSILON {
        max_v += 0.5;
        min_v -= 0.5;
    }

    let value_to_y = |value: f64| {
        let inner_height = height - 2.0 * PADDING;
        let norm = (value - min_v) / (max_v - min_v);
        PADDING + (1.0 - norm) * inner_height
    };

    let mut svg = String::new();
    svg.push_str(&svg_header(WIDTH, HEIGHT));

    let zero_y = value_to_y(0.0);
    svg.push_str(&format!(
        r##"<line x1="{x1:.2}" y1="{y:.2}" x2="{x2:.2}" y2="{y:.2}" stroke="#bbbbbb" stroke-width="1" stroke-dasharray="4 3" />"##,
        x1 = PADDING,
        x2 = width - PADDING,
        y = zero_y
    ));

    let mut date_index = HashMap::new();
    for (idx, date) in returns.dates.iter().enumerate() {
        date_index.insert(*date, idx);
    }

    for (idx, seg) in segments.iter().enumerate() {
        let start_idx = date_index.get(&seg.start).copied().unwrap_or(0);
        let end_idx = date_index
            .get(&seg.end)
            .copied()
            .unwrap_or(returns.values.len().saturating_sub(1));
        let x1 = xs[start_idx];
        let x2 = xs[end_idx];
        let (x_left, width_rect) = if x2 >= x1 {
            (x1, (x2 - x1).max(1.0))
        } else {
            (x2, (x1 - x2).max(1.0))
        };
        svg.push_str(&format!(
            r##"<rect x="{x:.2}" y="{y:.2}" width="{w:.2}" height="{h:.2}" fill="#af4b64" fill-opacity="0.12" />"##,
            x = x_left,
            y = PADDING,
            w = width_rect,
            h = height - 2.0 * PADDING
        ));

        svg.push_str(&format!(
            r##"<text x="{x:.2}" y="{y:.2}" text-anchor="end" fill="#af4b64" font-size="9">{label}</text>"##,
            x = width - PADDING,
            y = PADDING + 12.0 + idx as f64 * 12.0,
            label = format!("{}: {}d", idx + 1, seg.duration)
        ));
    }

    let points: Vec<(f64, f64)> = xs
        .iter()
        .enumerate()
        .map(|(idx, x)| (*x, value_to_y(cumulative[idx])))
        .collect();
    svg.push_str(&format!(
        r#"<polyline fill="none" stroke="{color}" stroke-width="1.8" points="{pts}" />"#,
        color = STRATEGY_COLOR,
        pts = points
            .iter()
            .map(|(x, y)| format!("{x:.2},{y:.2}"))
            .collect::<Vec<_>>()
            .join(" ")
    ));

    add_time_axis(&mut svg, &returns.dates, &xs, width, height, None);

    svg.push_str(svg_footer());
    wrap_plot("Strategy - Worst 5 Drawdown Periods", svg)
}

fn percentile(sorted: &[f64], p: f64) -> f64 {
    if sorted.is_empty() {
        return 0.0;
    }
    let clamped = p.clamp(0.0, 1.0);
    let pos = clamped * (sorted.len() as f64 - 1.0);
    let idx = pos.floor() as usize;
    let frac = pos - idx as f64;
    if idx + 1 >= sorted.len() {
        sorted[idx]
    } else {
        let lower = sorted[idx];
        let upper = sorted[idx + 1];
        lower + (upper - lower) * frac
    }
}

fn compute_box_stats(values: &[f64]) -> Option<BoxStats> {
    let mut clean: Vec<f64> = values.iter().copied().filter(|v| v.is_finite()).collect();
    if clean.is_empty() {
        return None;
    }
    clean.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    Some(BoxStats {
        min: *clean.first().unwrap(),
        q1: percentile(&clean, 0.25),
        median: percentile(&clean, 0.5),
        q3: percentile(&clean, 0.75),
        max: *clean.last().unwrap(),
    })
}

pub fn returns_distribution(returns: &ReturnSeries) -> String {
    let mut groups: Vec<(String, &'static str, Vec<f64>)> = Vec::new();

    let daily_vals = clean_values(&returns.values);
    if daily_vals.len() >= 2 {
        groups.push(("Daily".to_string(), "#4fa487", daily_vals));
    }

    let weekly_vals: Vec<f64> = weekly_compounded(returns).values().copied().collect();
    if weekly_vals.len() >= 2 {
        groups.push(("Weekly".to_string(), "#348dc1", weekly_vals));
    }

    let monthly_vals: Vec<f64> = monthly_compounded(returns).values().copied().collect();
    if monthly_vals.len() >= 2 {
        groups.push(("Monthly".to_string(), "#ff9933", monthly_vals));
    }

    let quarterly_vals: Vec<f64> = quarterly_compounded(returns).values().copied().collect();
    if quarterly_vals.len() >= 2 {
        groups.push(("Quarterly".to_string(), "#af4b64", quarterly_vals));
    }

    let yearly_vals: Vec<f64> = yearly_compounded(returns).values().copied().collect();
    if yearly_vals.len() >= 2 {
        groups.push(("Yearly".to_string(), "#9b59b6", yearly_vals));
    }

    let mut stats = Vec::new();
    for (label, color, values) in groups {
        if let Some(summary) = compute_box_stats(&values) {
            stats.push((label, color, summary));
        }
    }

    if stats.is_empty() {
        return String::new();
    }

    let mut min_v = stats
        .iter()
        .map(|(_, _, s)| s.min)
        .fold(f64::INFINITY, f64::min);
    let mut max_v = stats
        .iter()
        .map(|(_, _, s)| s.max)
        .fold(f64::NEG_INFINITY, f64::max);
    if !min_v.is_finite() || !max_v.is_finite() {
        min_v = -0.1;
        max_v = 0.1;
    }
    if (max_v - min_v).abs() < 1e-9 {
        max_v += 0.05;
        min_v -= 0.05;
    }

    let width = WIDTH as f64;
    let height = HEIGHT as f64;
    let left_pad = 60.0;
    let right_pad = 40.0;
    let top_pad = 40.0;
    let bottom_pad = 50.0;
    let band = (width - left_pad - right_pad) / stats.len() as f64;
    let inner_height = height - top_pad - bottom_pad;
    let value_to_y = |value: f64| {
        let norm = (value - min_v) / (max_v - min_v);
        top_pad + (1.0 - norm) * inner_height
    };

    let mut svg = String::new();
    svg.push_str(&svg_header(WIDTH, HEIGHT));

    // Y-axis grid lines
    let grid_steps = 4;
    for i in 0..=grid_steps {
        let value = min_v + (max_v - min_v) * (i as f64 / grid_steps as f64);
        let y = value_to_y(value);
        svg.push_str(&format!(
            r##"<line x1="{x1:.2}" y1="{y:.2}" x2="{x2:.2}" y2="{y:.2}" stroke="#eeeeee" stroke-width="1" />"##,
            x1 = left_pad,
            x2 = width - right_pad,
            y = y
        ));
        svg.push_str(&format!(
            r##"<text x="{x:.2}" y="{y:.2}" text-anchor="end" fill="#666">{label}</text>"##,
            x = left_pad - 6.0,
            y = y + 3.0,
            label = format!("{:.1}%", value * 100.0)
        ));
    }

    let zero_y = value_to_y(0.0);
    if zero_y >= top_pad && zero_y <= height - bottom_pad {
        svg.push_str(&format!(
            r##"<line x1="{x1:.2}" y1="{y:.2}" x2="{x2:.2}" y2="{y:.2}" stroke="#bbbbbb" stroke-width="1" stroke-dasharray="4 3" />"##,
            x1 = left_pad,
            x2 = width - right_pad,
            y = zero_y
        ));
    }

    for (idx, (label, color, summary)) in stats.iter().enumerate() {
        let cx = left_pad + (idx as f64 + 0.5) * band;
        let y_min = value_to_y(summary.min);
        let y_max = value_to_y(summary.max);
        let y_q1 = value_to_y(summary.q1);
        let y_q3 = value_to_y(summary.q3);
        let y_median = value_to_y(summary.median);

        svg.push_str(&format!(
            r#"<line x1="{x:.2}" y1="{y1:.2}" x2="{x:.2}" y2="{y2:.2}" stroke="{color}" stroke-width="1" />"#,
            x = cx,
            y1 = y_max,
            y2 = y_min,
            color = color
        ));
        svg.push_str(&format!(
            r#"<line x1="{x1:.2}" y1="{y:.2}" x2="{x2:.2}" y2="{y:.2}" stroke="{color}" stroke-width="1" />"#,
            x1 = cx - band * 0.15,
            x2 = cx + band * 0.15,
            y = y_max,
            color = color
        ));
        svg.push_str(&format!(
            r#"<line x1="{x1:.2}" y1="{y:.2}" x2="{x2:.2}" y2="{y:.2}" stroke="{color}" stroke-width="1" />"#,
            x1 = cx - band * 0.15,
            x2 = cx + band * 0.15,
            y = y_min,
            color = color
        ));

        let box_height = (y_q1 - y_q3).abs().max(1.0);
        let box_top = y_q3.min(y_q1);
        svg.push_str(&format!(
            r#"<rect x="{x:.2}" y="{y:.2}" width="{w:.2}" height="{h:.2}" fill="{color}" fill-opacity="0.25" stroke="{color}" stroke-width="1" />"#,
            x = cx - band * 0.2,
            y = box_top,
            w = band * 0.4,
            h = box_height,
            color = color
        ));

        svg.push_str(&format!(
            r#"<line x1="{x1:.2}" y1="{y:.2}" x2="{x2:.2}" y2="{y:.2}" stroke="{color}" stroke-width="2" />"#,
            x1 = cx - band * 0.2,
            x2 = cx + band * 0.2,
            y = y_median,
            color = color
        ));

        svg.push_str(&format!(
            r##"<text x="{x:.2}" y="{y:.2}" text-anchor="middle" fill="#444">{label}</text>"##,
            x = cx,
            y = height - bottom_pad + 16.0,
            label = label
        ));
    }

    svg.push_str(svg_footer());
    wrap_plot("Return Quantiles", svg)
}

fn compound_slice(values: &[f64]) -> f64 {
    values
        .iter()
        .copied()
        .filter(|v| v.is_finite())
        .fold(1.0_f64, |acc, r| acc * (1.0 + r))
        - 1.0
}

fn grouped_compounded<K: Ord + Clone>(
    series: &ReturnSeries,
    key_fn: impl Fn(&NaiveDate) -> K,
) -> BTreeMap<K, f64> {
    let mut grouped: BTreeMap<K, Vec<f64>> = BTreeMap::new();
    for (date, ret) in series.dates.iter().zip(series.values.iter()) {
        if !ret.is_finite() {
            continue;
        }
        grouped.entry(key_fn(date)).or_default().push(*ret);
    }

    let mut out = BTreeMap::new();
    for (key, vals) in grouped {
        if vals.is_empty() {
            continue;
        }
        out.insert(key.clone(), compound_slice(&vals));
    }
    out
}

fn weekly_compounded(series: &ReturnSeries) -> BTreeMap<(i32, u32), f64> {
    grouped_compounded(series, |date| {
        let iso = date.iso_week();
        (iso.year(), iso.week())
    })
}

fn monthly_compounded(series: &ReturnSeries) -> BTreeMap<(i32, u32), f64> {
    grouped_compounded(series, |date| (date.year(), date.month()))
}

fn quarterly_compounded(series: &ReturnSeries) -> BTreeMap<(i32, u32), f64> {
    grouped_compounded(series, |date| {
        let quarter = ((date.month() - 1) / 3) + 1;
        (date.year(), quarter)
    })
}

fn accumulate_histogram(
    data: &BTreeMap<(i32, u32), f64>,
    counts: &mut [usize],
    min_value: f64,
    bin_width: f64,
) {
    if bin_width <= 0.0 {
        return;
    }
    let bins = counts.len() as isize;
    for value in data.values() {
        if !value.is_finite() {
            continue;
        }
        let mut idx = ((*value - min_value) / bin_width).floor() as isize;
        if idx < 0 {
            idx = 0;
        }
        if idx >= bins {
            idx = bins - 1;
        }
        counts[idx as usize] += 1;
    }
}

fn kde_density(values: &[f64], xs: &[f64]) -> Vec<f64> {
    let clean: Vec<f64> = values.iter().copied().filter(|v| v.is_finite()).collect();
    if clean.len() < 2 {
        return vec![0.0; xs.len()];
    }

    let n = clean.len() as f64;
    let mean = clean.iter().sum::<f64>() / n;
    let var = clean
        .iter()
        .map(|x| {
            let d = x - mean;
            d * d
        })
        .sum::<f64>()
        / (n - 1.0);
    let std = var.sqrt();
    let bandwidth = (1.06 * std * n.powf(-0.2)).max(1e-6);

    let norm_const = 1.0 / (n * bandwidth * (2.0 * std::f64::consts::PI).sqrt());
    xs.iter()
        .map(|x| {
            clean
                .iter()
                .map(|v| {
                    let z = (x - v) / bandwidth;
                    (-0.5 * z * z).exp()
                })
                .sum::<f64>()
                * norm_const
        })
        .collect()
}

pub fn monthly_distribution(returns: &ReturnSeries, benchmark: Option<&ReturnSeries>) -> String {
    let monthly = monthly_compounded(returns);
    let monthly_bench = benchmark.map(monthly_compounded);
    if monthly.is_empty() {
        return String::new();
    }

    let mut min_v = *monthly
        .values()
        .filter(|v| v.is_finite())
        .min_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
        .unwrap_or(&0.0);
    let mut max_v = *monthly
        .values()
        .filter(|v| v.is_finite())
        .max_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
        .unwrap_or(&0.0);
    if let Some(ref b) = monthly_bench {
        if let Some(vmin) = b
            .values()
            .filter(|v| v.is_finite())
            .min_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
        {
            if *vmin < min_v {
                min_v = *vmin;
            }
        }
        if let Some(vmax) = b
            .values()
            .filter(|v| v.is_finite())
            .max_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
        {
            if *vmax > max_v {
                max_v = *vmax;
            }
        }
    }

    let ticks = generate_ticks(min_v, max_v);
    if ticks.len() < 2 {
        return String::new();
    }

    let width = WIDTH as f64;
    let height = HEIGHT as f64;
    let left_pad = 70.0;
    let right_pad = 30.0;
    let top_pad = 45.0;
    let bottom_pad = 60.0;
    let inner_height = height - top_pad - bottom_pad;
    let bar_area = width - left_pad - right_pad;
    let min_tick = *ticks.first().unwrap();
    let max_tick = *ticks.last().unwrap();
    let span = (max_tick - min_tick).max(1e-9);
    let bin_count = 20usize;
    let bin_width = span / bin_count as f64;
    let slot_width = bar_area / bin_count as f64;

    let mut strat_counts = vec![0usize; bin_count];
    accumulate_histogram(&monthly, &mut strat_counts, min_tick, bin_width);
    let strat_values: Vec<f64> = monthly.values().copied().collect();

    let bench_counts = monthly_bench.as_ref().map(|b| {
        let mut counts = vec![0usize; bin_count];
        accumulate_histogram(b, &mut counts, min_tick, bin_width);
        counts
    });
    let bench_values = monthly_bench
        .as_ref()
        .map(|b| b.values().copied().collect::<Vec<_>>());

    let mut max_count = strat_counts.iter().copied().max().unwrap_or(0);
    if let Some(ref bench) = bench_counts {
        if let Some(m) = bench.iter().copied().max() {
            if m > max_count {
                max_count = m;
            }
        }
    }
    if max_count == 0 {
        return String::new();
    }

    let build_bars = |counts: &[usize], color_pos: &str, color_neg: &str| {
        let mut bars = Vec::new();
        for (idx, count) in counts.iter().enumerate() {
            if *count == 0 {
                continue;
            }
            let ratio = *count as f64 / max_count as f64;
            let bar_height = ratio * inner_height;
            let x = left_pad + idx as f64 * slot_width;
            let y = top_pad + (inner_height - bar_height);
            let mid = min_tick + (idx as f64 + 0.5) * bin_width;
            let color = if mid >= 0.0 { color_pos } else { color_neg };
            bars.push((
                x + slot_width * 0.05,
                y,
                slot_width * 0.9,
                bar_height,
                color.to_string(),
            ));
        }
        bars
    };

    let strat_bars = build_bars(&strat_counts, "#4fa487", "#af4b64");
    let bench_bars = bench_counts
        .as_ref()
        .map(|counts| build_bars(counts, "#ff9933", "#c96a40"));

    let density_points = 180;
    let xs: Vec<f64> = (0..=density_points)
        .map(|i| min_tick + span * (i as f64 / density_points as f64))
        .collect();
    let strat_density = kde_density(&strat_values, &xs);
    let bench_density = bench_values.as_ref().map(|vals| kde_density(vals, &xs));
    let mut density_max = strat_density.iter().copied().fold(0.0_f64, f64::max);
    if let Some(ref bench) = bench_density {
        density_max = density_max.max(bench.iter().copied().fold(0.0, f64::max));
    }
    if density_max == 0.0 {
        density_max = 1.0;
    }
    let density_to_svg = |vals: &[f64], color: &str| {
        let mut path = String::new();
        for (idx, val) in vals.iter().enumerate() {
            let x = left_pad + ((xs[idx] - min_tick) / span).clamp(0.0, 1.0) * bar_area;
            let h = (val / density_max).clamp(0.0, 1.0) * inner_height;
            let y = top_pad + (inner_height - h);
            if idx == 0 {
                path.push_str(&format!("M{x:.2},{y:.2} "));
            } else {
                path.push_str(&format!("L{x:.2},{y:.2} "));
            }
        }
        format!(
            r##"<path d="{path}" fill="none" stroke="{color}" stroke-width="1.4" />"##,
            path = path.trim(),
            color = color
        )
    };

    let mut svg = String::new();
    svg.push_str(&svg_header(WIDTH, HEIGHT));
    let title = if monthly_bench.is_some() {
        "Distribution of Monthly Returns vs Benchmark"
    } else {
        "Distribution of Monthly Returns"
    };

    // grid background – horizontal bands
    let grid_steps = 4;
    for step in 0..=grid_steps {
        let frac = step as f64 / grid_steps as f64;
        let y = top_pad + frac * inner_height;
        let stroke = if step == grid_steps {
            "#000"
        } else {
            "#f0f0f0"
        };
        svg.push_str(&format!(
            r##"<line x1="{x1:.2}" y1="{y:.2}" x2="{x2:.2}" y2="{y:.2}" stroke="{stroke}" stroke-width="1" />"##,
            x1 = left_pad,
            x2 = width - right_pad,
            y = y,
            stroke = stroke
        ));
    }

    // vertical grid lines for each tick
    for tick in &ticks {
        let norm = ((*tick - min_tick) / span).clamp(0.0, 1.0);
        let x = left_pad + norm * bar_area;
        svg.push_str(&format!(
            r##"<line x1="{x:.2}" y1="{y1:.2}" x2="{x:.2}" y2="{y2:.2}" stroke="#f4f4f4" stroke-width="1" />"##,
            x = x,
            y1 = top_pad,
            y2 = height - bottom_pad
        ));
    }

    // vertical zero line
    let zero_norm = ((0.0 - min_tick) / span).clamp(0.0, 1.0);
    let zero_x = left_pad + zero_norm * bar_area;
    svg.push_str(&format!(
        r##"<line x1="{x:.2}" y1="{y1:.2}" x2="{x:.2}" y2="{y2:.2}" stroke="#bbbbbb" stroke-width="1" />"##,
        x = zero_x,
        y1 = top_pad,
        y2 = height - bottom_pad
    ));

    // draw bars (benchmark first so strategy overlays)
    if let Some(bars) = bench_bars {
        for (x, y, w, h, color) in bars {
            svg.push_str(&format!(
                r##"<rect x="{x:.2}" y="{y:.2}" width="{w:.2}" height="{h:.2}" fill="{color}" fill-opacity="0.6" />"##,
                x = x,
                y = y,
                w = w,
                h = h,
                color = color
            ));
        }
    }
    for (x, y, w, h, color) in strat_bars {
        svg.push_str(&format!(
            r##"<rect x="{x:.2}" y="{y:.2}" width="{w:.2}" height="{h:.2}" fill="{color}" />"##,
            x = x,
            y = y,
            w = w,
            h = h,
            color = color
        ));
    }

    if let Some(ref bench_density) = bench_density {
        svg.push_str(&density_to_svg(bench_density, "#ff9933"));
    }
    svg.push_str(&density_to_svg(&strat_density, "#4fa487"));

    // Y axis (count scale)
    let axis_bottom = height - bottom_pad;
    let axis_top = top_pad;
    let axis_height = axis_bottom - axis_top;
    svg.push_str(&format!(
        r##"<line x1="{x:.2}" y1="{y1:.2}" x2="{x:.2}" y2="{y2:.2}" stroke="#000" stroke-width="1" />"##,
        x = left_pad,
        y1 = axis_top,
        y2 = axis_bottom
    ));
    let y_ticks = 4;
    for step in 0..=y_ticks {
        let value = max_count as f64 * (step as f64 / y_ticks as f64);
        let y = axis_bottom - (value / max_count as f64) * axis_height;
        svg.push_str(&format!(
            r##"<line x1="{x1:.2}" y1="{y:.2}" x2="{x2:.2}" y2="{y:.2}" stroke="#000" stroke-width="1" />"##,
            x1 = left_pad - 4.0,
            x2 = left_pad,
            y = y
        ));
        svg.push_str(&format!(
            r##"<text x="{x:.2}" y="{y:.2}" text-anchor="end" fill="#333">{label:.0}</text>"##,
            x = left_pad - 6.0,
            y = y + 4.0,
            label = value
        ));
    }

    // X axis and ticks
    let axis_y = height - bottom_pad;
    svg.push_str(&format!(
        r##"<line x1="{x1:.2}" y1="{y:.2}" x2="{x2:.2}" y2="{y:.2}" stroke="#000" stroke-width="1" />"##,
        x1 = left_pad,
        x2 = width - right_pad,
        y = axis_y
    ));

    for tick in &ticks {
        let x = left_pad + ((*tick - min_tick) / span) * bar_area;
        svg.push_str(&format!(
            r##"<line x1="{x:.2}" y1="{y1:.2}" x2="{x:.2}" y2="{y2:.2}" stroke="#ccc" stroke-width="1" />"##,
            x = x,
            y1 = axis_y,
            y2 = axis_y + 6.0
        ));
        svg.push_str(&format!(
            r#"<text x="{x:.2}" y="{y:.2}" text-anchor="middle" dominant-baseline="hanging">{label}</text>"#,
            x = x,
            y = axis_y + 10.0,
            label = format_percentage(*tick)
        ));
    }

    // legend (left aligned for consistency with QuantStats)
    let legend_x = left_pad;
    let legend_y = top_pad - 24.0;
    let mut entry_y = legend_y;
    if monthly_bench.is_some() {
        svg.push_str(&format!(
            r##"<rect x="{x:.2}" y="{y:.2}" width="12" height="12" fill="#ff9933" fill-opacity="0.6" />"##,
            x = legend_x,
            y = entry_y
        ));
        svg.push_str(&format!(
            r##"<text x="{x:.2}" y="{y:.2}" text-anchor="start" fill="#333">Benchmark</text>"##,
            x = legend_x + 18.0,
            y = entry_y + 10.0
        ));
        entry_y += 16.0;
    }
    svg.push_str(&format!(
        r##"<rect x="{x:.2}" y="{y:.2}" width="12" height="12" fill="#4fa487" />"##,
        x = legend_x,
        y = entry_y
    ));
    svg.push_str(&format!(
        r##"<text x="{x:.2}" y="{y:.2}" text-anchor="start" fill="#333">Strategy</text>"##,
        x = legend_x + 18.0,
        y = entry_y + 10.0
    ));

    svg.push_str(svg_footer());
    wrap_plot(title, svg)
}

fn rolling_apply_indexed(
    values: &[f64],
    window: usize,
    f: impl Fn(&[f64]) -> f64,
) -> Vec<(usize, f64)> {
    if window == 0 || values.len() < window {
        return Vec::new();
    }

    let mut out = Vec::with_capacity(values.len() - window + 1);
    for end in window - 1..values.len() {
        let start = end + 1 - window;
        out.push((end, f(&values[start..=end])));
    }
    out
}

fn rolling_beta_points(strat: &[f64], bench: &[f64], window: usize) -> Vec<(usize, f64)> {
    if window == 0 || strat.len() < window || bench.len() < window {
        return Vec::new();
    }

    let mut out = Vec::with_capacity(strat.len() - window + 1);
    for end in window - 1..strat.len().min(bench.len()) {
        let start = end + 1 - window;
        let mut pairs: Vec<(f64, f64)> = Vec::with_capacity(window);
        for i in start..=end {
            let s = strat[i];
            let b = bench[i];
            if s.is_finite() && b.is_finite() {
                pairs.push((s, b));
            }
        }
        if pairs.len() < 2 {
            out.push((end, 0.0));
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
        out.push((end, beta));
    }
    out
}

fn mean_from_points(points: &[(usize, f64)]) -> Option<f64> {
    let mut acc = 0.0;
    let mut count = 0;
    for (_, value) in points {
        if value.is_finite() {
            acc += *value;
            count += 1;
        }
    }
    if count == 0 {
        None
    } else {
        Some(acc / count as f64)
    }
}

pub fn rolling_volatility(
    returns: &ReturnSeries,
    benchmark: Option<&ReturnSeries>,
    periods_per_year: u32,
) -> String {
    let window = ((periods_per_year + 1) / 2) as usize;
    let strat_points = rolling_apply_indexed(&returns.values, window, |win| {
        let clean: Vec<f64> = win.iter().copied().filter(|v| v.is_finite()).collect();
        if clean.len() < 2 {
            return 0.0;
        }
        let n = clean.len() as f64;
        let mean = clean.iter().sum::<f64>() / n;
        let var = clean
            .iter()
            .map(|x| {
                let d = x - mean;
                d * d
            })
            .sum::<f64>()
            / (n - 1.0);
        var.sqrt() * (periods_per_year as f64).sqrt()
    });

    if strat_points.is_empty() {
        return String::new();
    }

    let mut series = Vec::new();
    series.push(IndexedSeries {
        label: Some(resolve_label(returns, "Strategy")),
        color: STRATEGY_COLOR,
        stroke_width: 2.0,
        dash: false,
        points: strat_points.clone(),
    });

    if let Some(bench) = benchmark {
        let len = returns.values.len().min(bench.values.len());
        if len >= window {
            let bench_points = rolling_apply_indexed(&bench.values[..len], window, |win| {
                let clean: Vec<f64> = win.iter().copied().filter(|v| v.is_finite()).collect();
                if clean.len() < 2 {
                    return 0.0;
                }
                let n = clean.len() as f64;
                let mean = clean.iter().sum::<f64>() / n;
                let var = clean
                    .iter()
                    .map(|x| {
                        let d = x - mean;
                        d * d
                    })
                    .sum::<f64>()
                    / (n - 1.0);
                var.sqrt() * (periods_per_year as f64).sqrt()
            });
            if !bench_points.is_empty() {
                series.push(IndexedSeries {
                    label: Some(resolve_label(bench, "Benchmark")),
                    color: BENCHMARK_COLOR,
                    stroke_width: 1.4,
                    dash: true,
                    points: bench_points,
                });
            }
        }
    }

    let mut guides = Vec::new();
    if let Some(mean) = mean_from_points(&strat_points) {
        guides.push(HorizontalGuide {
            value: mean,
            color: ACCENT_COLOR,
            width: 1.0,
            dash: true,
            label: Some("Mean".to_string()),
        });
    }

    render_indexed_line_chart(
        &returns.dates,
        &series,
        &guides,
        "Rolling Volatility (6-Months)",
        false,
        false,
        series.len() > 1,
        true,
    )
}

pub fn rolling_sharpe(returns: &ReturnSeries, rf: f64, periods_per_year: u32) -> String {
    let window = ((periods_per_year + 1) / 2) as usize;
    let sharpe_points = rolling_apply_indexed(&returns.values, window, |win| {
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

    if sharpe_points.is_empty() {
        return String::new();
    }

    let mut guides = Vec::new();
    if let Some(mean) = mean_from_points(&sharpe_points) {
        guides.push(HorizontalGuide {
            value: mean,
            color: ACCENT_COLOR,
            width: 1.0,
            dash: true,
            label: Some("Mean".to_string()),
        });
    }

    let series = [IndexedSeries {
        label: Some(resolve_label(returns, "Strategy")),
        color: STRATEGY_COLOR,
        stroke_width: 2.0,
        dash: false,
        points: sharpe_points,
    }];

    render_indexed_line_chart(
        &returns.dates,
        &series,
        &guides,
        "Rolling Sharpe (6-Months)",
        true,
        true,
        false,
        false,
    )
}

pub fn rolling_sortino(returns: &ReturnSeries, rf: f64, periods_per_year: u32) -> String {
    let window = ((periods_per_year + 1) / 2) as usize;
    let sortino_points = rolling_apply_indexed(&returns.values, window, |win| {
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

    if sortino_points.is_empty() {
        return String::new();
    }

    let mut guides = Vec::new();
    if let Some(mean) = mean_from_points(&sortino_points) {
        guides.push(HorizontalGuide {
            value: mean,
            color: ACCENT_COLOR,
            width: 1.0,
            dash: true,
            label: Some("Mean".to_string()),
        });
    }

    let series = [IndexedSeries {
        label: Some(resolve_label(returns, "Strategy")),
        color: STRATEGY_COLOR,
        stroke_width: 2.0,
        dash: false,
        points: sortino_points,
    }];

    render_indexed_line_chart(
        &returns.dates,
        &series,
        &guides,
        "Rolling Sortino (6-Months)",
        true,
        true,
        false,
        false,
    )
}

pub fn rolling_beta(
    returns: &ReturnSeries,
    benchmark: &ReturnSeries,
    periods_per_year: u32,
) -> String {
    let len = returns.values.len().min(benchmark.values.len());
    if len == 0 {
        return String::new();
    }

    let short_window = ((periods_per_year + 1) / 2) as usize;
    let long_window = periods_per_year as usize;

    let short_points = rolling_beta_points(
        &returns.values[..len],
        &benchmark.values[..len],
        short_window,
    );
    if short_points.is_empty() {
        return String::new();
    }

    let mut series = Vec::new();
    series.push(IndexedSeries {
        label: Some("6-Months".to_string()),
        color: STRATEGY_COLOR,
        stroke_width: 2.0,
        dash: false,
        points: short_points.clone(),
    });

    let mut guides = Vec::new();
    if let Some(mean) = mean_from_points(&short_points) {
        guides.push(HorizontalGuide {
            value: mean,
            color: ACCENT_COLOR,
            width: 1.0,
            dash: true,
            label: Some("Mean".to_string()),
        });
    }

    let long_points = rolling_beta_points(
        &returns.values[..len],
        &benchmark.values[..len],
        long_window,
    );
    if !long_points.is_empty() {
        series.push(IndexedSeries {
            label: Some("12-Months".to_string()),
            color: ACCENT_COLOR,
            stroke_width: 1.5,
            dash: true,
            points: long_points,
        });
    }

    render_indexed_line_chart(
        &returns.dates,
        &series,
        &guides,
        "Rolling Beta to Benchmark",
        true,
        true,
        true,
        false,
    )
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
        "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC",
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
    wrap_plot("Strategy - Monthly Returns (%)", svg)
}

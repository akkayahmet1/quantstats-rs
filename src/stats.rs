use chrono::NaiveDate;

use crate::utils::ReturnSeries;

#[derive(Clone, Debug)]
pub struct PerformanceMetrics {
    pub total_return: f64,
    pub annualized_return: f64,
    pub annualized_volatility: f64,
    pub sharpe_ratio: f64,
    pub max_drawdown: f64,
    pub max_drawdown_duration: u32,
    pub max_drawdown_start: Option<NaiveDate>,
    pub max_drawdown_trough: Option<NaiveDate>,
    pub max_drawdown_end: Option<NaiveDate>,
    pub best_day: f64,
    pub worst_day: f64,
}

pub fn compute_performance_metrics(
    returns: &ReturnSeries,
    rf: f64,
    periods_per_year: u32,
) -> PerformanceMetrics {
    let n = returns.len() as f64;
    let total_return = compounded_return(&returns.values);
    let annualized_return = if n > 0.0 {
        (1.0 + total_return).powf(periods_per_year as f64 / n) - 1.0
    } else {
        0.0
    };

    // Annualized volatility (used for display), but Sharpe is computed
    // directly from per-period returns to match QuantStats' definition.
    let volatility = annualized_volatility(&returns.values, periods_per_year);
    let sharpe_ratio = sharpe_from_values(&returns.values, rf, periods_per_year);

    // Use detailed drawdown analysis for max drawdown stats
    let dd_segments = top_drawdowns(returns, 1);
    let (max_drawdown, max_duration, max_start, max_trough, max_end) =
        if let Some(dd) = dd_segments.first() {
            (
                dd.depth,
                dd.duration,
                Some(dd.start),
                Some(dd.trough),
                Some(dd.end),
            )
        } else {
            (0.0, 0, None, None, None)
        };

    let (best_day, worst_day) = best_and_worst(&returns.values);

    PerformanceMetrics {
        total_return,
        annualized_return,
        annualized_volatility: volatility,
        sharpe_ratio,
        max_drawdown,
        max_drawdown_duration: max_duration,
        max_drawdown_start: max_start,
        max_drawdown_trough: max_trough,
        max_drawdown_end: max_end,
        best_day,
        worst_day,
    }
}

fn compounded_return(returns: &[f64]) -> f64 {
    returns
        .iter()
        .filter(|v| !v.is_nan())
        .fold(1.0, |acc, r| acc * (1.0 + r))
        - 1.0
}

fn annualized_volatility(returns: &[f64], periods_per_year: u32) -> f64 {
    let clean: Vec<f64> = returns
        .iter()
        .copied()
        .filter(|v| v.is_finite())
        .collect();

    if clean.len() < 2 {
        return 0.0;
    }

    let n = clean.len() as f64;
    let mean = clean.iter().sum::<f64>() / n;
    let var = clean
        .iter()
        .map(|r| {
            let diff = r - mean;
            diff * diff
        })
        .sum::<f64>()
        / (n - 1.0);

    var.sqrt() * (periods_per_year as f64).sqrt()
}

fn best_and_worst(returns: &[f64]) -> (f64, f64) {
    let mut best = f64::NEG_INFINITY;
    let mut worst = f64::INFINITY;

    for r in returns.iter().copied().filter(|v| !v.is_nan()) {
        if r > best {
            best = r;
        }
        if r < worst {
            worst = r;
        }
    }

    if best == f64::NEG_INFINITY {
        best = 0.0;
    }
    if worst == f64::INFINITY {
        worst = 0.0;
    }

    (best, worst)
}

fn sharpe_from_values(returns: &[f64], rf: f64, periods_per_year: u32) -> f64 {
    let vals: Vec<f64> = returns
        .iter()
        .copied()
        .filter(|v| v.is_finite())
        .collect();

    if vals.len() < 2 {
        return 0.0;
    }

    let n = vals.len() as f64;

    // Convert annual risk-free rate to per-period
    let rf_per_period = if rf != 0.0 {
        (1.0 + rf).powf(1.0 / periods_per_year as f64) - 1.0
    } else {
        0.0
    };

    let excess: Vec<f64> = vals.into_iter().map(|r| r - rf_per_period).collect();

    let mean = excess.iter().sum::<f64>() / n;
    let var = excess
        .iter()
        .map(|r| {
            let diff = *r - mean;
            diff * diff
        })
        .sum::<f64>()
        / (n - 1.0);
    let std = var.sqrt();

    if std == 0.0 {
        0.0
    } else {
        mean / std * (periods_per_year as f64).sqrt()
    }
}

#[derive(Clone, Debug)]
pub struct Drawdown {
    pub start: NaiveDate,
    pub trough: NaiveDate,
    pub end: NaiveDate,
    /// Depth as a negative fraction (e.g. -0.25 for -25%)
    pub depth: f64,
    /// Duration in days (number of data points in the drawdown)
    pub duration: u32,
}

/// Compute the worst drawdown segments for a return series.
///
/// This is a simplified implementation that scans the equity curve,
/// identifies drawdown periods (from a new peak until recovery),
/// and returns the `top_n` deepest ones.
pub fn top_drawdowns(returns: &ReturnSeries, top_n: usize) -> Vec<Drawdown> {
    let mut segments = compute_drawdown_segments(returns);

    // Sort by depth (most negative first) and take top_n
    segments.sort_by(|a, b| a.depth.partial_cmp(&b.depth).unwrap_or(std::cmp::Ordering::Equal));
    segments.truncate(top_n);

    segments
}

/// Compute all drawdown segments (from each peak to full recovery or series end).
pub fn all_drawdowns(returns: &ReturnSeries) -> Vec<Drawdown> {
    compute_drawdown_segments(returns)
}

fn compute_drawdown_segments(returns: &ReturnSeries) -> Vec<Drawdown> {
    let n = returns.values.len();
    if n == 0 {
        return Vec::new();
    }

    // Build equity curve
    let mut equity = Vec::with_capacity(n);
    let mut eq = 1.0_f64;
    for r in &returns.values {
        if r.is_nan() {
            equity.push(eq);
        } else {
            eq *= 1.0 + *r;
            equity.push(eq);
        }
    }

    // Drawdown series relative to running peak
    let mut peak = equity[0];
    let mut drawdowns = Vec::with_capacity(n);
    for &e in &equity {
        if e > peak {
            peak = e;
        }
        let dd = e / peak - 1.0;
        drawdowns.push(dd);
    }

    // Identify drawdown segments
    let mut segments: Vec<Drawdown> = Vec::new();
    let mut in_dd = false;
    let mut start_idx = 0usize;
    let mut trough_idx = 0usize;
    let mut min_dd = 0.0_f64;

    for i in 0..n {
        let dd = drawdowns[i];
        if !in_dd {
            if dd < 0.0 {
                in_dd = true;
                start_idx = i;
                trough_idx = i;
                min_dd = dd;
            }
        } else {
            if dd < min_dd {
                min_dd = dd;
                trough_idx = i;
            }

            if dd >= 0.0 {
                // Recovered
                let start_date = returns.dates[start_idx];
                let end_date = returns.dates[i];
                let duration = (i - start_idx + 1) as u32;
                segments.push(Drawdown {
                    start: start_date,
                    trough: returns.dates[trough_idx],
                    end: end_date,
                    depth: min_dd,
                    duration,
                });
                in_dd = false;
            }
        }
    }

    // Handle open drawdown at the end
    if in_dd {
        let last = n - 1;
        let start_date = returns.dates[start_idx];
        let end_date = returns.dates[last];
        let duration = (last - start_idx + 1) as u32;
        segments.push(Drawdown {
            start: start_date,
            trough: returns.dates[trough_idx],
            end: end_date,
            depth: min_dd,
            duration,
        });
    }

    segments
}

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
    // and longest drawdown duration (may be a different period).
    let dd_segments = top_drawdowns(returns, 1);
    let (max_drawdown, max_start, max_trough, max_end) = if let Some(dd) = dd_segments.first() {
        (dd.depth, Some(dd.start), Some(dd.trough), Some(dd.end))
    } else {
        (0.0, None, None, None)
    };

    let all_dd_segments = all_drawdowns(returns);
    let max_duration = all_dd_segments
        .iter()
        .map(|d| d.duration)
        .max()
        .unwrap_or(0);

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

/// Compound annual growth rate (CAGR) from a slice of return values,
/// matching QuantStats' `cagr` implementation for rf = 0 and
/// compounded returns.
///
/// This is used for multi-year annualized metrics (3Y/5Y/10Y/All-time)
/// in the HTML report.
pub fn cagr_from_values(returns: &[f64], periods_per_year: u32) -> f64 {
    if returns.is_empty() {
        return 0.0;
    }

    // Total compounded return over the slice
    let total = compounded_return(returns);

    // Time in years expressed in trading periods
    let n = returns.len() as f64;
    let years = n / periods_per_year as f64;
    if years <= 0.0 {
        return 0.0;
    }

    (1.0 + total).abs().powf(1.0 / years) - 1.0
}

fn compounded_return(returns: &[f64]) -> f64 {
    returns
        .iter()
        .filter(|v| !v.is_nan())
        .fold(1.0, |acc, r| acc * (1.0 + r))
        - 1.0
}

fn annualized_volatility(returns: &[f64], periods_per_year: u32) -> f64 {
    let clean: Vec<f64> = returns.iter().copied().filter(|v| v.is_finite()).collect();

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
    let vals: Vec<f64> = returns.iter().copied().filter(|v| v.is_finite()).collect();

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
    segments.sort_by(|a, b| {
        a.depth
            .partial_cmp(&b.depth)
            .unwrap_or(std::cmp::Ordering::Equal)
    });
    segments.truncate(top_n);

    segments
}

/// Compute daily Value-at-Risk (normal approximation) at given confidence.
///
/// Matches QuantStats' `value_at_risk` when called with rf=0,
/// `sigma` multiplier = 1 and `confidence = 0.95`.
pub fn var_normal(returns: &ReturnSeries, sigma: f64, confidence: f64) -> f64 {
    let vals: Vec<f64> = returns
        .values
        .iter()
        .copied()
        .filter(|v| v.is_finite())
        .collect();
    if vals.len() < 2 {
        return 0.0;
    }

    let n = vals.len() as f64;
    let mean = vals.iter().sum::<f64>() / n;
    let var = vals
        .iter()
        .map(|r| {
            let d = *r - mean;
            d * d
        })
        .sum::<f64>()
        / (n - 1.0);
    let std = var.sqrt();

    let mut conf = confidence;
    if conf > 1.0 {
        conf /= 100.0;
    }
    let z = normal_ppf(1.0 - conf);
    mean + sigma * std * z
}

/// Conditional Value-at-Risk (Expected Shortfall) using the same logic as
/// QuantStats' `conditional_value_at_risk`.
///
/// This is currently not used directly in the HTML report (which mirrors
/// the reference QuantStats HTML where CVaR equals VaR), but is kept
/// available for library consumers.
#[allow(dead_code)]
pub fn cvar(returns: &ReturnSeries, sigma: f64, confidence: f64) -> f64 {
    let vals: Vec<f64> = returns
        .values
        .iter()
        .copied()
        .filter(|v| v.is_finite())
        .collect();
    if vals.len() < 2 {
        return 0.0;
    }

    let var_threshold = var_normal(returns, sigma, confidence);
    let tail: Vec<f64> = vals.into_iter().filter(|v| *v < var_threshold).collect();

    if tail.is_empty() {
        var_threshold
    } else {
        tail.iter().sum::<f64>() / tail.len() as f64
    }
}

/// Kelly criterion based on average win/loss and win rate, matching
/// QuantStats' `kelly_criterion` behavior for a single return series.
pub fn kelly(returns: &ReturnSeries) -> f64 {
    let vals: Vec<f64> = returns
        .values
        .iter()
        .copied()
        .filter(|v| v.is_finite())
        .collect();
    if vals.is_empty() {
        return 0.0;
    }

    let wins: Vec<f64> = vals.iter().copied().filter(|v| *v > 0.0).collect();
    let losses: Vec<f64> = vals.iter().copied().filter(|v| *v < 0.0).collect();

    if wins.is_empty() || losses.is_empty() {
        return 0.0;
    }

    let avg_win = wins.iter().sum::<f64>() / wins.len() as f64;
    let avg_loss = losses.iter().sum::<f64>() / losses.len() as f64;
    if avg_loss == 0.0 {
        return 0.0;
    }
    let win_loss_ratio = avg_win / -avg_loss;

    // win rate uses only non-zero returns in the denominator
    let non_zero: Vec<f64> = vals.iter().copied().filter(|v| *v != 0.0).collect();
    if non_zero.is_empty() {
        return 0.0;
    }
    let win_prob = non_zero.iter().filter(|v| **v > 0.0).count() as f64 / non_zero.len() as f64;
    let lose_prob = 1.0 - win_prob;

    if win_loss_ratio == 0.0 {
        0.0
    } else {
        ((win_loss_ratio * win_prob) - lose_prob) / win_loss_ratio
    }
}

/// Risk of Ruin using the gambler's ruin formula as in QuantStats'
/// `risk_of_ruin`.
pub fn risk_of_ruin(returns: &ReturnSeries) -> f64 {
    let vals: Vec<f64> = returns
        .values
        .iter()
        .copied()
        .filter(|v| v.is_finite())
        .collect();
    if vals.is_empty() {
        return 0.0;
    }

    // win rate over non-zero returns
    let non_zero: Vec<f64> = vals.iter().copied().filter(|v| *v != 0.0).collect();
    if non_zero.is_empty() {
        return 0.0;
    }
    let win_prob = non_zero.iter().filter(|v| **v > 0.0).count() as f64 / non_zero.len() as f64;

    if win_prob <= 0.0 {
        return 1.0;
    }

    let ratio = (1.0 - win_prob) / (1.0 + win_prob);
    ratio.powf(vals.len() as f64)
}

fn normal_cdf(x: f64) -> f64 {
    0.5 * (1.0 + erf(x / std::f64::consts::SQRT_2))
}

fn normal_ppf(p: f64) -> f64 {
    // Simple binary search inversion of the CDF on a bounded interval.
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
        let c = normal_cdf(mid);
        if c < p {
            lo = mid;
        } else {
            hi = mid;
        }
    }
    0.5 * (lo + hi)
}

fn erf(x: f64) -> f64 {
    // Abramowitz & Stegun approximation.
    let sign = if x < 0.0 { -1.0 } else { 1.0 };
    let x = x.abs();
    let t = 1.0 / (1.0 + 0.3275911 * x);
    let y = 1.0
        - (((((1.061405429 * t - 1.453152027) * t) + 1.421413741) * t - 0.284496736) * t
            + 0.254829592)
            * t
            * (-x * x).exp();
    sign * y
}

/// Compute all drawdown segments (from each peak to full recovery or series end).
pub fn all_drawdowns(returns: &ReturnSeries) -> Vec<Drawdown> {
    compute_drawdown_segments(returns)
}

/// Market exposure / time in market, matching QuantStats' `exposure`
/// (with prepare_returns = false).
///
/// Counts non-NaN, non-zero returns and divides by total periods,
/// then rounds up to nearest percent: ceil(ex * 100) / 100.
pub fn exposure(series: &ReturnSeries) -> f64 {
    if series.values.is_empty() {
        return 0.0;
    }

    let active = series
        .values
        .iter()
        .filter(|v| v.is_finite() && **v != 0.0)
        .count() as f64;
    let total = series.values.len() as f64;
    if total == 0.0 {
        return 0.0;
    }

    let ex = active / total;
    (ex * 100.0).ceil() / 100.0
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

    // Identify start and end indices following QuantStats'
    // drawdown_details() semantics.
    let mut starts: Vec<usize> = Vec::new();
    let mut ends: Vec<usize> = Vec::new();

    for i in 0..n {
        let no_dd = drawdowns[i] >= 0.0;
        let prev_no_dd = if i == 0 {
            true
        } else {
            drawdowns[i - 1] >= 0.0
        };

        // Start when we enter a drawdown region
        if !no_dd && prev_no_dd {
            starts.push(i);
        }

        // Candidate end when we exit a drawdown region (dd becomes >= 0)
        if no_dd && !prev_no_dd {
            // Python shifts the end marker back by one, so store i-1
            let end_idx = i.saturating_sub(1);
            ends.push(end_idx);
        }
    }

    // Handle edge case: drawdown series begins in a drawdown
    if !ends.is_empty() && !starts.is_empty() && starts[0] > ends[0] {
        starts.insert(0, 0);
    }

    // Handle edge case: series ends in a drawdown
    if ends.is_empty() || (!starts.is_empty() && starts[starts.len() - 1] > ends[ends.len() - 1]) {
        ends.push(n - 1);
    }

    let mut segments: Vec<Drawdown> = Vec::new();

    for (s_idx, e_idx) in starts.into_iter().zip(ends.into_iter()) {
        if s_idx > e_idx || s_idx >= n || e_idx >= n {
            continue;
        }

        // Find trough (minimum drawdown) within [s_idx, e_idx]
        let mut trough_idx = s_idx;
        let mut min_dd = drawdowns[s_idx];
        for i in s_idx..=e_idx {
            let dd = drawdowns[i];
            if dd < min_dd {
                min_dd = dd;
                trough_idx = i;
            }
        }

        let start_date = returns.dates[s_idx];
        let end_date = returns.dates[e_idx];
        let duration_days = (end_date - start_date).num_days() + 1;

        segments.push(Drawdown {
            start: start_date,
            trough: returns.dates[trough_idx],
            end: end_date,
            depth: min_dd,
            duration: duration_days as u32,
        });
    }

    segments
}

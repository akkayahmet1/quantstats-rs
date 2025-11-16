use crate::utils::ReturnSeries;

#[derive(Clone, Debug)]
pub struct PerformanceMetrics {
    pub total_return: f64,
    pub annualized_return: f64,
    pub annualized_volatility: f64,
    pub sharpe_ratio: f64,
    pub max_drawdown: f64,
    pub max_drawdown_duration: u32,
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

    let volatility = annualized_volatility(&returns.values, periods_per_year);
    let sharpe_ratio = if volatility > 0.0 {
        (annualized_return - rf) / volatility
    } else {
        0.0
    };

    let (max_drawdown, max_duration) = max_drawdown(&returns.values);
    let (best_day, worst_day) = best_and_worst(&returns.values);

    PerformanceMetrics {
        total_return,
        annualized_return,
        annualized_volatility: volatility,
        sharpe_ratio,
        max_drawdown,
        max_drawdown_duration: max_duration,
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
    let clean: Vec<f64> = returns.iter().copied().filter(|v| !v.is_nan()).collect();

    if clean.len() < 2 {
        return 0.0;
    }

    let mean = clean.iter().sum::<f64>() / clean.len() as f64;
    let var = clean
        .iter()
        .map(|r| {
            let diff = r - mean;
            diff * diff
        })
        .sum::<f64>()
        / (clean.len() as f64 - 1.0);

    var.sqrt() * (periods_per_year as f64).sqrt()
}

fn max_drawdown(returns: &[f64]) -> (f64, u32) {
    let mut equity = 1.0;
    let mut peak = 1.0;
    let mut max_dd = 0.0;
    let mut max_duration = 0_u32;
    let mut current_duration = 0_u32;

    for r in returns.iter().copied().filter(|v| !v.is_nan()) {
        equity *= 1.0 + r;
        if equity > peak {
            peak = equity;
            current_duration = 0;
        } else {
            current_duration += 1;
            let dd = equity / peak - 1.0;
            if dd < max_dd {
                max_dd = dd;
                max_duration = current_duration;
            }
        }
    }

    (max_dd, max_duration)
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


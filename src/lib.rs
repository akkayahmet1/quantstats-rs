mod reports;
mod stats;
mod utils;
mod plots;

pub use crate::reports::{html, HtmlReportError, HtmlReportOptions};
pub use crate::stats::PerformanceMetrics;
pub use crate::utils::{DataError, ReturnSeries};

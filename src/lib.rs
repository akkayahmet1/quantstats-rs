mod reports;
mod stats;
mod utils;

pub use crate::reports::{html, HtmlReportError, HtmlReportOptions};
pub use crate::stats::PerformanceMetrics;
pub use crate::utils::{DataError, ReturnSeries};

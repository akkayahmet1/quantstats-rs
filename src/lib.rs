mod plots;
mod reports;
mod stats;
mod utils;

pub use crate::reports::{HtmlReportError, HtmlReportOptions, html};
pub use crate::stats::PerformanceMetrics;
pub use crate::utils::{DataError, ReturnSeries};

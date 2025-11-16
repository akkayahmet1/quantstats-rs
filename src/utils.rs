use chrono::NaiveDate;

#[derive(Debug)]
pub enum DataError {
    Empty,
    LengthMismatch { dates: usize, values: usize },
}

impl std::fmt::Display for DataError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DataError::Empty => write!(f, "time series is empty"),
            DataError::LengthMismatch { dates, values } => write!(
                f,
                "time series length mismatch: {} dates vs {} values",
                dates, values
            ),
        }
    }
}

impl std::error::Error for DataError {}

#[derive(Clone, Debug)]
pub struct ReturnSeries {
    pub dates: Vec<NaiveDate>,
    pub values: Vec<f64>,
    pub name: Option<String>,
}

impl ReturnSeries {
    pub fn new(
        dates: Vec<NaiveDate>,
        values: Vec<f64>,
        name: Option<String>,
    ) -> Result<Self, DataError> {
        if dates.is_empty() || values.is_empty() {
            return Err(DataError::Empty);
        }

        if dates.len() != values.len() {
            return Err(DataError::LengthMismatch {
                dates: dates.len(),
                values: values.len(),
            });
        }

        let mut paired: Vec<(NaiveDate, f64)> = dates.into_iter().zip(values.into_iter()).collect();
        paired.sort_by_key(|(d, _)| *d);

        let (sorted_dates, sorted_values): (Vec<_>, Vec<_>) = paired.into_iter().unzip();

        Ok(Self {
            dates: sorted_dates,
            values: sorted_values,
            name,
        })
    }

    pub fn len(&self) -> usize {
        self.dates.len()
    }

    pub fn is_empty(&self) -> bool {
        self.dates.is_empty()
    }

    pub fn date_range(&self) -> Option<(NaiveDate, NaiveDate)> {
        if self.dates.is_empty() {
            None
        } else {
            Some((
                *self.dates.first().expect("len checked"),
                *self.dates.last().expect("len checked"),
            ))
        }
    }
}

pub fn align_start_dates(
    a: &ReturnSeries,
    b: &ReturnSeries,
) -> (ReturnSeries, ReturnSeries) {
    let idx_a = first_non_zero_index(&a.values).unwrap_or(0);
    let idx_b = first_non_zero_index(&b.values).unwrap_or(0);
    let start_idx = idx_a.max(idx_b);

    let slice_a = ReturnSeries {
        dates: a.dates[start_idx..].to_vec(),
        values: a.values[start_idx..].to_vec(),
        name: a.name.clone(),
    };

    let slice_b = ReturnSeries {
        dates: b.dates[start_idx..].to_vec(),
        values: b.values[start_idx..].to_vec(),
        name: b.name.clone(),
    };

    (slice_a, slice_b)
}

fn first_non_zero_index(values: &[f64]) -> Option<usize> {
    values
        .iter()
        .position(|v| !v.is_nan() && *v != 0.0)
}


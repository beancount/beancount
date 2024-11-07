use chrono::NaiveDate;

/// A civil date representation, following the Abseil civil time specification.
/// See https://abseil.io/blog/20181010-civil-time
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Date {
    /// Year component of the date
    pub year: Option<i32>,
    /// Month component of the date (1-12)
    pub month: Option<i32>,
    /// Day component of the date (1-31)
    pub day: Option<i32>,
}

impl Date {
    /// Creates a new Date with the given components
    pub fn new(year: Option<i32>, month: Option<i32>, day: Option<i32>) -> Self {
        Self { year, month, day }
    }

    /// Attempts to convert the Date into a chrono::NaiveDate
    ///
    /// Returns None if any components are missing or if the date is invalid
    pub fn to_naive_date(&self) -> Option<NaiveDate> {
        match (self.year, self.month, self.day) {
            (Some(y), Some(m), Some(d)) => NaiveDate::from_ymd_opt(y, m as u32, d as u32),
            _ => None,
        }
    }

    /// Creates a Date from a chrono::NaiveDate
    pub fn from_naive_date(date: NaiveDate) -> Self {
        Self {
            year: Some(date.year()),
            month: Some(date.month() as i32),
            day: Some(date.day() as i32),
        }
    }
}

impl Default for Date {
    fn default() -> Self {
        Self {
            year: None,
            month: None,
            day: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_date_creation() {
        let date = Date::new(Some(2024), Some(3), Some(15));
        assert_eq!(date.year, Some(2024));
        assert_eq!(date.month, Some(3));
        assert_eq!(date.day, Some(15));
    }

    #[test]
    fn test_date_conversion() {
        let date = Date::new(Some(2024), Some(3), Some(15));
        let naive = date.to_naive_date();
        assert!(naive.is_some());
        
        let converted_back = Date::from_naive_date(naive.unwrap());
        assert_eq!(date, converted_back);
    }

    #[test]
    fn test_invalid_date() {
        let date = Date::new(Some(2024), Some(2), Some(30)); // Invalid - February 30th
        assert!(date.to_naive_date().is_none());
    }

    #[test]
    fn test_missing_components() {
        let date = Date::new(Some(2024), None, Some(15));
        assert!(date.to_naive_date().is_none());
    }
}

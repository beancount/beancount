use std::collections::HashMap;

/// Summarized precision statistics data used for number formatting
#[derive(Debug, Clone, PartialEq)]
pub struct PrecisionStats {
    /// Collection of currency pair statistics
    pub pairs: Vec<Pair>,
}

/// Statistics for a specific currency pair
#[derive(Debug, Clone, PartialEq)]
pub struct Pair {
    /// Quote currency
    pub quote: Option<String>,
    
    /// Base currency
    pub base: Option<String>,
    
    /// Indicates whether the pair uses signs
    pub has_sign: Option<bool>,
    
    /// Maximum number of digits before the decimal point
    pub max_integer_digits: Option<i32>,
    
    /// Mode for exponent handling
    pub exponent_mode: Option<i32>,
    
    /// Maximum exponent value
    pub exponent_max: Option<i32>,
    
    /// Map of exponent values to their frequencies
    pub exponents: HashMap<i32, i32>,
}

impl PrecisionStats {
    /// Creates a new empty PrecisionStats
    pub fn new() -> Self {
        Self { pairs: Vec::new() }
    }

    /// Adds a pair to the statistics
    pub fn add_pair(&mut self, pair: Pair) {
        self.pairs.push(pair);
    }

    /// Finds a pair by quote and base currencies
    pub fn find_pair(&self, quote: &str, base: &str) -> Option<&Pair> {
        self.pairs.iter().find(|p| {
            p.quote.as_deref() == Some(quote) && 
            p.base.as_deref() == Some(base)
        })
    }
}

impl Default for PrecisionStats {
    fn default() -> Self {
        Self::new()
    }
}

impl Pair {
    /// Creates a new currency Pair with default values
    pub fn new(quote: String, base: String) -> Self {
        Self {
            quote: Some(quote),
            base: Some(base),
            has_sign: None,
            max_integer_digits: None,
            exponent_mode: None,
            exponent_max: None,
            exponents: HashMap::new(),
        }
    }

    /// Builds a new Pair with a fluent interface
    pub fn builder() -> PairBuilder {
        PairBuilder::new()
    }

    /// Records an exponent occurrence
    pub fn record_exponent(&mut self, exponent: i32) {
        *self.exponents.entry(exponent).or_insert(0) += 1;
    }

    /// Gets the most common exponent
    pub fn most_common_exponent(&self) -> Option<i32> {
        self.exponents
            .iter()
            .max_by_key(|&(_, count)| count)
            .map(|(exponent, _)| *exponent)
    }
}

/// Builder for creating Pair instances with a fluent interface
#[derive(Default)]
pub struct PairBuilder {
    quote: Option<String>,
    base: Option<String>,
    has_sign: Option<bool>,
    max_integer_digits: Option<i32>,
    exponent_mode: Option<i32>,
    exponent_max: Option<i32>,
    exponents: HashMap<i32, i32>,
}

impl PairBuilder {
    /// Creates a new PairBuilder
    pub fn new() -> Self {
        Self::default()
    }

    /// Sets the quote currency
    pub fn quote(mut self, quote: String) -> Self {
        self.quote = Some(quote);
        self
    }

    /// Sets the base currency
    pub fn base(mut self, base: String) -> Self {
        self.base = Some(base);
        self
    }

    /// Sets whether the pair has signs
    pub fn has_sign(mut self, has_sign: bool) -> Self {
        self.has_sign = Some(has_sign);
        self
    }

    /// Sets the maximum integer digits
    pub fn max_integer_digits(mut self, digits: i32) -> Self {
        self.max_integer_digits = Some(digits);
        self
    }

    /// Sets the exponent mode
    pub fn exponent_mode(mut self, mode: i32) -> Self {
        self.exponent_mode = Some(mode);
        self
    }

    /// Sets the maximum exponent
    pub fn exponent_max(mut self, max: i32) -> Self {
        self.exponent_max = Some(max);
        self
    }

    /// Adds an exponent occurrence
    pub fn add_exponent(mut self, exponent: i32, count: i32) -> Self {
        self.exponents.insert(exponent, count);
        self
    }

    /// Builds the Pair instance
    pub fn build(self) -> Pair {
        Pair {
            quote: self.quote,
            base: self.base,
            has_sign: self.has_sign,
            max_integer_digits: self.max_integer_digits,
            exponent_mode: self.exponent_mode,
            exponent_max: self.exponent_max,
            exponents: self.exponents,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_precision_stats_creation() {
        let mut stats = PrecisionStats::new();
        let pair = Pair::new("USD".to_string(), "EUR".to_string());
        stats.add_pair(pair);
        assert_eq!(stats.pairs.len(), 1);
    }

    #[test]
    fn test_pair_builder() {
        let pair = Pair::builder()
            .quote("USD".to_string())
            .base("EUR".to_string())
            .has_sign(true)
            .max_integer_digits(10)
            .exponent_mode(1)
            .exponent_max(5)
            .add_exponent(2, 3)
            .build();

        assert_eq!(pair.quote.as_deref(), Some("USD"));
        assert_eq!(pair.base.as_deref(), Some("EUR"));
        assert_eq!(pair.has_sign, Some(true));
        assert_eq!(pair.max_integer_digits, Some(10));
        assert_eq!(pair.exponent_mode, Some(1));
        assert_eq!(pair.exponent_max, Some(5));
        assert_eq!(pair.exponents.get(&2), Some(&3));
    }

    #[test]
    fn test_find_pair() {
        let mut stats = PrecisionStats::new();
        let pair = Pair::new("USD".to_string(), "EUR".to_string());
        stats.add_pair(pair);

        let found = stats.find_pair("USD", "EUR");
        assert!(found.is_some());
        
        let not_found = stats.find_pair("USD", "GBP");
        assert!(not_found.is_none());
    }

    #[test]
    fn test_record_and_most_common_exponent() {
        let mut pair = Pair::new("USD".to_string(), "EUR".to_string());
        
        pair.record_exponent(2);
        pair.record_exponent(2);
        pair.record_exponent(3);

        assert_eq!(pair.most_common_exponent(), Some(2));
    }
}

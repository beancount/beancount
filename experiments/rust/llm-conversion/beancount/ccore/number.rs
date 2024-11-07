use rust_decimal::Decimal;
use std::convert::TryFrom;

/// A variant type for decimal number representation following the mpdecimal IEEE 754 format.
#[derive(Debug, Clone, PartialEq)]
pub enum Number {
    /// Exact string representation of the number
    Exact(String),
    /// MPD decimal representation
    Mpd(Mpd),
    /// Triple representation for uint128
    Triple(MpdTriple),
}

/// Representation of mpdecimal's `mpd_t` with serialized data block
#[derive(Debug, Clone, PartialEq)]
pub struct Mpd {
    /// Flags for the decimal representation
    pub flags: i32,
    /// Exponent value
    pub exp: i32,
    /// Number of digits
    pub digits: i32,
    /// Length of data array
    pub len: i32,
    /// Packed data array representing the number
    pub data: Vec<u32>,
}

/// Representation of mpdecimal's `mpd_uint128_triple_t`
#[derive(Debug, Clone, PartialEq)]
pub struct MpdTriple {
    /// Tag value
    pub tag: u32,
    /// Sign indicator
    pub sign: u32,
    /// High 64 bits
    pub hi: u64,
    /// Low 64 bits
    pub lo: u64,
    /// Exponent value
    pub exp: i64,
}

impl Number {
    /// Creates a new Number from an exact string representation
    pub fn from_exact(s: String) -> Self {
        Number::Exact(s)
    }

    /// Creates a new Number from an Mpd representation
    pub fn from_mpd(mpd: Mpd) -> Self {
        Number::Mpd(mpd)
    }

    /// Creates a new Number from an MpdTriple representation
    pub fn from_triple(triple: MpdTriple) -> Self {
        Number::Triple(triple)
    }

    /// Attempts to convert the Number to a Decimal
    pub fn to_decimal(&self) -> Result<Decimal, &'static str> {
        match self {
            Number::Exact(s) => Decimal::from_str_exact(s)
                .map_err(|_| "Failed to parse exact string to Decimal"),
            Number::Mpd(_) => Err("MPD to Decimal conversion not implemented"),
            Number::Triple(_) => Err("Triple to Decimal conversion not implemented"),
        }
    }
}

impl TryFrom<&str> for Number {
    type Error = &'static str;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        // Validate that the string represents a valid number
        if s.parse::<Decimal>().is_ok() {
            Ok(Number::Exact(s.to_string()))
        } else {
            Err("Invalid number string")
        }
    }
}

impl Mpd {
    /// Creates a new Mpd instance
    pub fn new(flags: i32, exp: i32, digits: i32, len: i32, data: Vec<u32>) -> Self {
        Self {
            flags,
            exp,
            digits,
            len,
            data,
        }
    }
}

impl MpdTriple {
    /// Creates a new MpdTriple instance
    pub fn new(tag: u32, sign: u32, hi: u64, lo: u64, exp: i64) -> Self {
        Self {
            tag,
            sign,
            hi,
            lo,
            exp,
        }
    }

    /// Attempts to convert the triple to a u128 value
    pub fn to_u128(&self) -> u128 {
        ((self.hi as u128) << 64) | (self.lo as u128)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_number_from_exact() {
        let num = Number::from_exact("123.45".to_string());
        match num {
            Number::Exact(s) => assert_eq!(s, "123.45"),
            _ => panic!("Expected Exact variant"),
        }
    }

    #[test]
    fn test_number_to_decimal() {
        let num = Number::from_exact("123.45".to_string());
        let decimal = num.to_decimal().unwrap();
        assert_eq!(decimal.to_string(), "123.45");
    }

    #[test]
    fn test_invalid_number_string() {
        let result = Number::try_from("not_a_number");
        assert!(result.is_err());
    }

    #[test]
    fn test_mpd_creation() {
        let mpd = Mpd::new(0, -2, 3, 1, vec![12345]);
        assert_eq!(mpd.exp, -2);
        assert_eq!(mpd.digits, 3);
        assert_eq!(mpd.data, vec![12345]);
    }

    #[test]
    fn test_triple_to_u128() {
        let triple = MpdTriple::new(0, 0, 0xFFFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF, 0);
        assert_eq!(triple.to_u128(), u128::MAX);
    }
}

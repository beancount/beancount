use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// A set of valid booking method names for positions on accounts.
/// See http://furius.ca/beancount/doc/inventories for a full explanation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Booking {
    Unknown = 0,
    /// Reject ambiguous matches with an error.
    Strict = 1,
    /// Reject ambiguous matches with an error but if a lot matches the size exactly, 
    /// accept it the oldest.
    StrictWithSize = 2,
    /// Disable matching and accept the creation of mixed inventories.
    None = 3,
    /// Average cost booking: merge all matching lots before and after.
    Average = 4,
    /// First-in first-out in the case of ambiguity.
    Fifo = 5,
    /// Last-in first-out in the case of ambiguity.
    Lifo = 6,
}

/// Plugin configuration within ProcessingInfo
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Plugin {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    /// Configuration, most typically as a text-formatted protobuf message.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub config: Option<String>,
}

/// Data produced as a by-product of the parsing process. This is essentially
/// read-only state that is conceptually separate from the input options,
/// produced by the parser.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ProcessingInfo {
    /// The name of the top-level Beancount input file parsed from which the
    /// contents of the ledger have been extracted. This may be None, if no file
    /// was used.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub filename: Option<String>,

    /// A list of other filenames included.
    pub include: Vec<String>,

    /// A hash of some of the input data.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub input_hash: Option<String>,

    /// Computed processing stats, that can be used to initialize a number formatter.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub precision_stats: Option<PrecisionStats>,

    /// A set of all the commodities that we have seen in the file.
    pub commodities: Vec<String>,

    /// A list of Python modules containing transformation functions
    pub plugin: Vec<Plugin>,
}

/// Container for account types.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AccountTypes {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub assets: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub liabilities: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub equity: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub income: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub expenses: Option<String>,
}

/// Plugin processing mode enumeration
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ProcessingMode {
    Default = 0,
    Raw = 1,
}

/// Options that are visible to the user and that can be set.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Options {
    /// Root names of every account.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub account_types: Option<AccountTypes>,

    /// The title of this ledger / input file.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub title: Option<String>,

    /// Leaf name of the equity account used for summarizing previous transactions
    #[serde(skip_serializing_if = "Option::is_none")]
    pub account_previous_balances: Option<String>,

    /// Leaf name of the equity account used for transferring previous retained earnings
    #[serde(skip_serializing_if = "Option::is_none")]
    pub account_previous_earnings: Option<String>,

    /// Leaf name of the equity account used for inserting conversions
    #[serde(skip_serializing_if = "Option::is_none")]
    pub account_previous_conversions: Option<String>,

    /// Leaf name of the equity account used for transferring current retained earnings
    #[serde(skip_serializing_if = "Option::is_none")]
    pub account_current_earnings: Option<String>,

    /// Leaf name of the equity account used for inserting conversions during the exercise
    #[serde(skip_serializing_if = "Option::is_none")]
    pub account_current_conversions: Option<String>,

    /// The name of an account to be used to post unrealized gains to
    #[serde(skip_serializing_if = "Option::is_none")]
    pub account_unrealized_gains: Option<String>,

    /// The name of an account to be used to post to and accumulate rounding error
    #[serde(skip_serializing_if = "Option::is_none")]
    pub account_rounding: Option<String>,

    /// The imaginary currency used to convert all units for conversions
    #[serde(skip_serializing_if = "Option::is_none")]
    pub conversion_currency: Option<String>,

    /// Precision for evaluating arithmetic computations
    #[serde(skip_serializing_if = "Option::is_none")]
    pub decimal_evaluation_precision: Option<i32>,

    /// Explicit precision settings per currency or instrument pair
    pub precision: HashMap<String, String>,

    /// Mappings of currency to the tolerance
    pub inferred_tolerance_default: HashMap<String, String>,

    /// A multiplier for inferred tolerance values
    #[serde(skip_serializing_if = "Option::is_none")]
    pub inferred_tolerance_multiplier: Option<String>,

    /// Enable feature that expands the maximum tolerance inferred on transactions
    #[serde(skip_serializing_if = "Option::is_none")]
    pub infer_tolerance_from_cost: Option<bool>,

    /// A list of directory, relative to the CWD
    pub documents: Vec<String>,

    /// A list of currencies that we single out during reporting
    pub operating_currency: Vec<String>,

    /// Boolean for thousand separators in numbers
    #[serde(skip_serializing_if = "Option::is_none")]
    pub render_commas: Option<bool>,

    /// Plugin processing mode
    #[serde(skip_serializing_if = "Option::is_none")]
    pub plugin_processing_mode: Option<ProcessingMode>,

    /// The booking method to apply to ambiguous reductions of inventory lots
    #[serde(skip_serializing_if = "Option::is_none")]
    pub booking_method: Option<Booking>,

    /// Boolean to prepend directory name to PYTHONPATH
    #[serde(skip_serializing_if = "Option::is_none")]
    pub insert_pythonpath: Option<bool>,
}

// Note: The following type is assumed to be defined in another module
// and imported appropriately:
pub use beancount_core::PrecisionStats;

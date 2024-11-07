use serde::{Deserialize, Serialize};

/// Container for all data produced by the parser, and by Beancount in
/// general (after booking, interpolation, and plugins processing).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Ledger {
    /// A list of directives, with ownership.
    pub directives: Vec<Directive>,

    /// A list of errors encountered during parsing and processing.
    pub errors: Vec<Error>,

    /// Parsed options.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub options: Option<Options>,

    /// Processing details.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub info: Option<ProcessingInfo>,
}

// Note: The following types are assumed to be defined in other modules
// and imported appropriately:
pub use beancount_core::Directive;
pub use beancount_core::Error;
pub use beancount_options::{Options, ProcessingInfo};

/* Implementation note:
 * As noted in the original protobuf comment:
 * The final balances accumulated from processing the Ledger could
 * be saved and produced in the output, so that booking on top of it is cheap
 * and fast. This would be useful for importers, for example.
 */

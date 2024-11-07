use serde::{Deserialize, Serialize};

/// Arithmetic expressions to be evaluated later with Decimal context.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Expr {
    /// Which operator this represents in the tree.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub op: Option<ExprOp>,

    /// Terminal value for 'NUM'.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub number: Option<Number>,

    /// Binary arguments.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub arg1: Option<Box<Expr>>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub arg2: Option<Box<Expr>>,
}

/// Operators of expressions.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ExprOp {
    Num = 0,
    Add = 1,
    Sub = 2,
    Mul = 3,
    Div = 4,
    Neg = 5, // Unary
    Plus = 6, // Unary
    Paren = 7,
}

/// A pair of a number or expression.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ExprNumber {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub number: Option<Number>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub expr: Option<Expr>,
}

/// A container for all the parser specs produced on a posting.
/// These are the objects that directly represent the parsed input yet to be
/// interpolated and booked.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Spec {
    /// A UnitSpec instance.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub units: Option<UnitSpec>,

    /// A CostSpec instance. Warning: This partially filled field is only used in
    /// the production of the intermediate representation produced by the parser
    /// and never set in the final processed output from Beancount.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cost: Option<CostSpec>,

    /// A PriceSpec instance.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub price: Option<PriceSpec>,
}

/// A unit specification. This is like `Amount`, except that either the number or
/// currency can be missing.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnitSpec {
    /// Finalized value in 'number' and original expression in 'expr'.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub number: Option<Number>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub expr: Option<Expr>,

    /// The unit currency for the enclosing posting.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub currency: Option<String>,
}

/// A price specification. This is similar to `Amount`, except that either the
/// number or currency can be missing.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct PriceSpec {
    /// Finalized value in 'number' and original expression in 'expr'.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub number: Option<Number>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub expr: Option<Expr>,

    /// The quote currency for the enclosing posting.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub currency: Option<String>,

    /// A flag that determines whether the expression above refers to a per-unit
    /// price or a total price, yet to be calculated.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub is_total: Option<bool>,
}

/// A cost specification, which still requires to be filled in, and the final
/// per-unit numbers be calculated. This gets translated to Cost after booking
/// and interpolation.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CostSpec {
    /// A decimal number, exclusive the per-unit cost.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub per_unit: Option<ExprNumber>,

    /// A decimal number, the total cost/price.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub total: Option<ExprNumber>,

    /// A string, the commodity of the cost.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub currency: Option<String>,

    /// A datetime.date for the date that the lot was created at.
    /// There should always be a valid date.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub date: Option<Date>,

    /// A string for the label of this lot, or None, if there is no label.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub label: Option<String>,

    /// A flag explicitly indicating to merge all matching lots and compute the
    /// average cost.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub merge_cost: Option<bool>,
}

// Note: The following types are assumed to be defined in other modules
// and imported appropriately:
pub use beancount_core::Number;
pub use beancount_core::Date;

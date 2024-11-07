use std::time::SystemTime;
use rust_decimal::Decimal;
use chrono::NaiveDate;

// MetaValue represents different types of metadata values
#[derive(Debug, Clone)]
pub enum MetaValue {
    Text(String),
    Account(String),
    Currency(String),
    Tag(String),
    Link(String),
    Flag(String),
    Date(NaiveDate),
    Boolean(bool),
    Integer(i64),
    Number(Decimal),
    Amount(Box<Amount>),
}

// Meta represents a key-value metadata dictionary
#[derive(Debug, Clone)]
pub struct Meta {
    pub kv: Vec<(String, MetaValue)>,
}

// Amount represents a number of a particular unit
#[derive(Debug, Clone)]
pub struct Amount {
    pub number: Option<Decimal>,
    pub currency: Option<String>,
}

// Cost represents an Amount with date and label
#[derive(Debug, Clone)]
pub struct Cost {
    pub number: Option<Decimal>,
    pub currency: Option<String>,
    pub date: Option<NaiveDate>,
    pub label: Option<String>,
}

// Position represents a pair of units and optional cost
#[derive(Debug, Clone)]
pub struct Position {
    pub units: Option<Amount>,
    pub cost: Option<Cost>,
}

// Location represents a span of lines in an input file
#[derive(Debug, Clone)]
pub struct Location {
    pub filename: Option<String>,
    pub lineno: Option<i32>,
    pub lineno_end: Option<i32>,
}

// Posting represents an individual leg of a transaction
#[derive(Debug, Clone)]
pub struct Posting {
    pub location: Option<Location>,
    pub meta: Option<Meta>,
    pub date: Option<NaiveDate>,
    pub flag: Option<Vec<u8>>,
    pub account: Option<String>,
    pub position: Option<Position>,
    pub price: Option<Amount>,
}

// TxnPosting pairs a Posting with its parent Transaction
#[derive(Debug, Clone)]
pub struct TxnPosting {
    pub txn: Option<Box<Transaction>>,
    pub posting: Option<Posting>,
}

// Transaction is the main type representing financial transactions
#[derive(Debug, Clone)]
pub struct Transaction {
    pub flag: Option<Vec<u8>>,
    pub payee: Option<String>,
    pub narration: Option<String>,
    pub postings: Vec<Posting>,
}

// Open represents an "open account" directive
#[derive(Debug, Clone)]
pub struct Open {
    pub account: Option<String>,
    pub currencies: Vec<String>,
    pub booking: Option<BookingMethod>,
}

// Close represents a "close account" directive
#[derive(Debug, Clone)]
pub struct Close {
    pub account: Option<String>,
}

// Commodity represents an optional commodity declaration
#[derive(Debug, Clone)]
pub struct Commodity {
    pub currency: Option<String>,
}

// Pad represents a "pad this account" directive
#[derive(Debug, Clone)]
pub struct Pad {
    pub account: Option<String>,
    pub source_account: Option<String>,
}

// Balance represents a balance assertion directive
#[derive(Debug, Clone)]
pub struct Balance {
    pub account: Option<String>,
    pub amount: Option<Amount>,
    pub tolerance: Option<Decimal>,
    pub difference: Option<Decimal>,
}

// Note represents a note attached to an account
#[derive(Debug, Clone)]
pub struct Note {
    pub account: Option<String>,
    pub comment: Option<String>,
}

// Event represents a value change directive
#[derive(Debug, Clone)]
pub struct Event {
    pub type_: Option<String>,
    pub description: Option<String>,
}

// Query represents a named query declaration
#[derive(Debug, Clone)]
pub struct Query {
    pub name: Option<String>,
    pub query_string: Option<String>,
}

// Price represents a price declaration directive
#[derive(Debug, Clone)]
pub struct Price {
    pub currency: Option<String>,
    pub amount: Option<Amount>,
}

// Document represents a document file declaration
#[derive(Debug, Clone)]
pub struct Document {
    pub account: Option<String>,
    pub filename: Option<String>,
}

// Custom represents a custom directive
#[derive(Debug, Clone)]
pub struct Custom {
    pub type_: Option<String>,
    pub values: Vec<MetaValue>,
}

// DirectiveContent represents the various types of directives
#[derive(Debug, Clone)]
pub enum DirectiveContent {
    Transaction(Transaction),
    Price(Price),
    Balance(Balance),
    Open(Open),
    Close(Close),
    Commodity(Commodity),
    Pad(Pad),
    Document(Document),
    Note(Note),
    Event(Event),
    Query(Query),
    Custom(Custom),
}

// Directive represents any valid directive type
#[derive(Debug, Clone)]
pub struct Directive {
    pub location: Option<Location>,
    pub date: Option<NaiveDate>,
    pub meta: Option<Meta>,
    pub tags: Vec<String>,
    pub links: Vec<String>,
    pub content: DirectiveContent,
}

// Error represents a processing error
#[derive(Debug, Clone)]
pub struct Error {
    pub message: Option<String>,
    pub location: Option<Location>,
    pub dirhash: Option<String>,
}

// BookingMethod represents different booking methods
#[derive(Debug, Clone)]
pub enum BookingMethod {
    Undefined,
    Strict,
    None,
    Average,
    Fifo,
    Lifo,
}

// LedgerProto is the container for all directives and information
#[derive(Debug, Clone)]
pub struct LedgerProto {
    pub directives: Vec<Directive>,
    pub errors: Vec<Error>,
    pub options: Option<Options>,  // You'll need to define Options struct
    pub info: Option<ProcessingInfo>,  // You'll need to define ProcessingInfo struct
}

// Note: You'll need to implement Options and ProcessingInfo structs
// based on the referenced proto files that weren't included

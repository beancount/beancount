use std::error::Error;
use std::fmt;

use pest::Span;

use super::parser::Rule;

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Clone, Debug, PartialEq)]
pub enum ParseErrorKind {
    /// An error was encountered while converting string to a numeric representation.
    DecimalError { message: String },
    /// Input is invalid in some way.
    InvalidInput { message: String },
    /// Parser has reached an invalid state (most likely a bug in the parser).
    InvalidParserState { message: String },
}

#[derive(Debug)]
pub struct ParseError {
    /// The type of error.
    pub kind: ParseErrorKind,
    /// The (line, column) location of the error in the input.
    pub location: (usize, usize),
    pub source: Option<Box<dyn Error + 'static + Send + Sync>>,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            ParseErrorKind::DecimalError { message } => {
                write!(f, "{}", message)?;
            }
            ParseErrorKind::InvalidInput { message } => {
                write!(f, "Invalid input: {}", message)?;
            }
            ParseErrorKind::InvalidParserState { message } => {
                write!(f, "Parser has reached an invalid state (please report this as a bug): expected {}", message)?;
            }
        }
        write!(f, " at line {} column {}", self.location.0, self.location.1)
    }
}

impl Error for ParseError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        self.source
            .as_ref()
            .map(|e| e.as_ref() as &(dyn Error + 'static))
    }
}

impl ParseError {
    pub(crate) fn invalid_input_with_span<T: ToString>(msg: T, span: Span) -> ParseError {
        ParseError {
            kind: ParseErrorKind::InvalidInput {
                message: msg.to_string(),
            },
            location: span.start_pos().line_col(),
            source: None,
        }
    }

    pub(crate) fn invalid_state<T: ToString>(msg: T) -> ParseError {
        ParseError {
            kind: ParseErrorKind::InvalidParserState {
                message: msg.to_string(),
            },
            location: (0, 0),
            source: None,
        }
    }

    pub(crate) fn invalid_state_with_span<T: ToString>(msg: T, span: Span) -> ParseError {
        ParseError {
            kind: ParseErrorKind::InvalidParserState {
                message: msg.to_string(),
            },
            location: span.start_pos().line_col(),
            source: None,
        }
    }

    pub(crate) fn decimal_parse_error(err: rust_decimal::Error, span: Span) -> ParseError {
        let message = format!("error while parsing number: {}", err);
        let pest_error = pest::error::Error::new_from_span(
            pest::error::ErrorVariant::<Rule>::CustomError { message },
            span.clone(),
        );
        ParseError {
            kind: ParseErrorKind::DecimalError {
                message: format!("{}", pest_error),
            },
            location: span.start_pos().line_col(),
            source: Some(Box::new(err)),
        }
    }
}

impl From<pest::error::Error<Rule>> for ParseError {
    fn from(err: pest::error::Error<Rule>) -> Self {
        let err = err.renamed_rules(|rule| {
            match *rule {
                Rule::EOI => "end of input",
                Rule::alpha => "alpha",
                Rule::digit => "digit",
                Rule::WHITESPACE => "whitespace",
                Rule::COMMENT => "comment",
                Rule::boolean => "boolean value",
                Rule::indent => "indentation",
                Rule::eol => "end of line",
                Rule::asterisk => "asterisk ('*')",
                Rule::key => "key",
                Rule::value => "value",
                Rule::key_value => "key-value pair",
                Rule::key_value_line => "key-value line",
                Rule::eol_kv_list => "newline followed by key-value line",
                Rule::year => "4-digit year",
                Rule::month => "2-digit month",
                Rule::day => "2-digit day",
                Rule::date_separator => "date separator ('-' or '/')",
                Rule::date => "date",
                Rule::num => "number",
                Rule::int => "integer",
                Rule::separated_int => "integer with separators",
                Rule::operation => "numeric operator",
                Rule::add => "'+'",
                Rule::subtract => "'-'",
                Rule::multiply => "'*'",
                Rule::divide => "'/'",
                Rule::num_expr => "numeric expression",
                Rule::num_prefix => "numeric prefix",
                Rule::term => "numeric expression term",
                Rule::amount => "amount",
                Rule::double_quote => "double quotation mark",
                Rule::quoted_str => "quoted string",
                Rule::unquoted_str => "unquoted string",
                Rule::inner_quoted_str => "inner part of a quoted string",
                Rule::quoted_char => "a (possibly escaped) character",
                Rule::escape_sequence => "escape sequence",
                Rule::valid_non_letter_commodity_char => "valid commodity non-letter character",
                Rule::commodity_trailing => "trailing commodity",
                Rule::commodity => "commodity",
                Rule::commodity_list => "list of commodities",
                Rule::account_type => "an account category (first part of account name)",
                Rule::account_name_piece => "part of an account name",
                Rule::account => "an account name",
                Rule::tag_name => "tag name",
                Rule::link => "link",
                Rule::tag => "tag",
                Rule::tags_links => "sequence of tags and/or links",
                Rule::org_mode_title => "an Org-mode title",
                Rule::balance => "balance directive",
                Rule::close => "close directive",
                Rule::commodity_directive => "commodity directive",
                Rule::custom_value => "a value",
                Rule::custom_value_list => "one or more values",
                Rule::custom => "custom directive",
                Rule::document => "document directive",
                Rule::event => "event directive",
                Rule::include => "include directive",
                Rule::note => "note directive",
                Rule::open => "open directive",
                Rule::option => "option directive",
                Rule::pad => "pad directive",
                Rule::plugin => "plugin directive",
                Rule::price => "price directive",
                Rule::query => "query directive",
                Rule::pushtag => "pushtag",
                Rule::poptag => "poptag",
                Rule::transaction => "transaction directive",
                Rule::txn_flag => "transaction flag",
                Rule::flag_okay => "'txn' or '*'",
                Rule::flag_warning => "'!'",
                Rule::flag_padding => "'P'",
                Rule::flag_summarize => "'S'",
                Rule::flag_transfer => "'T'",
                Rule::flag_conversions => "'C'",
                Rule::flag_unrealized => "'U'",
                Rule::flag_returns => "'R'",
                Rule::flag_merging => "'M'",
                Rule::flag_forecasted => "'#'",
                Rule::txn_strings => "payee and narration strings",
                Rule::posting => "posting",
                Rule::posting_or_kv_list => "posting or metadata",
                Rule::indented_posting_or_kv_list => "indented posting or metadata",
                Rule::eol_posting_or_kv_list => "newline followed by indented posting or metadata",
                Rule::price_annotation => "price annotation",
                Rule::price_annotation_unit => "unit price annotation",
                Rule::price_annotation_total => "total price annotation",
                Rule::incomplete_amount => "amount and/or commodity",
                Rule::cost_spec => "cost spec",
                Rule::cost_spec_unit => "unit cost spec",
                Rule::cost_spec_total => "total cost spec",
                Rule::cost_comp_list => "comma-separated list of cost spec components",
                Rule::cost_comp => "cost spec component",
                Rule::compound_amount => "compound amount (amount with unit and total price)",
                Rule::file => "beancount file",
            }
            .to_string()
        });
        let location = match &err.line_col {
            pest::error::LineColLocation::Pos(ref p) => *p,
            pest::error::LineColLocation::Span(ref p, _) => *p,
        };
        ParseError {
            kind: ParseErrorKind::InvalidInput {
                message: format!("{}", err),
            },
            location,
            source: Some(Box::new(err)),
        }
    }
}

use crate::data::{self, Close, Custom, Metadata, Note, Open, Plugin};
use crate::error::{ParseError, ParseResult};
use crate::ParserError;
use chrono::NaiveDate;
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest_derive::Parser;
use pyo3::prelude::*;
use std::collections::{HashMap, HashSet};
use std::fmt::format;
use std::vec;

#[derive(Parser)]
#[grammar = "./grammar.pest"] // relative to src
pub struct MyParser;

use super::NewLineKind;
use serde::{Deserialize, Serialize};

pub const DEFAULT_LINE_WIDTH: u32 = 88;
pub const DEFAULT_INDENT_WIDTH: u8 = 2;
pub const DEFAULT_NEW_LINE_KIND: NewLineKind = NewLineKind::LF;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Configuration {
  pub line_width: u32,
  pub indent_width: u8,
  #[serde(rename = "new_line")]
  pub new_line: NewLineKind,
  #[serde(default)]
  pub prefix_width: Option<usize>,
  #[serde(default)]
  pub num_width: Option<usize>,
}

impl Default for Configuration {
  fn default() -> Self {
    Self {
      line_width: DEFAULT_LINE_WIDTH,
      indent_width: DEFAULT_INDENT_WIDTH,
      new_line: DEFAULT_NEW_LINE_KIND,
      prefix_width: None,
      num_width: None,
    }
  }
}

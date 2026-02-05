use std::fs;
use std::path::Path;

use anyhow::{Context, Result};
use beancount_parser::parse_lossy;

fn main() -> Result<()> {
  let path = std::env::args()
    .nth(1)
    .map(|p| p.into())
    .unwrap_or_else(|| std::ffi::OsString::from("-"));

  let content = if path == std::ffi::OsStr::new("-") {
    use std::io::Read;
    let mut buf = String::new();
    std::io::stdin()
      .read_to_string(&mut buf)
      .context("failed to read stdin")?;
    buf
  } else {
    let path_ref = Path::new(&path);
    fs::read_to_string(path_ref)
      .with_context(|| format!("failed to read {}", path_ref.display()))?
  };

  let directives = parse_lossy(&content);
  let pretty = serde_json::to_string_pretty(&directives)?;
  println!("{}", pretty);

  Ok(())
}

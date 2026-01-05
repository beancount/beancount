#[test]
fn format_and_check_fixtures() {
  use std::ffi::OsStr;
  use std::fs;
  use std::path::Path;

  use beancount_formatter::configuration::{Configuration, NewLineKind};
  use beancount_formatter::format;
  use serde::Deserialize;

  #[derive(Debug, Default, Deserialize)]
  #[serde(default)]
  struct PartialConfiguration {
    line_width: Option<u32>,
    indent_width: Option<u8>,
    new_line: Option<NewLineKind>,
    prefix_width: Option<usize>,
    num_width: Option<usize>,
  }

  impl PartialConfiguration {
    fn apply_to(self, mut config: Configuration) -> Configuration {
      config.line_width = self.line_width.unwrap_or(config.line_width);
      config.indent_width = self.indent_width.unwrap_or(config.indent_width);
      config.new_line = self.new_line.unwrap_or(config.new_line);
      config.prefix_width = self.prefix_width.or(config.prefix_width);
      config.num_width = self.num_width.or(config.num_width);

      config
    }
  }

  fn assert_eq_with_diff(expected: &str, actual: &str) {
    if expected == actual {
      return;
    }

    let diff = similar::TextDiff::from_lines(expected, actual)
      .unified_diff()
      .header("expected", "actual")
      .to_string();

    eprintln!("{diff}");
    panic!("text mismatch; see unified diff above");
  }

  fn run_case(input_path: &Path) {
    let update_expected = std::env::var("TEST_UPDATE_EXPECTED")
      .map(|v: String| v == "1" || v.eq_ignore_ascii_case("true"))
      .unwrap_or(false)
      || std::env::args().any(|a| a == "--update");

    let file_name = input_path
      .file_name()
      .and_then(|s| s.to_str())
      .expect("input path has non-utf8 filename");

    let case_name = file_name
      .strip_suffix(".input.bean")
      .expect("input file must end with .input.bean");

    let dir = input_path.parent().expect("input file has no parent dir");

    let config_path = dir.join(format!("{case_name}.config.json"));
    let expected_path = dir.join(format!("{case_name}.expected.bean"));

    let input =
      fs::read_to_string(input_path).unwrap_or_else(|e| panic!("Failed to read input {}: {e}", input_path.display()));

    let config = if config_path.exists() {
      let json = fs::read_to_string(&config_path)
        .unwrap_or_else(|e| panic!("Failed to read config {}: {e}", config_path.display()));
      let partial: PartialConfiguration =
        serde_json::from_str(&json).unwrap_or_else(|e| panic!("Invalid JSON in {}: {e}", config_path.display()));
      partial.apply_to(Configuration::default())
    } else {
      Configuration::default()
    };

    // Use case name as the filename for nicer error messages and meta handling.
    let formatted = format(Some(&format!("{case_name}.bean")), &input, &config)
      .unwrap_or_else(|e| panic!("format() failed for {case_name}: {e:?}"));

    if !expected_path.exists() {
      fs::write(&expected_path, &formatted)
        .unwrap_or_else(|e| panic!("Failed to write expected {}: {e}", expected_path.display()));

      panic!(
        "Missing expected file; wrote formatted output to {}",
        expected_path.display()
      );
    }

    let expected = fs::read_to_string(&expected_path)
      .unwrap_or_else(|e| panic!("Failed to read expected {}: {e}", expected_path.display()));

    // Fixtures in git may be checked out with LF endings even when the case
    // config requests CRLF. Convert expected text to the configured newline
    // style before comparing.
    let expected = match config.new_line {
      NewLineKind::LF => expected.replace("\r\n", "\n"),
      NewLineKind::CRLF => {
        let lf = expected.replace("\r\n", "\n");
        lf.replace("\n", "\r\n")
      }
    };

    if expected == formatted {
      return;
    }

    if update_expected {
      fs::write(&expected_path, &formatted)
        .unwrap_or_else(|e| panic!("Failed to write expected {}: {e}", expected_path.display()));
      eprintln!("updated expected fixture {}", expected_path.display());
    } else {
      assert_eq_with_diff(&expected, &formatted);
    }
  }

  let fixtures_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/format-and-check");
  let mut input_files = vec![];
  for entry in fs::read_dir(&fixtures_dir)
    .unwrap_or_else(|e| panic!("Failed to read fixtures dir {}: {e}", fixtures_dir.display()))
  {
    let entry = entry.unwrap_or_else(|e| panic!("Failed to read fixtures dir entry: {e}"));
    let path = entry.path();
    if path.extension() != Some(OsStr::new("bean")) {
      continue;
    }
    if !path
      .file_name()
      .and_then(|s| s.to_str())
      .is_some_and(|s| s.ends_with(".input.bean"))
    {
      continue;
    }
    input_files.push(path);
  }

  input_files.sort();
  assert!(
    !input_files.is_empty(),
    "No fixtures found in {} (expected at least one `*.input.bean`)",
    fixtures_dir.display()
  );

  for input_path in input_files {
    run_case(&input_path);
  }
}

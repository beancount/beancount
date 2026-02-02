use beancount_chumsky::parse_str as parse_chumsky;
use beancount_parser::core::Include;

fn parse_include(input: &str, filename: &str) -> Include {
  let ast = parse_chumsky(input, filename).expect("chumsky parse failed");
  let core = beancount_parser::normalize_directives(ast).expect("chumsky normalize failed");
  assert_eq!(core.len(), 1, "expected a single directive");
  match core.into_iter().next().expect("directive") {
    beancount_parser::core::CoreDirective::Include(include) => include,
    other => panic!("expected include directive, got {other:?}"),
  }
}

#[test]
fn include_directive_resolves_path() {
  let filename = "books/main.bean";
  let input = "include \"includes/extra.bean\"";

  let include = parse_include(input, filename);

  let expected = Include {
    meta: include.meta.clone(),
    span: include.span,
    filename: if cfg!(windows) {
      "books\\includes\\extra.bean".to_string()
    } else {
      "books/includes/extra.bean".to_string()
    },
  };

  assert_eq!(include, expected);
}

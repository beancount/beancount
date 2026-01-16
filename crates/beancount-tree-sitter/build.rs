fn main() {
  let src_dir = std::path::Path::new("src");

  let mut c_config = cc::Build::new();
  c_config
    .std("c11")
    .include(src_dir)
    .flag_if_supported("-Wno-unused-parameter")
    .flag_if_supported("-Wno-unused-but-set-variable")
    .flag_if_supported("-Wno-trigraphs")
    .flag_if_supported("-Wno-incompatible-pointer-types")
    .flag_if_supported("-Wno-unused-function")
    .flag_if_supported("-Wno-unused-label")
    .flag_if_supported("-O");

  #[cfg(target_env = "msvc")]
  c_config.flag_if_supported("/utf-8");

  let target = std::env::var("TARGET").unwrap_or_default();
  if target.starts_with("wasm32-") {
    let Ok(wasm_headers) = std::env::var("DEP_TREE_SITTER_LANGUAGE_WASM_HEADERS") else {
      panic!(
        "Environment variable DEP_TREE_SITTER_LANGUAGE_WASM_HEADERS must be set by the language crate"
      );
    };

    let Ok(wasm_src) =
      std::env::var("DEP_TREE_SITTER_LANGUAGE_WASM_SRC").map(std::path::PathBuf::from)
    else {
      panic!(
        "Environment variable DEP_TREE_SITTER_LANGUAGE_WASM_SRC must be set by the language crate"
      );
    };

    c_config.include(&wasm_headers);
    c_config.files([
      wasm_src.join("stdio.c"),
      wasm_src.join("stdlib.c"),
      wasm_src.join("string.c"),
    ]);
  }

  let parser_path = src_dir.join("parser.c");
  c_config.file(&parser_path);
  println!("cargo:rerun-if-changed={}", parser_path.to_str().unwrap());

  let scanner_path = src_dir.join("scanner.c");
  // if scanner_path.exists() {
  c_config.file(&scanner_path);
  println!("cargo:rerun-if-changed={}", scanner_path.to_str().unwrap());
  // }

  c_config.compile("tree-sitter-beancount");
}

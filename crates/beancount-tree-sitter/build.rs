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

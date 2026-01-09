This project provide core library beancount.

it's original use a c parser and I'm replacing it with a rust parser.

It should export same python api and just replace the parser.

It's based on a tree-sitter parser that generated from `crates/beancount-tree-sitter/grammar.js`, `crates/beancount-tree-sitter/src/grammar.json` and `crates/beancount-tree-sitter/src/node-types.json` which you can find from project root directory.

crates:

- `crates/beancount-tree-sitter/`: this crate contains tree-sitter parser.
- `crates/beancount-parser/`: the parser that parse string into tree-sitter Node and convert them to our internal AST and core data type, which is called `CoreDirective`.
- `crates/beancount-parser-py/`: this create convert `CoreDirective` to beancount.core python types defined in the python file `beancount/core/data.py`. you do not add any rust tests to this crate because it require a linkage to python to run these tests. if you want to test this crate, write test code in python.

## Something need to notice:

1. rust is a language that need compiling. so when you update rust code and try to check if parser works as expected in python, you should call shell comamnd `maturin develop` first.
2. the crate `crates/beancount-parser-py/` will compiled to `beancount/parser/_parser_rust.abi3.so` and will be imported as `beancount/parser/_parser_rust`, when you update the signature, you must update type-stub accordingly.

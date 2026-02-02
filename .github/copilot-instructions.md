This project provide core library beancount.

it's original use a c parser and I'm replacing it with a rust parser.

It should export same python api and just replace the parser.

It's based on a Chumsky parser implemented in `crates/chumsky/`.

crates:

- `crates/chumsky/`: this crate contains the Chumsky parser and shared AST types.
- `crates/parser/`: the core types and normalization logic; `parse_str` is powered by the Chumsky parser.
- `crates/parser-py/`: this create convert `CoreDirective` to beancount.core python types defined in the python file `beancount/core/data.py`. you do not add any rust tests to this crate because it require a linkage to python to run these tests. if you want to test this crate, write test code in python.

## Something need to notice:

1. rust is a language that need compiling. so when you update rust code and try to check if parser works as expected in python, you should call shell comamnd `maturin develop` first.
2. the crate `crates/parser-py/` will compiled to `beancount/parser/_parser_rust`, when you update the signature, you must update type-stub accordingly.

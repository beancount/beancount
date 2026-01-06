This project provide core library beancount.

it's original use a c parser and I'm replacing it with a rust parser.

It should export same python api and just replace the parser.

It's based on a tree-sitter parser that generated from `./grammar.js`, `./grammer.json` and `./node-types.js` which you can find from project root directory.

crates:

- `./crates/beancount-parser/`: the parser that parse string into tree-sitter Node and convert them to our internal AST and core data type.
- `./crates/beancount-parser-py/`: this create convert core types to beancount.core python types defined in the python file `beancount/core/data.py`. you do not add any rust tests to this crate because it require a linkage to python to run these tests. if you want to test this crate, write test code in python.

## Something need to notice:

1. rust is a language that need compiling. so when you update rust code and try to check if parser works as expected in python, you should call shell comamnd `maturin develop` first.
2. our rust native library has a type stubs with it. when you update the signature, you must update type-stub accordingly.

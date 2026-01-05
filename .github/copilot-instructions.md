This project provide core library beancount.

it's original use a c parser and I'm replacing it with a rust parser.

It should export same python api and just replace the parser.

the parser is located in the `./crates/beancount-parser/`

It's based on a tree-sitter parser that generated from `./grammar.js`, `./grammer.json` and `./node-types.js` which you can find from project root directory.

#!/usr/bin/env python3
from distutils.core import setup, Extension

setup(
    name="beancount",
    package_dir = {'': 'src/python'},
    packages = ['beancount'],
    ext_modules=[
        Extension("beancount/parser/_parser", [
            "src/python/beancount/parser/lexer.c",
            "src/python/beancount/parser/grammar.c",
            "src/python/beancount/parser/parser.c"
            ]),
    ],
)

#!/usr/bin/env python3
from distutils.core import setup, Extension

setup(
    name="beancount",
    package_dir = {'': 'lib/python'},
    packages = ['beancount2'],
    ext_modules=[
        Extension("beancount2/parser/_parser", [
            "lib/python/beancount2/parser/lexer.c",
            "lib/python/beancount2/parser/grammar.c",
            "lib/python/beancount2/parser/parser.c"
            ]),
    ],
)

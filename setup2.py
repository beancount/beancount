#!/usr/bin/env python3
from distutils.core import setup, Extension

setup(
    name="beancount",
    package_dir = {'': 'lib/python'},
    packages = ['beancount2'],
    ext_modules=[
        Extension("beancount2/_parser", [
            "lib/python/beancount2/lexer.c",
            "lib/python/beancount2/grammar.c",
            "lib/python/beancount2/parser.c"
            ]),
    ],
)

#!/usr/bin/env python3
from distutils.core import setup, Extension

setup(
    name="beancount",
    ext_modules=[
        Extension("_beancount", [
            "beancount_lexer.c",
            "beancount_parser.c",
            "builder.c"
            ]),
    ],
)


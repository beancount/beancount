import io

from beancount.parser.grammar import Builder

class Parser:
    def __init__(self, builder: Builder, debug: bool) -> None: ...
    def parse(self, file: io.IOBase, filename: str, lineno: int) -> None: ...

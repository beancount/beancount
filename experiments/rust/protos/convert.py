#!/usr/bin/env python3
"""Parse protos in Python."""

import re
from enum import Enum, auto

import click
import logging
import pprint


class TokenType(Enum):
    KEYWORD = auto()
    IDENTIFIER = auto()
    NUMBER = auto()
    STRING = auto()
    COMMENT = auto()
    SYMBOL = auto()
    WHITESPACE = auto()  # We'll mostly ignore this for now


def tokenize(input_string):
    token_specification = [
        ("COMMENT", r"\/\/.*?$"),  # Match line comments
        (
            "BLOCK_COMMENT",
            r"\/*[\s\S]*?*\/",
        ),  # Match block comments, be careful with this one, it might need more escaping
        ("KEYWORD", r"\b(message|package|import|syntax|option|enum|service|rpc|returns)\b"),
        ("IDENTIFIER", r"[A-Za-z_]\w*"),
        ("NUMBER", r"\d+"),
        (
            "STRING",
            r'"(?:\\.|[^\\"])*"',
        ),  # Simplified for example; needs escaping for quotes inside strings
        ("SYMBOL", r"[{};=\[\]]"),
        ("WHITESPACE", r"\s+"),
    ]

    tokens = []
    for regex, token_type in token_specification:
        for mo in re.finditer(regex, input_string, re.MULTILINE):
            tokens.append((token_type, mo.group()))

    return [(t, v) for t, v in tokens if t != TokenType.WHITESPACE]


class ASTNode:
    def __init__(self, node_type, value=None, children=None):
        self.type = node_type
        self.value = value
        self.children = children or []

    def __str__(self):
        return f"{self.type} {self.value} {self.children}"


class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.current = 0

    def parse(self):
        nodes = []
        while self.current < len(self.tokens):
            token_type, token_value = self.tokens[self.current]
            if token_type == TokenType.KEYWORD:
                if token_value == "package":
                    nodes.append(self.parse_package())
                elif token_value in ["message", "enum", "service"]:
                    nodes.append(getattr(self, f"parse_{token_value}")())
            self.current += 1
        return ASTNode("ProtoFile", children=nodes)

    def parse_package(self):
        # Assumes syntax like: 'package some.package;'
        self.expect(TokenType.KEYWORD, "package")
        package_name = self.expect(TokenType.IDENTIFIER)
        self.expect(TokenType.SYMBOL, ";")
        return ASTNode("Package", package_name.value)

    def parse_message(self):
        self.expect(TokenType.KEYWORD, "message")
        message_name = self.expect(TokenType.IDENTIFIER)
        self.expect(TokenType.SYMBOL, "{")

        fields = []
        while (
            self.tokens[self.current][0] != TokenType.SYMBOL
            or self.tokens[self.current][1] != "}"
        ):
            if self.tokens[self.current][0] == TokenType.COMMENT:
                self.current += 1
            else:
                fields.append(self.parse_field())

        self.expect(TokenType.SYMBOL, "}")
        return ASTNode("Message", message_name.value, fields)

    def parse_field(self):
        type_name = self.expect(TokenType.IDENTIFIER)
        field_name = self.expect(TokenType.IDENTIFIER)
        self.expect(TokenType.SYMBOL, "=")
        field_number = self.expect(TokenType.NUMBER)
        self.expect(TokenType.SYMBOL, ";")
        return ASTNode(
            "Field", type=type_name.value, name=field_name.value, number=field_number.value
        )

    def expect(self, token_type, value=None):
        if self.current >= len(self.tokens):
            raise ValueError(f"Expected {token_type}, found end of tokens")
        token, token_value = self.tokens[self.current]
        if token != token_type:
            raise ValueError(f"Expected {token_type}, found {token}")
        if value is not None and token_value != value:
            raise ValueError(f"Expected '{value}', found '{token_value}'")
        self.current += 1
        return ASTNode(token_type, value=token_value)


def parse_proto_file(file_path):
    with open(file_path, "r") as file:
        proto_content = file.read()
    tokens = tokenize(proto_content)
    pprint.pprint(tokens)
    # parser = Parser(tokens)
    # return parser.parse()


@click.command()
@click.argument("filename", type=click.Path(exists=True))
def main(filename: str):
    ast = parse_proto_file(filename)
    print(ast)


if __name__ == "__main__":
    main()

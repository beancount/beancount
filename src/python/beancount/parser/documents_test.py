"""
Tests for documents.
"""
import unittest
import textwrap
import inspect
import sys

from beancount.parser import parsedoc
from beancount.parser import parser
from beancount.core.data import Transaction, Balance, Open, Close, Pad, Event, Price, Note


# class TestParserEntries(unittest.TestCase):
#     """Basic smoke test one entry of each kind."""


__incomplete__ = True

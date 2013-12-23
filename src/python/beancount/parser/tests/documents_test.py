"""
Tests for documents.
"""
import unittest
import textwrap
import inspect
import sys

from beancount.parser import parsedoc
from beancount.parser import parser
from beancount.core.data import Transaction, Check, Open, Close, Pad, Event, Price, Note
from beancount.core.data import format_entry


# class TestParserEntries(unittest.TestCase):
#     """Basic smoke test one entry of each kind."""


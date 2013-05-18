"""
Tests for tree_utils.
"""
from beancount2.utils import tree_utils

import sys
import unittest
import collections


Node = collections.namedtuple('Node', 'name children')

def create_test_tree():
    return tree_utils.TreeDict(lambda name: Node(name, []),
                               lambda node: node.name,
                               lambda node: node.children,
                               ':')

class TestTree(unittest.TestCase):

    def test_simple(self):
        tree = create_test_tree()
        tree['Assets:US:TD:Checking']
        tree['Assets:US:TD']
        tree['Assets:US:HSBC:Savings']
        tree['Assets:US:HSBC:Checking']
        tree.dump(sys.stdout)

    def test_simple2(self):
        tree = create_test_tree()
        tree['Assets']
        tree['Assets:US:HSBC:Savings']
        tree.dump(sys.stdout)

"""
Tests for tree_utils.
"""
from beancount2.utils import tree_utils

import sys
import unittest
import collections


Node = collections.namedtuple('Node', 'name children')


class TreeAdaptor:

    def create_node(self, name):
        return Node(name, [])

    def get_name(self, node):
        return node.name.split(':')[-1]

    def get_children(self, node):
        return node.children

def create_test_tree():
    return tree_utils.TreeDict(TreeAdaptor(), ':')

class TestTree(unittest.TestCase):

    def test_simple(self):
        tree = create_test_tree()
        tree['Assets:US:TD:Checking']
        tree['Assets:US:TD']
        tree['Assets:US:HSBC:Savings']
        tree['Assets:US:HSBC:Checking']
        print()
        for line, node in tree.render_lines():
            print('{:32}: {}'.format(line, repr(node.name)))

    def test_simple2(self):
        tree = create_test_tree()
        tree['Assets']
        tree['Assets:US:HSBC:Savings']
        print()
        for line, node in tree.render_lines():
            print('{:32}: {}'.format(line, repr(node.name)))

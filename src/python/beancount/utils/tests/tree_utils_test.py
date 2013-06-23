"""
Tests for tree_utils.
"""
from beancount.utils import tree_utils

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
        tree.get_create('Assets:US:TD:Checking')
        tree.get_create('Assets:US:TD')
        tree.get_create('Assets:US:HSBC:Savings')
        tree.get_create('Assets:US:HSBC:Checking')
        # FIXME: Need to add assets here.

    def test_simple2(self):
        tree = create_test_tree()
        tree.get_create('Assets')
        tree.get_create('Assets:US:HSBC:Savings')
        # FIXME: Need to add assets here.

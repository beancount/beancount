"""
Tests for tree_utils.
"""
from beancount.utils import tree_utils
from beancount.core import account

import unittest
import collections


Node = collections.namedtuple('Node', 'name children')


class TreeAdaptor:

    def create_node(self, name):
        return Node(name, [])

    def get_name(self, node):
        return node.name.split(account.sep)[-1]

    def get_children(self, node):
        return node.children


def create_tree():
    return tree_utils.TreeDict(TreeAdaptor(), account.sep)


class TestTree(unittest.TestCase):

    pass
    # def test_simple(self):
    #     tree = create_tree()
    #     tree.get_create('Assets:US:TD:Checking')
    #     tree.get_create('Assets:US:TD')
    #     tree.get_create('Assets:US:HSBC:Savings')
    #     tree.get_create('Assets:US:HSBC:Checking')
    #     # FIXME: Need to add assets here.

    # def test_simple2(self):
    #     tree = create_tree()
    #     tree.get_create('Assets')
    #     tree.get_create('Assets:US:HSBC:Savings')
    #     # FIXME: Need to add assets here.


__incomplete__ = True

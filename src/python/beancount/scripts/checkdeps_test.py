__author__ = "Martin Blais <blais@furius.ca>"

import unittest

from beancount.scripts import checkdeps


class TestCheckDeps(unittest.TestCase):

    def test_check_dependencies(self):
        # There isn't much to test here, just run it, it will call all the
        # functions in the module.
        deps = checkdeps.check_dependencies()
        self.assertTrue(deps)
        self.assertTrue(isinstance(deps, list))

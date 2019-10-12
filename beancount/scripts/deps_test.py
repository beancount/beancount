__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount.scripts import deps


class TestCheckDeps(unittest.TestCase):

    def test_check_dependencies(self):
        # There isn't much to test here, just run it, it will call all the
        # functions in the module.
        dependencies = deps.check_dependencies()
        self.assertTrue(dependencies)
        self.assertTrue(isinstance(dependencies, list))


if __name__ == '__main__':
    unittest.main()

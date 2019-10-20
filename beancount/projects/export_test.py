__copyright__ = "Copyright (C) 2014, 2016  Martin Blais"
__license__ = "GNU GPLv2"

from os import path
import unittest

from beancount.utils import test_utils
from beancount.projects import export


class TestExport(test_utils.TestCase):

    def test_export_basic(self):
        rootdir = test_utils.find_repository_root(__file__)
        example_beancount = path.join(rootdir, 'examples', 'example.beancount')
        with test_utils.capture('stdout', 'stderr'):
            result = test_utils.run_with_args(export.main,
                                              [example_beancount, '-o-'])
            self.assertEqual(0, result)


if __name__ == '__main__':
    unittest.main()

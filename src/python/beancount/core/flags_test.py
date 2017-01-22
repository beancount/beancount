__copyright__ = "Copyright (C) 2014, 2016-2017  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount.core import flags


class TestFlags(unittest.TestCase):

    ALLOW_NOT_UNIQUE = {'FLAG_IMPORT'}

    def test_unique_flags(self):
        names = set()
        values = set()
        for name, value in flags.__dict__.items():
            # pylint: disable=bad-continuation
            if (not name.startswith("FLAG_") or
                name in self.ALLOW_NOT_UNIQUE):
                continue
            names.add(name)
            values.add(value)
        self.assertEqual(len(names), len(values))

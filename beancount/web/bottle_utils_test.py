__copyright__ = "Copyright (C) 2014, 2016-2017  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount.web import bottle_utils


class TestAttrMapper(unittest.TestCase):

    def setUp(self):
        dictionary = {'a': 'the_a',
                      'b': 'the_b',
                      'c': 'the_c'}
        self.mapper = bottle_utils.AttrMapper(dictionary.__getitem__)

    def test_AttrMapper(self):
        self.assertTrue(self.mapper is not None)

    def test_AttrMapper_getattr(self):
        self.assertTrue(self.mapper.a, 'the_a')
        self.assertTrue(self.mapper.b, 'the_b')

    def test_AttrMapper_build(self):
        self.assertTrue(self.mapper.build('a'), 'the_a')
        self.assertTrue(self.mapper.build('b'), 'the_b')

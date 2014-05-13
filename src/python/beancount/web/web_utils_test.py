import collections
import re
import unittest

from beancount.web import web_utils


class TestWebUtils(unittest.TestCase):

    def test_clean_attributes(self):
        attributes = web_utils.clean_attributes(['a', 'a_b', 'aa_bb_cc'])
        self.assertEqual(['A', 'A B', 'Aa Bb Cc'], attributes)


    def test_render_tuples_to_html_table(self):

        Tup = collections.namedtuple('Tup', 'foo bar baz')

        tuples = [
            Tup("Malawi", "Lilongwe", "Kwacha"),
            Tup("Mali", "Bamako", "CFA franc"),
            Tup("Mauritania", "Nouakchott", "Ouguiya"),
            ]
        table = web_utils.render_tuples_to_html_table(
            tuples, ["Name", "Capital", "Currency"])

        self.assertTrue(re.search(r'<table\b', table))
        self.assertTrue(re.search('Capital', table))
        self.assertTrue(re.search('Mauritania', table))
        self.assertTrue(re.search(r'</table>', table))

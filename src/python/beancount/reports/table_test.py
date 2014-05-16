import collections
import textwrap
import unittest

from beancount.core.amount import Decimal
from beancount.reports import table


class TestWebUtils(unittest.TestCase):

    def test_attribute_to_title(self):
        attributes = list(map(table.attribute_to_title, ['a', 'a_b', 'aa_bb_cc']))
        self.assertEqual(['A', 'A B', 'Aa Bb Cc'], attributes)

    def test_render_tuples_to_html_table(self):
        Tup = collections.namedtuple('Tup', 'country capital currency amount')

        tuples = [
            Tup("Malawi", "Lilongwe", "Kwacha", Decimal("0.11")),
            Tup("Mali", "Bamako", "CFA franc", Decimal("0.22")),
            Tup("Mauritania", "Nouakchott", "Ouguiya", Decimal("0.33")),
            ]
        html = table.render_tuples_to_html_table(
            tuples, ["country",
                     ("capital",),
                     ("currency", "Currency"),
                     ("amount", "Amount", "{:.3f}".format)],
            classes=['high-class'])

        expected = textwrap.dedent("""\
            <table class="high-class">
              <thead>
                <tr>
                  <th>Country</th>
                  <th>Capital</th>
                  <th>Currency</th>
                  <th>Amount</th>
                </tr>
              </thead>
              <tbody>
                <tr>
                  <td>Malawi</td>
                  <td>Lilongwe</td>
                  <td>Kwacha</td>
                  <td>0.110</td>
                </tr>
                <tr>
                  <td>Mali</td>
                  <td>Bamako</td>
                  <td>CFA franc</td>
                  <td>0.220</td>
                </tr>
                <tr>
                  <td>Mauritania</td>
                  <td>Nouakchott</td>
                  <td>Ouguiya</td>
                  <td>0.330</td>
                </tr>
              </tbody>
            </table>
        """)
        self.assertEqual(expected, html)

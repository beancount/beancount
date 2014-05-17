import collections
import textwrap
import unittest

from beancount.core.amount import Decimal
from beancount.reports import table


class TestTable(unittest.TestCase):

    def test_attribute_to_title(self):
        attributes = list(map(table.attribute_to_title, ['a', 'a_b', 'aa_bb_cc']))
        self.assertEqual(['A', 'A B', 'Aa Bb Cc'], attributes)

    def test_create_table(self):
        Tup = collections.namedtuple('Tup', 'country capital currency amount')

        tuples = [
            Tup("Malawi", "Lilongwe", "Kwacha", Decimal("0.111")),
            Tup("Mali", "Bamako", "CFA franc", Decimal("0.222")),
            Tup("Mauritania", "Nouakchott", "Ouguiya", Decimal("0.333")),
            ]
        table_object = table.create_table(
            tuples, ["country",
                     ("capital",),
                     ("currency", "Currency"),
                     ("amount", "Amount", "{:.3f}".format)],
            )
        return table_object

    def test_table_to_html(self):
        table_object = self.test_create_table()
        html = table.table_to_html(table_object, classes=['high-class'])
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
                  <td>0.111</td>
                </tr>
                <tr>
                  <td>Mali</td>
                  <td>Bamako</td>
                  <td>CFA franc</td>
                  <td>0.222</td>
                </tr>
                <tr>
                  <td>Mauritania</td>
                  <td>Nouakchott</td>
                  <td>Ouguiya</td>
                  <td>0.333</td>
                </tr>
              </tbody>
            </table>
        """)
        self.assertEqual(expected, html)

    def test_table_to_text(self):
        table_object = self.test_create_table()
        text = table.table_to_text(table_object,
                                   formats={'amount': '>'})
        expected = textwrap.dedent("""\
            ---------- ---------- --------- ------
            Country    Capital    Currency  Amount
            ---------- ---------- --------- ------
            Malawi     Lilongwe   Kwacha     0.111
            Mali       Bamako     CFA franc  0.222
            Mauritania Nouakchott Ouguiya    0.333
            ---------- ---------- --------- ------
        """)
        self.assertEqual(expected, text)

    def test_table_to_csv(self):
        table_object = self.test_create_table()
        text = table.table_to_csv(table_object, lineterminator='\n')
        expected = textwrap.dedent("""\
            Country,Capital,Currency,Amount
            Malawi,Lilongwe,Kwacha,0.111
            Mali,Bamako,CFA franc,0.222
            Mauritania,Nouakchott,Ouguiya,0.333
        """)
        self.assertEqual(expected, text)

    def test_compute_table_widths(self):
        widths = table.compute_table_widths([
            ['a', 'bb', 'ccc'],
            ['aa', 'b', 'c'],
            ['aaaa', 'bb', 'ccccc'],
            ])
        self.assertEqual([4, 2, 5], widths)

        # With first row longer.
        with self.assertRaises(IndexError):
            widths = table.compute_table_widths([
                ['a', 'bb', 'ccc'],
                ['aaaa', 'bb'],
            ])

        # With first row shorter.
        with self.assertRaises(IndexError):
            widths = table.compute_table_widths([
                ['a', 'bb', 'ccc'],
                ['aaaa', 'bb', 'c', 'd'],
            ])

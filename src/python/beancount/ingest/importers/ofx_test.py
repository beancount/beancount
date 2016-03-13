__author__ = "Martin Blais <blais@furius.ca>"

import datetime
import unittest
import re

import bs4

from beancount.ingest.importers import ofx


class TestOFXImporter(unittest.TestCase):

    def test_parse_ofx_time(self):
        dtime = datetime.datetime(2014, 1, 12, 5, 0, 0)
        self.assertEqual(dtime, ofx.parse_ofx_time('20140112050000.000[-7:MST]'))
        self.assertEqual(dtime, ofx.parse_ofx_time('20140112050000'))
        self.assertEqual(dtime.replace(hour=0), ofx.parse_ofx_time('20140112'))

    def test_find_acctids(self):
        contents = clean_xml("""
          <OFX>
            <CREDITCARDMSGSRSV1>
              <CCSTMTTRNRS>
                <TRNUID>0
                <CCSTMTRS>
                  <CURDEF>USD
                  <CCACCTFROM>
                    <ACCTID>379700001111222
                    <DOWNLOAD.FLAG>false
        """)
        self.assertEqual(['379700001111222'],
                         list(ofx.find_acctids(contents)))

    def test_find_date(self):
        contents = clean_xml("""
          <OFX>
            <CREDITCARDMSGSRSV1>
              <CCSTMTTRNRS>
                <TRNUID>
                  0
                  <STATUS>
                    <CODE>
                      0
                      <SEVERITY>
                        INFO
                      </SEVERITY>
                    </CODE>
                  </STATUS>
                  <CCSTMTRS>
                    <CURDEF>
                      USD
                      <LEDGERBAL>
                        <BALAMT>
                          -2356.38
                          <DTASOF>
                            20140112050000.000[-7:MST]
                          </DTASOF>
                        </BALAMT>
                      </LEDGERBAL>
                    </CURDEF>
                  </CCSTMTRS>
                </TRNUID>
              </CCSTMTTRNRS>
            </CREDITCARDMSGSRSV1>
          </OFX>
        """)
        date = ofx.find_date(contents)
        self.assertEqual(datetime.date(2014, 1, 12), date)

    def test_find_currency(self):
        contents = clean_xml("""
          <OFX>
            <CREDITCARDMSGSRSV1>
              <CCSTMTTRNRS>
                <TRNUID>
                  0
                  <CCSTMTRS>
                    <CURDEF>
                      USD
                    </CURDEF>
                  </CCSTMTRS>
                  <STMTRS>
                    <CURDEF>
                      CAD
                    </CURDEF>
                  </STMTRS>
                </TRNUID>
              </CCSTMTTRNRS>
            </CREDITCARDMSGSRSV1>
          </OFX>
        """)
        soup = bs4.BeautifulSoup(contents, 'lxml')
        self.assertEqual("USD", ofx.find_currency(soup))

    def test_find_statement_transactions(self):
        pass # FIXME: TODO



















def clean_xml(string):
    "Compress some formatted XML as it might appear in a real file."
    return re.sub(r"[ \t\n]", "", string)

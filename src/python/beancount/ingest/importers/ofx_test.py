__author__ = "Martin Blais <blais@furius.ca>"

import datetime
import unittest
import re
from xml.sax import saxutils

import bs4

from beancount.core.number import D
from beancount.ingest.importers import ofx
from beancount.parser import parser
from beancount.parser import printer
from beancount.parser import cmptest


def clean_xml(string):
    """Compress some formatted XML as it might appear in a real file."""
    return re.sub(r"(^[ \t\n]+|[ \t\n]+$)", "", string,
                  flags=re.MULTILINE).replace('\n', '')


class TestOFXImporter(cmptest.TestCase):

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
        contents = clean_xml("""
          <OFX>
           <SIGNONMSGSRSV1>
           </SIGNONMSGSRSV1>
           <CREDITCARDMSGSRSV1>
            <CCSTMTTRNRS>
             <TRNUID>0
              <STATUS>
               <CODE>0
                <SEVERITY>INFO
                </SEVERITY>
               </CODE>
              </STATUS>

              <CCSTMTRS>
               <CURDEF>USD
                <CCACCTFROM>
                 <ACCTID>379700001111222
                 </ACCTID>
                </CCACCTFROM>
                <BANKTRANLIST>
                 <DTSTART>20140112050000.000[-7:MST]
                  <DTEND>20140112050000.000[-7:MST]
                   <STMTTRN>
                    <TRNTYPE>DEBIT
                     <DTPOSTED>20131121000000.000[-7:MST]
                      <TRNAMT>-29
                       <FITID>320133250213757584
                        <REFNUM>320133250213757584
                         <NAME>JEFFREY'S 0252      NEW YORK
                          <MEMO>0000000681  646-429-8383
                          </MEMO>
                         </NAME>
                        </REFNUM>
                       </FITID>
                      </TRNAMT>
                     </DTPOSTED>
                    </TRNTYPE>
                   </STMTTRN>
                   <STMTTRN>
                    <TRNTYPE>DEBIT
                     <DTPOSTED>20131122000000.000[-7:MST]
                      <TRNAMT>-13.93
                       <FITID>320133260227320537
                        <REFNUM>320133260227320537
                         <NAME>WHOLEFDS HOU 10236 02124201320
                          <MEMO>042102720272124201320
                          </MEMO>
                         </NAME>
                        </REFNUM>
                       </FITID>
                      </TRNAMT>
                     </DTPOSTED>
                    </TRNTYPE>
                   </STMTTRN>
                  </DTEND>
                 </DTSTART>
                </BANKTRANLIST>
                <LEDGERBAL>
                 <BALAMT>-2356.38
                  <DTASOF>20140112050000.000[-7:MST]
                  </DTASOF>
                 </BALAMT>
                </LEDGERBAL>
               </CURDEF>
              </CCSTMTRS>
             </TRNUID>
            </CCSTMTTRNRS>

            <STMTTRNRS>
             <TRNUID>0
              <STATUS>
               <CODE>0
                <SEVERITY>INFO
                </SEVERITY>
               </CODE>
              </STATUS>
              <STMTRS>
               <CURDEF>CAD
                <CCACCTFROM>
                 <ACCTID>456700001111222
                 </ACCTID>
                </CCACCTFROM>
                <BANKTRANLIST>
                 <DTSTART>20131112000000.000[-7:MST]
                  <DTEND>20131231000000.000[-7:MST]
                   <STMTTRN>
                    <TRNTYPE>DEBIT
                     <DTPOSTED>20131112000000.000[-7:MST]
                      <TRNAMT>-21.51
                       <FITID>320133160086762615
                        <REFNUM>320133160086762615
                         <NAME>LOBSTER JOINT 542929NEW YORK
                          <MEMO>000224167   6468961200
                          </MEMO>
                         </NAME>
                        </REFNUM>
                       </FITID>
                      </TRNAMT>
                     </DTPOSTED>
                    </TRNTYPE>
                   </STMTTRN>
                </BANKTRANLIST>
                <LEDGERBAL>
                 <BALAMT>-2356.38
                  <DTASOF>20140112050000.000[-7:MST]
                  </DTASOF>
                 </BALAMT>
                </LEDGERBAL>
               </CURDEF>
              </STMTRS>
             </TRNUID>
            </STMTTRNRS>

           </CREDITCARDMSGSRSV1>
          </OFX>
        """)
        soup = bs4.BeautifulSoup(contents, 'lxml')
        txns = list(ofx.find_statement_transactions(soup))

        self.assertEqual(2, len(txns))

        self.assertEqual('379700001111222', txns[0][0])
        self.assertIsInstance(txns[0][1][0], bs4.element.Tag)
        self.assertEqual(2, len(txns[0][1]))

        self.assertIsInstance(txns[1][1][0], bs4.element.Tag)
        self.assertEqual('456700001111222', txns[1][0])
        self.assertEqual(1, len(txns[1][1]))

    def test_find_child(self):
        contents = clean_xml("""
          <STMTTRN>
           <TRNTYPE>DEBIT
            <DTPOSTED>20131122000000.000[-7:MST]
             <TRNAMT>-13.93
              <FITID>320133260227320537
               <REFNUM>320133260227320537
                <NAME>WHOLE &amp; FDS HOU 10236 02124201320
                 <MEMO>042102720272124201320
                 </MEMO>
                </NAME>
               </REFNUM>
              </FITID>
             </TRNAMT>
            </DTPOSTED>
           </TRNTYPE>
          </STMTTRN>
        """)
        node = bs4.BeautifulSoup(contents, 'lxml')

        self.assertEqual('20131122000000.000[-7:MST]',
                         ofx.find_child(node, 'dtposted'))
        self.assertEqual('-13.93',
                         ofx.find_child(node, 'trnamt'))
        self.assertEqual('320133260227320537',
                         ofx.find_child(node, 'fitid'))
        self.assertEqual('320133260227320537',
                         ofx.find_child(node, 'refnum'))
        self.assertEqual('WHOLE & FDS HOU 10236 02124201320',
                         ofx.find_child(node, 'name'))
        self.assertEqual('042102720272124201320',
                         ofx.find_child(node, 'memo'))

        # Test conversions.
        self.assertEqual(datetime.datetime(2013, 11, 22, 0, 0, 0),
                         ofx.find_child(node, 'dtposted', ofx.parse_ofx_time))
        self.assertEqual(D('-13.93'),
                         ofx.find_child(node, 'trnamt', D))

    def test_find_statement_transactions(self):
        contents = clean_xml("""
          <STMTTRN>
           <TRNTYPE>DEBIT
            <DTPOSTED>20131122000000.000[-7:MST]
             <TRNAMT>-13.93
              <FITID>320133260227320537
               <REFNUM>320133260227320537
                <NAME>WHOLEFDS HOU 10236 02124201320
                 <MEMO>042102720272124201320
                 </MEMO>
                </NAME>
               </REFNUM>
              </FITID>
             </TRNAMT>
            </DTPOSTED>
           </TRNTYPE>
          </STMTTRN>
        """)
        node = bs4.BeautifulSoup(contents, 'lxml')
        entry = ofx.build_transaction(node, '&', 'Liabilities:CreditCard', 'EUR')
        self.assertEqualEntries("""
          2013-11-22 & "WHOLEFDS HOU 10236 02124201320 / 042102720272124201320"
            Liabilities:CreditCard  -13.93 EUR
        """, [entry])

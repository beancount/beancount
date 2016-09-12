__author__ = "Martin Blais <blais@furius.ca>"

from os import path
import re
import os
import unittest
import argparse
import runpy

from beancount.utils import test_utils
from beancount.ingest import importer
from beancount.ingest import cache


def create_arguments_parser(description):
    """Create an arguments parser for common options.

    Returns:
      A partially initialized argparse.ArgumentParser instance.
      You may add on new arguments to this.
    """
    parser = argparse.ArgumentParser(description=description)

    parser.add_argument('config', action='store', metavar='CONFIG_FILENAME',
                        help=('Importer configuration file. '
                              'This is a Python file with a data structure that '
                              'is specific to your accounts'))

    parser.add_argument('files_or_directories', nargs='+', metavar='DOWNLOADS',
                        default=[],
                        help='Filenames or directories to search for files to import')

    return parser


def parse_arguments(parser, argv=None):
    """Parse the arguments, validate them and return a file iterator.

    Args:
      parser: An initialized argparse.ArgumentParser instance.
      argv: An optional list of arguments to process (used only for testing).
    Returns:
      A tuple of
        An argparse.Namespace instance containing the parsed command-line args.
        A list of importers.
        A list of files or directories to process.
    """
    args = parser.parse_args(args=argv)

    # Check the existence of the config.
    if not path.exists(args.config) or path.isdir(args.config):
        parser.error("File does not exist: '{}'".format(args.config))

    # Import the configuration.
    mod = runpy.run_path(args.config)
    config = mod['CONFIG']

    # Check the existence of all specified files.
    for filename in args.files_or_directories:
        if not path.exists(filename):
            parser.error("File does not exist: '{}'".format(filename))

    return args, config, list(map(path.abspath, args.files_or_directories))


class _TestFileImporter(importer.ImporterProtocol):

    def __init__(self, name, account, regexp_mime, regexp_contents):
        self._name = name
        self.account = account
        self.regexp_mime = regexp_mime
        self.regexp_contents = regexp_contents

    def name(self):
        return self._name

    def identify(self, file):
        mimetype = file.convert(cache.mimetype)
        if re.match(self.regexp_mime, mimetype):
            return True
        if re.search(self.regexp_contents, file.contents()):
            return True
        return False

    def file_account(self, _):
        return self.account


IMPORT_FILE = """\
#!/usr/bin/env python3
from beancount.ingest import scripts_utils

CONFIG = [

    scripts_utils._TestFileImporter(
        'mybank-checking-ofx', 'Assets:Checking',
        'application/x-ofx', '<FID>3011'),
    scripts_utils._TestFileImporter(
        'mybank-credit-csv', 'Liabilities:CreditCard',
        'text/csv', '.*DATE,TRANSACTION ID,DESCRIPTION,QUANTITY,SYMBOL'),

]
"""

OFX_FILE = """\
OFXHEADER:100
DATA:OFXSGML
VERSION:102
SECURITY:NONE
ENCODING:USASCII
CHARSET:1252
COMPRESSION:NONE
OLDFILEUID:NONE
NEWFILEUID:NONE

<OFX><SIGNONMSGSRSV1><SONRS><STATUS><CODE>0<SEVERITY>INFO<MESSAGE>Login successful</STATUS><DTSERVER>20131217204544.559[-7:MST]<LANGUAGE>ENG<FI><ORG>OFCT<FID>3011</FI><ORIGIN.ID>FMPWeb<START.TIME>20131217204544</SONRS></SIGNONMSGSRSV1><CREDITCARDMSGSRSV1><CCSTMTTRNRS><TRNUID>0<STATUS><CODE>0<SEVERITY>INFO</STATUS><CCSTMTRS><CURDEF>USD<CCACCTFROM><ACCTID>092243467384967<DOWNLOAD.FLAG>false<DOWNLOAD.TYPE>downloadSince<AMEX.BASICACCT>090341355486768<DAYS.SINCE>true<AMEX.ROLE>B<AMEX.UNIVID>iHJPMCPVMUZESUMTMIASKPSHBZOJZQMZ</CCACCTFROM><BANKTRANLIST><DTSTART>20131213050000.000[-7:MST]<DTEND>20131217050000.000[-7:MST]<STMTTRN><TRNTYPE>DEBIT<DTPOSTED>20131206000000.000[-7:MST]<DTUSER>20131206000000.000[-7:MST]<TRNAMT>-75<FITID>132124581254980455<REFNUM>140941621247980353<NAME>Cvzndybfhlgsy Kbptkt010-743-2492<MEMO>87278814438304-062-9392</STMTTRN><STMTTRN><TRNTYPE>DEBIT<DTPOSTED>20131208000000.000[-7:MST]<DTUSER>20131207000000.000[-7:MST]<TRNAMT>-29.5<FITID>139251640671720832<REFNUM>411944529384600439<NAME>YJTEJSYC JXJ 38137 80223112202<MEMO>841814901332133213240</STMTTRN><STMTTRN><TRNTYPE>DEBIT<DTPOSTED>20131208000000.000[-7:MST]<DTUSER>20131208000000.000[-7:MST]<TRNAMT>-96.73<FITID>518223640481029842<REFNUM>349922421383839452<NAME>TEMSRB TQBHHWZO CZYKCGDX.LAR/CD<MEMO>BVR5D49Q7S3 IWOUXSFCCIZ</STMTTRN><STMTTRN><TRNTYPE>DEBIT<DTPOSTED>20131209000000.000[-7:MST]<DTUSER>20131208000000.000[-7:MST]<TRNAMT>-45.49<FITID>410313240598642566<REFNUM>201153532386740368<NAME>JWNNJ VPVHHV -  HWKZIGH QXWR   <MEMO>35905       UJGZDQD IUTFL</STMTTRN><STMTTRN><TRNTYPE>DEBIT<DTPOSTED>20131209000000.000[-7:MST]<DTUSER>20131208000000.000[-7:MST]<TRNAMT>-01.7<FITID>118954331459601590<REFNUM>112944250496740196<NAME>ZPIKRWGV EBQVUE 4521XJT AYDM   <MEMO>227092130   2924489277</STMTTRN><STMTTRN><TRNTYPE>DEBIT<DTPOSTED>20131209000000.000[-7:MST]<DTUSER>20131208000000.000[-7:MST]<TRNAMT>-30.9<FITID>118335238578609388<REFNUM>542324610398801568<NAME>SBMUZYXG XRB 98038 03324302420<MEMO>853700608200014392232</STMTTRN><STMTTRN><TRNTYPE>DEBIT<DTPOSTED>20131209000000.000[-7:MST]<DTUSER>20131206000000.000[-7:MST]<TRNAMT>-39.72<FITID>141044448255701269<REFNUM>230245232285603469<NAME>SITEH NIHOX HTYZBAWP392-139-734<MEMO>84165624260 423-151-8227</STMTTRN><STMTTRN><TRNTYPE>DEBIT<DTPOSTED>20131209000000.000[-7:MST]<DTUSER>20131208000000.000[-7:MST]<TRNAMT>-22.09<FITID>111921351569432591<REFNUM>101153620388713392<NAME>HGJQJEOB PCQ 08418 1KVO VVDJ   <MEMO>84408170244 1144012409</STMTTRN><STMTTRN><TRNTYPE>DEBIT<DTPOSTED>20131211000000.000[-7:MST]<DTUSER>20131210000000.000[-7:MST]<TRNAMT>-22.14<FITID>548935642111458816<REFNUM>328141439181292814<NAME>LILFN DVRIFI -  LJBPFDT HYSF   <MEMO>32793       IBTSNAF UDOMK</STMTTRN><STMTTRN><TRNTYPE>DEBIT<DTPOSTED>20131211000000.000[-7:MST]<DTUSER>20131210000000.000[-7:MST]<TRNAMT>-12.67<FITID>330054241010450007<REFNUM>342912468199362629<NAME>PEHKXNPZ PNW 91458 21015119128<MEMO>823891919293222482430</STMTTRN><STMTTRN><TRNTYPE>DEBIT<DTPOSTED>20131214000000.000[-7:MST]<DTUSER>20131212000000.000[-7:MST]<TRNAMT>-31<FITID>402252668937162476<REFNUM>448222302958184678<NAME>YHVWV NNPYW HRQZDUOJ201-251-533<MEMO>65093823538 334-432-6338</STMTTRN><STMTTRN><TRNTYPE>DEBIT<DTPOSTED>20131214000000.000[-7:MST]<DTUSER>20131213000000.000[-7:MST]<TRNAMT>-64.25<FITID>542124402167304547<REFNUM>222333308839462735<NAME>MRKW'G #814 HYLDQF OVNN PSCS   <MEMO>89318083925 WOQO'G</STMTTRN><STMTTRN><TRNTYPE>DEBIT<DTPOSTED>20131216000000.000[-7:MST]<DTUSER>20131215000000.000[-7:MST]<TRNAMT>-42.01<FITID>210943512087240724<REFNUM>501152389299358014<NAME>ICPBVFY #2321 929883SXU QRKN   <MEMO>12054513980 3062749128</STMTTRN><STMTTRN><TRNTYPE>DEBIT<DTPOSTED>20131216000000.000[-7:MST]<DTUSER>20131215000000.000[-7:MST]<TRNAMT>-5<FITID>310215309277250199<REFNUM>448945618978219897<NAME>LON BXBZA 409 QEPNUAMCO WDOD   <MEMO>31345520940 4169979285</STMTTRN><STMTTRN><TRNTYPE>DEBIT<DTPOSTED>20131216000000.000[-7:MST]<DTUSER>20131215000000.000[-7:MST]<TRNAMT>-0.69<FITID>430314310166238818<REFNUM>109924680968112897<NAME>CFXUF LGTXVL -  VXCJCNI EBUH   <MEMO>308496      NSIWFWL RHSFP</STMTTRN><STMTTRN><TRNTYPE>DEBIT<DTPOSTED>20131216000000.000[-7:MST]<DTUSER>20131214000000.000[-7:MST]<TRNAMT>-34.43<FITID>330252321978428019<REFNUM>511914698876210997<NAME>DGCV GXD MJKRYD     JLC IYLM   <MEMO>67935504527 134-064-6852</STMTTRN><STMTTRN><TRNTYPE>DEBIT<DTPOSTED>20131215000000.000[-7:MST]<DTUSER>20131214000000.000[-7:MST]<TRNAMT>-24<FITID>149941491182603315<REFNUM>429354298891572407<NAME>UPTMFSAD DSD 12354 14003323410<MEMO>963402919381906089541</STMTTRN></BANKTRANLIST><LEDGERBAL><BALAMT>-3609.07<DTASOF>20131217050000.000[-7:MST]</LEDGERBAL><CYCLECUT.INDICATOR>false<PURGE.INDICATOR>false<INTL.INDICATOR>false</CCSTMTRS></CCSTMTTRNRS></CREDITCARDMSGSRSV1></OFX>
"""

CSV_FILE = """\
DATE,TRANSACTION ID,DESCRIPTION,QUANTITY,SYMBOL,PRICE,COMMISSION,AMOUNT,NET CASH BALANCE,REG FEE,SHORT-TERM RDM FEE,FUND REDEMPTION FEE, DEFERRED SALES CHARGE
07/02/2013,10223506553,ORDINARY DIVIDEND (HDV),,HDV,,,31.04,31.04,,,,
07/02/2013,10224851005,MONEY MARKET PURCHASE,,,,,-31.04,0.00,,,,
07/02/2013,10224851017,MONEY MARKET PURCHASE (MMDA10),31.04,MMDA10,,,0.00,0.00,,,,
09/30/2013,10561187188,ORDINARY DIVIDEND (HDV),,HDV,,,31.19,31.19,,,,
09/30/2013,10563719172,MONEY MARKET PURCHASE,,,,,-31.19,0.00,,,,
09/30/2013,10563719198,MONEY MARKET PURCHASE (MMDA10),31.19,MMDA10,,,0.00,0.00,,,,
***END OF FILE***
"""

TXT_FILE = """\
Some random text file.
"""

class TestScriptsBase(test_utils.TestTempdirMixin, unittest.TestCase):

    FILES = {
        'test.import': IMPORT_FILE,
        'Downloads/ofxdownload.ofx': OFX_FILE,
        'Downloads/Subdir/bank.csv': CSV_FILE,
        'Downloads/Subdir/readme.txt': TXT_FILE,
        }

    def setUp(self):
        super().setUp()
        for filename, contents in self.FILES.items():
            absname = path.join(self.tempdir, filename)
            os.makedirs(path.dirname(absname), exist_ok=True)
            with open(absname, 'w') as file:
                file.write(contents)

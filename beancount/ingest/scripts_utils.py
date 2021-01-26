"""Common front-end to all ingestion tools.
"""
__copyright__ = "Copyright (C) 2016,2018  Martin Blais"
__license__ = "GNU GPLv2"

from os import path
import functools
import os
import re
import stat
import sys
import unittest
import warnings

from beancount.ingest import importer
from beancount.ingest import cache
from beancount.utils import test_utils
from beancount.parser import version
from beancount.ingest import identify
from beancount.ingest import extract
from beancount.ingest import file


DESCRIPTION = ("Identify, extract or file away data downloaded from "
               "financial institutions.")


def ingest(importers_list, detect_duplicates_func=None, hooks=None):
    """Driver function that calls all the ingestion tools.

    Put a call to this function at the end of your importer configuration to
    make your import script; this should be its main function, like this:

      from beancount.ingest.scripts_utils import ingest
      my_importers = [ ... ]
      ingest(my_importers)

    This more explicit way of invoking the ingestion is now the preferred way to
    invoke the various tools, and replaces calling the bean-identify,
    bean-extract, bean-file tools with a --config argument. When you call the
    import script itself (as as program) it will parse the arguments, expecting
    a subcommand ('identify', 'extract' or 'file') and corresponding
    subcommand-specific arguments.

    Here you can override some importer values, such as installing a custom
    duplicate finding hook, and eventually more. Note that this newer invocation
    method is optional and if it is not present, a call to ingest() is generated
    implicitly, and it functions as it used to. Future configurable
    customization of the ingestion process will be implemented by inserting new
    arguments to this function, this is the motivation behind doing this.

    Note that invocation by the three bean-* ingestion tools is still supported,
    and calling ingest() explicitly from your import configuration file will not
    break these tools either, if you invoke them on it; the values you provide
    to this function will be used by those tools.

    Args:
      importers_list: A list of importer instances. This is used as a
        chain-of-responsibility, called on each file.
      detect_duplicates_func: (DEPRECATED) An optional function which accepts a
        list of lists of imported entries and a list of entries already existing
        in the user's ledger. See function find_duplicate_entries(), which is
        the default implementation for this. Use 'filter_funcs' instead.
      hooks: An optional list of hook functions to apply to the list of extract
        (filename, entries) pairs, in order. This replaces
        'detect_duplicates_func'.
    """
    if detect_duplicates_func is not None:
        warnings.warn("Argument 'detect_duplicates_func' is deprecated.")
        # Fold it in hooks.
        if hooks is None:
            hooks = []
        hooks.insert(0, detect_duplicates_func)
        del detect_duplicates_func

    parser = version.ArgumentParser(description=DESCRIPTION)

    # Use required on subparsers.
    # FIXME: Remove this when we require version 3.7 or above.
    kwargs = {}
    if sys.version_info >= (3, 7):
        kwargs['required'] = True
    subparsers = parser.add_subparsers(dest='command', **kwargs)

    parser.add_argument('--downloads', '-d', metavar='DIR-OR-FILE',
                        action='append', default=[],
                        help='Filenames or directories to search for files to import')

    for cmdname, module in [('identify', identify),
                            ('extract', extract),
                            ('file', file)]:
        parser_cmd = subparsers.add_parser(cmdname, help=module.DESCRIPTION)
        parser_cmd.set_defaults(command=module.run)
        module.add_arguments(parser_cmd)

    args = parser.parse_args()

    if not args.downloads:
        args.downloads.append(os.getcwd())

    # Implement required ourselves.
    # FIXME: Remove this when we require version 3.7 or above.
    if not (sys.version_info >= (3, 7)):
        if not hasattr(args, 'command'):
            parser.error("Subcommand is required.")

    abs_downloads = list(map(path.abspath, args.downloads))
    args.command(args, parser, importers_list, abs_downloads, hooks=hooks)
    return 0


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


INGEST_MAIN = """\
scripts_utils.ingest(CONFIG)
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

    # Example input files.
    FILES = {
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
            if filename.endswith('.py') or filename.endswith('.sh'):
                os.chmod(absname, stat.S_IRUSR|stat.S_IXUSR)

        importers = [
            _TestFileImporter(
                'mybank-checking-ofx', 'Assets:Checking',
                'application/x-ofx', '<FID>3011'),
            _TestFileImporter(
                'mybank-credit-csv', 'Liabilities:CreditCard',
                'text/csv', '.*DATE,TRANSACTION ID,DESCRIPTION,QUANTITY,SYMBOL'),
        ]
        self.ingest = functools.partial(ingest, importers)


class TestExamplesBase(test_utils.TestTempdirMixin, unittest.TestCase):

    def setUp(self):
        super().setUp()

        self.example_dir = path.join(
            test_utils.find_repository_root(__file__), 'examples', 'ingest')

        # Add examples dir to the Python path to import importers.
        sys.path.insert(0, path.join(self.example_dir, 'office'))

        # pylint: disable=import-outside-toplevel
        from beancount.ingest.importers import ofx
        from importers.utrade import utrade_csv
        from importers.acme import acme_pdf

        importers = [
            utrade_csv.Importer("USD",
                                "Assets:US:UTrade",
                                "Assets:US:UTrade:Cash",
                                "Income:US:UTrade:{}:Dividend",
                                "Income:US:UTrade:{}:Gains",
                                "Expenses:Financial:Fees",
                                "Assets:US:BofA:Checking"),

            ofx.Importer("379700001111222",
                         "Liabilities:US:CreditCard",
                         "bofa"),

            acme_pdf.Importer("Assets:US:AcmeBank"),
        ]
        self.ingest = functools.partial(ingest, importers)

    def tearDown(self):
        # Restore the Python path
        sys.path.pop(0)

"""OFX file format importer for investment accounts.
"""
import itertools
import re
import bs4

from beancount.imports import importer
from beancount.core.account import account_from_name
from beancount.core.amount import Decimal, Amount
from beancount.core import data
from beancount.core.data import Posting, Transaction, Check
from beancount.core.position import Lot, Position
from beancount.sources.ofx import souptodict, soup_get, parse_ofx_time
from beancount.core.account import accountify_dict
from beancount.core import flags


class Importer(importer.ImporterBase):

    REQUIRED_CONFIG = {
        'FILE'         : 'Account for filing',
        'asset_cash'   : 'Cash account that receives the contributions',
        'asset_pretax' : 'Root of positions from pre-tax contributions',
        'asset_match'  : 'Root of positions from matching contributions',
        'income_match' : 'Income from matching contributions',
        'dividend'     : 'Dividends',
        'fees'         : 'Fees',
    }

    def import_file(self, filename):
        """Extract transaction info from the given OFX file into transactions for the
        given account. This function returns a list of entries possibly partially
        filled entries.
        """
        config = self.get_accountified_config()

        # Prepare mappings to accounts from the config provided.
        source_subaccounts = {
            'PRETAX' : config['asset_pretax'],
            'MATCH'  : config['asset_match'],
        }

        def get_cash_account(trantype, source, _incometype):
            if trantype == 'BUYMF' and source == 'MATCH':
                return config['income_match']
            elif trantype == 'REINVEST':
                return config['dividend']
            elif trantype == 'TRANSFER':
                return config['fees']
            else:
                return config['asset_cash']

        new_entries = []

        # Parse the XML file.
        soup = bs4.BeautifulSoup(open(filename), 'lxml')

        # Get the description of securities used in this file.
        securities = get_securities(soup)
        if securities:
            securities_map = {security['uniqueid']: security
                              for security in securities}

        # For each statement.
        txn_counter = itertools.count()
        for stmtrs in soup.find_all(re.compile('.*stmtrs$')):
            # account_typee = st.find('accttype').text.strip()
            # bank_id = st.find('bankid').text.strip()

            # For each currnecy.
            for currency_node in stmtrs.find_all('curdef'):
                currency = currency_node.contents[0].strip()

                # Process all investment transaction lists.
                # Note: this was developed for Vanguard.
                for invtranlist in stmtrs.find_all(re.compile('invtranlist')):

                    for tran in invtranlist.find_all(re.compile('(buymf|sellmf|reinvest|buystock|sellstock|buyopt|sellopt|transfer)')):

                        date = parse_ofx_time(soup_get(tran, 'dttrade')).date()
                        # date = parse_ofx_time(trndict['dtsettle']).date()

                        uniqueid = soup_get(tran, 'uniqueid')
                        security = securities_map[uniqueid]['ticker']

                        units = soup_get(tran, 'units', Decimal)
                        unitprice = soup_get(tran, 'unitprice', Decimal)
                        total = soup_get(tran, 'total', Decimal)

                        fileloc = data.FileLocation(filename, next(txn_counter))
                        payee = None

                        trantype = tran.name.upper()
                        incometype = soup_get(tran, 'incometype')
                        source = soup_get(tran, 'inv401ksource')
                        memo = soup_get(tran, 'memo')
                        narration = ' - '.join(filter(None, (trantype, incometype, source, memo)))

                        entry = Transaction(fileloc, date, flags.FLAG_IMPORT, payee, narration, None, None, [])

                        # Create a posting for it.
                        tferaction = soup_get(tran, 'tferaction')
                        if tferaction == 'OUT':
                            units = -units

                        position = Position(Lot(security, Amount(unitprice, currency), None), units)

                        account = account_from_name('{}:{}'.format(
                            source_subaccounts[source].name, security))
                        entry.postings.append(Posting(entry, account, position, None, None))

                        if total is None:
                            total = units * unitprice
                            ##print('{:.9f} {:.9f} = {:.9f}'.format(units, unitprice, total))

                        position = Position(Lot(currency, None, None), -total)
                        account = get_cash_account(trantype, source, incometype)
                        entry.postings.append(Posting(entry, account, position, None, None))

                        new_entries.append(entry)

                    # Process all positions, convert them to Check directives.
                    # Note: this was developed for Vanguard.
                    for invposlist in stmtrs.find_all('invposlist'):
                        for invpos in invposlist.find_all('invpos'):
                            date = parse_ofx_time(soup_get(invpos, 'dtpriceasof')).date()

                            uniqueid = soup_get(invpos, 'uniqueid')
                            security = securities_map[uniqueid]['ticker']

                            units = soup_get(invpos, 'units', Decimal)
                            unitprice = soup_get(invpos, 'unitprice', Decimal)

                            fileloc = data.FileLocation(filename, next(txn_counter))
                            source = soup_get(invpos, 'inv401ksource')
                            if source is None:
                                continue

                            account = account_from_name('{}:{}'.format(
                                source_subaccounts[source].name, security))

                            amount = Amount(units, security)
                            new_entries.append(Check(fileloc, date, account, amount, None))

        new_entries.sort(key=lambda entry: entry.date)
        return new_entries


def get_securities(soup):
    """Extract the list of securities from the OFX file."""

    seclistmsgsrsv = soup.find('seclistmsgsrsv1')
    if not seclistmsgsrsv:
        return

    securities = []
    for secinfo in seclistmsgsrsv.find_all('secinfo'):
        # Merge the two nodes in a dictionary.
        secid = souptodict(secinfo.find('secid'))
        secname = souptodict(secinfo.find('secname'))
        secid.update(secname)
        securities.append(secid)

    return securities

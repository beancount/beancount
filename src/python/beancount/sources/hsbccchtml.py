"""HSBC Credit Card - Import from HTML.

When they migrated the hsbccreditcard.com website into the www.us.hsbc.com
platform, the idiots at HSBC decided to remove OFX download. Fuckwits. Why!?
So this importer grabs it from the HTML. Save page, from Chrome.

You really, really ought to get another credit card. Close your account, go open
a TD card or Capital One, HSBC sucks, and they're getting worse every year! I
wrote this importer while I was making the transition, hoping to move away from
their terrible servie.
"""
import re
import datetime
import collections
import bs4

from beancount.imports import importer
from beancount.core import data
from beancount.core.amount import to_decimal
from beancount.core.data import create_simple_posting
from beancount.core.data import Transaction
from beancount.core import flags


class Importer(importer.ImporterBase):

    REQUIRED_CONFIG = {
        'FILE'               : 'Account for filing',
        'asset'              : 'Credit card account',
        'cash_currency'      : 'Currency used for cash account',
    }


    def import_file(self, filename):
        """Import an HTML dump of HSBC's transaction list."""

        config = self.get_accountified_config()

        new_entries = []
        for index, hsbc_entry in enumerate(extract_transactions_xhtml(filename)):

            # Create a new entry from the Hsbc file entry.
            fileloc = data.FileLocation(filename, index)
            entry = Transaction(fileloc, hsbc_entry.trans_date, flags.FLAG_IMPORT,
                                None, hsbc_entry.description, None, None, [])
            create_simple_posting(entry, config['asset'],
                                  -hsbc_entry.amount, config['cash_currency'])

            new_entries.append(entry)

        return new_entries


HsbcEntry = collections.namedtuple('HsbcEntry',
                                   'trans_date post_date description amount')


def cleanup_html(html):
    "Those incompetent programmers at HSBC produce the worst HTML."
    html = re.sub('<!--.*?-->',          '', html, flags=re.DOTALL)
    html = re.sub('onclick=".*?"',       '', html, flags=re.DOTALL)
    html = re.sub('style=".*?"',         '', html, flags=re.DOTALL)
    html = re.sub('<script.*?</script>', '', html, flags=re.DOTALL)
    html = re.sub('<style.*?</style>',   '', html, flags=re.DOTALL)
    return html


def extract_transactions_xhtml(filename):
    """Read the transactions off the "Printer Friendly" XHTML file you can obtain
    from the website. Save the file.
    """
    contents = open(filename).read()
    contents = cleanup_html(contents)

    soup = bs4.BeautifulSoup(contents)
    table = soup.find('table', id='accountActivity:data')
    tbody = table.find('tbody', id='accountActivity:data:tbody_element')

    for tr in tbody.find_all('tr'):
        cells = []
        for td in tr.find_all('td'):
            cell = ''.join(node.contents[0].string
                              for node in td.find_all(re.compile('(span|a)')))
            cells.append(cell)

        trans_date_str, post_date_str, description, amount = cells
        yield HsbcEntry(datetime.datetime.strptime(trans_date_str, '%m/%d/%y').date(),
                        datetime.datetime.strptime(post_date_str, '%m/%d/%y').date(),
                        description,
                        to_decimal(amount.lstrip('$')))


# Retiring; this thing's a moving target, the idiots are changing it every week.
def extract_transactions_xhtml__old(filename):
    """Read the transactions off the "Printer Friendly" XHTML file you can obtain
    from the website. Save the file.
    """
    contents = open(filename).read()
    if re.search('menuBean', contents):
        raise ValueError("Attempting to parse the source HTML, not the Printer-Friendly HTML.")

    soup = bs4.BeautifulSoup(contents)
    table = soup.find('table', id='form:data')
    tbody = table.find('tbody', id='form:data:tbody_element')

    for tr in tbody.find_all('tr'):
        cells = []
        for td in tr.find_all('td'):
            cell = ''.join(node.contents[0].string
                              for node in td.find_all(re.compile('(span|a)')))
            cells.append(cell)

        trans_date_str, post_date_str, description, amount = cells
        yield HsbcEntry(datetime.datetime.strptime(trans_date_str, '%m/%d/%y').date(),
                        datetime.datetime.strptime(post_date_str, '%m/%d/%y').date(),
                        description,
                        to_decimal(amount.lstrip('$')))

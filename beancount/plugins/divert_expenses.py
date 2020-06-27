"""For tagged transactions, convert expenses to a single account.

This plugin allows you to select a tag and it automatically converts all the
Expenses postings to use a single account. For example, with this input:

    plugin "divert_expenses" "['kid', 'Expenses:Child']"

    2018-01-28 * "CVS" "Formula" #kid
      Liabilities:CreditCard      -10.27 USD
      Expenses:Food:Grocery        10.27 USD

It will output:

   2018-01-28 * "CVS" "Formula" #kid
      Liabilities:CreditCard      -10.27 USD
      Expenses:Child               10.27 USD

You can limit the diversion to one posting only, like this:

    2018-05-05 * "CVS/PHARMACY" "" #kai
      Liabilities:CreditCard        -66.38 USD
      Expenses:Pharmacy              21.00 USD  ;; Vitamins for Kai
      Expenses:Pharmacy              45.38 USD
        divert: FALSE

See unit test for details.

See this thread for context:
https://docs.google.com/drawings/d/18fTrrGlmz0jFbfcGGHTffbdRwbmST8r9_3O26Dd1Xww/edit?usp=sharing
"""
__copyright__ = "Copyright (C) 2018  Martin Blais"
__license__ = "GNU GPLv2"

from beancount.core.data import Transaction
from beancount.core import account_types
from beancount.parser import options


__plugins__ = ('divert_expenses',)


def divert_expenses(entries, options_map, config_str):
    """Divert expenses.

    Explicit price entries are simply maintained in the output list. Prices from
    postings with costs or with prices from Transaction entries are synthesized
    as new Price entries in the list of entries output.

    Args:
      entries: A list of directives. We're interested only in the Transaction instances.
      options_map: A parser options dict.
      config_str: A configuration string, which is intended to be a list of two strings,
        a tag, and an account to replace expenses with.
    Returns:
      A modified list of entries.
    """
    # pylint: disable=eval-used
    config_obj = eval(config_str, {}, {})
    if not isinstance(config_obj, dict):
        raise RuntimeError("Invalid plugin configuration: should be a single dict.")
    tag = config_obj['tag']
    replacement_account = config_obj['account']

    acctypes = options.get_account_types(options_map)

    new_entries = []
    errors = []
    for entry in entries:
        if isinstance(entry, Transaction) and tag in entry.tags:
            entry = replace_diverted_accounts(entry, replacement_account, acctypes)
        new_entries.append(entry)

    return new_entries, errors


def replace_diverted_accounts(entry, replacement_account, acctypes):
    """Replace the Expenses accounts from the entry.

    Args:
      entry: A Transaction directive.
      replacement_account: A string, the account to use for replacement.
      acctypes: An AccountTypes instance.
    Returns:
      A possibly entry directive.
    """
    new_postings = []
    for posting in entry.postings:
        divert = posting.meta.get('divert', None) if posting.meta else None
        if (divert is True or (
                divert is None and
                account_types.is_account_type(acctypes.expenses, posting.account))):
            posting = posting._replace(account=replacement_account,
                                       meta={'diverted_account': posting.account})
        new_postings.append(posting)
    return entry._replace(postings=new_postings)

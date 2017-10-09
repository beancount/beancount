"""Automatically adding IRA contributions postings.

This plugin looks for increasing postings on specified accounts ('+' sign for
Assets and Expenses accounts, '-' sign for the others), or postings with a
particular flag on them and when it finds some, inserts a pair of postings on
that transaction of the corresponding amounts in a different currency. The
currency is intended to be an imaginary currency used to track the number of
dollars contributed to a retirement account over time.

For example, a possible configuration could be:

  plugin "beancount.plugins.ira_contribs" "{
      'currency': 'IRAUSD',
      'flag': 'M',
      'accounts': {

          'Income:US:Acme:Match401k': (
              'Assets:US:Federal:Match401k',
              'Expenses:Taxes:TY{year}:US:Federal:Match401k'),

          ('C', 'Assets:US:Fidelity:PreTax401k:Cash'): (
              'Assets:US:Federal:PreTax401k',
              'Expenses:Taxes:TY{year}:US:Federal:PreTax401k'),
       }
  }"

Note: In this example, the configuration that triggers on the
"Income:US:Acme:Match401k" account does not require a flag for those accounts;
the configuration for the "Assets:US:Fidelity:PreTax401k:Cash" account requires
postings to have a "C" flag to trigger an insertion.

Given a transaction like the following, which would be typical for a salary
entry where the employer is automatically diverting some of the pre-tax money to
a retirement account (in this example, at Fidelity):

  2013-02-15 * "ACME INC       PAYROLL"
    Income:US:Acme:Salary                   ...
    ...
    Assets:US:BofA:Checking                 ...
    Assets:US:Fidelity:PreTax401k:Cash      620.50 USD
    ...

A posting with account 'Assets:US:Fidelity:PreTax401k:Cash', which is configured
to match, would be found. The configuration above instructs the plugin to
automatically insert new postings like this:

  2013-02-15 * "ACME INC       PAYROLL"
    ...
    Assets:US:Fidelity:PreTax401k:Cash              620.50 USD
    M Assets:US:Federal:PreTax401k                 -620.50 IRAUSD
    M Expenses:Taxes:TY2013:US:Federal:PreTax401k   620.50 IRAUSD
    ...

Notice that the "{year}" string in the configuration's account names is
automatically replaced by the current year in the account name. This is useful
if you maintain separate tax accounts per yera.

Furthermore, as in the configuration example above, you may have multiple
matching entries to trigger multiple insertions. For example, the employer may
also match the employee's retirement contribution by depositing some money in
the retirement account:

  2013-02-15 * "BUYMF - MATCH" "Employer match, invested in SaveEasy 2030 fund"
    Assets:US:Fidelity:Match401k:SE2030   34.793 SE2030 {17.834 USD}
    Income:US:Acme:Match401k             -620.50 USD

In this example the funds get reported as invested immediately (an intermediate
deposit into a cash account does not take place). The plugin configuration would
match against the 'Income:US:Acme:Match401k' account and since it increases its
value (the normal balance of an Income account is negative), postings would be
inserted like this:

  2013-02-15 * "BUYMF - MATCH" "Employer match, invested in SaveEasy 2030 fund"
    Assets:US:Fidelity:Match401k:SE2030              34.793 SE2030 {17.834 USD}
    Income:US:Acme:Match401k                        -620.50 USD
    M Assets:US:Federal:Match401k                   -620.50 IRAUSD
    M Expenses:Taxes:TY2013:US:Federal:Match401k     620.50 IRAUSD

Note that the special dict keys 'currency' and 'flag' are used to
specify which currency to use for the inserted postings, and if set, which flag
to mark these postings with.

"""

__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

from beancount.core.number import MISSING
from beancount.core import data
from beancount.core import account_types
from beancount.core import amount
from beancount.parser import printer


__plugins__ = ('add_ira_contribs',)


DEBUG = 0


def add_ira_contribs(entries, options_map, config_str):
    """Add legs for 401k employer match contributions.

    See module docstring for an example configuration.

    Args:
      entries: a list of entry instances
      options_map: a dict of options parsed from the file
      config_str: A configuration string, which is intended to be a Python dict
        mapping match-accounts to a pair of (negative-account, position-account)
        account names.
    Returns:
      A tuple of entries and errors.
    """
    # Parse and extract configuration values.
    # FIXME: Use ast.literal_eval() here; you need to convert this code and the getters.
    # FIXME: Also, don't raise a RuntimeError, return an error object; review
    # this for all the plugins.
    # FIXME: This too is temporary.
    # pylint: disable=eval-used
    config_obj = eval(config_str, {}, {})
    if not isinstance(config_obj, dict):
        raise RuntimeError("Invalid plugin configuration: should be a single dict.")

    # Currency of the inserted postings.
    currency = config_obj.pop('currency', 'UNKNOWN')

    # Flag to attach to the inserted postings.
    insert_flag = config_obj.pop('flag', None)

    # A dict of account names that trigger the insertion of postings to pairs of
    # inserted accounts when triggered.
    accounts = config_obj.pop('accounts', {})

    # Convert the key in the accounts configuration for matching.
    account_transforms = {}
    for key, config in accounts.items():
        if isinstance(key, str):
            flag = None
            account = key
        else:
            assert isinstance(key, tuple)
            flag, account = key
        account_transforms[account] = (flag, config)

    new_entries = []
    for entry in entries:
        if isinstance(entry, data.Transaction):
            orig_entry = entry
            for posting in entry.postings:
                if (posting.units is not MISSING and
                    (posting.account in account_transforms) and
                    (account_types.get_account_sign(posting.account) *
                     posting.units.number > 0)):

                    # Get the new account legs to insert.
                    required_flag, (neg_account,
                                    pos_account) = account_transforms[posting.account]
                    assert posting.cost is None

                    # Check required flag if present.
                    if (required_flag is None or
                        (required_flag and required_flag == posting.flag)):
                        # Insert income/expense entries for 401k.
                        entry = add_postings(
                            entry,
                            amount.Amount(abs(posting.units.number), currency),
                            neg_account.format(year=entry.date.year),
                            pos_account.format(year=entry.date.year),
                            insert_flag)

            if DEBUG and orig_entry is not entry:
                printer.print_entry(orig_entry)
                printer.print_entry(entry)

        new_entries.append(entry)

    return new_entries, []


def add_postings(entry, amount_, neg_account, pos_account, flag):
    """Insert positive and negative postings of a position in an entry.

    Args:
      entry: A Transaction instance.
      amount_: An Amount instance to create the position, with positive number.
      neg_account: An account for the posting with the negative amount.
      pos_account: An account for the posting with the positive amount.
      flag: A string, that is to be set as flag for the new postings.
    Returns:
      A new, modified entry.
    """
    return entry._replace(postings=entry.postings + [
        data.Posting(neg_account, -amount_, None, None, flag, None),
        data.Posting(pos_account, amount_, None, None, flag, None),
        ])

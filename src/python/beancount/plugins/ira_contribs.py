"""Automatically adding IRA contributions postings.

This plugin looks for increasing postings on specified accounts ('+' sign for
Assets and Expenses accounts, '-' sign for the others) and when it finds some,
inserts a pair of postings on that transaction of the corresponding amounts in a
different currency. The currency is intended to be an imaginary currency used to
track the number of dollars contributed to a retirement account over time.

For example, a possible configuration could be:

  plugin "beancount.plugins.iracontribs" "{
      'currency': 'IRAUSD',
      'flag': 'M',
      'accounts': {
          'Income:US:Acme:Match401k': (
              'Assets:US:Federal:Match401k',
              'Expenses:Taxes:TY{year}:US:Federal:Match401k'),
          'Assets:US:Fidelity:PreTax401k:Cash': (
              'Assets:US:Federal:PreTax401k',
              'Expenses:Taxes:TY{year}:US:Federal:PreTax401k'),
       }
  }"

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

  2013-02-15 * "BUYMF - MATCH" | "Employer match, invested in SaveEasy 2030 fund"
    Assets:US:Fidelity:Match401k:SE2030   34.793 SE2030 {17.834 USD}
    Income:US:Acme:Match401k             -620.50 USD

In this example the funds get reported as invested immediately (an intermediate
deposit into a cash account does not take place). The plugin configuration would
match against the 'Income:US:Acme:Match401k' account and since it increases its
value (the normal balance of an Income account is negative), postings would be
inserted like this:

  2013-02-15 * "BUYMF - MATCH" | "Employer match, invested in SaveEasy 2030 fund"
    Assets:US:Fidelity:Match401k:SE2030              34.793 SE2030 {17.834 USD}
    Income:US:Acme:Match401k                        -620.50 USD
    M Assets:US:Federal:Match401k                   -620.50 IRAUSD
    M Expenses:Taxes:TY2013:US:Federal:Match401k     620.50 IRAUSD

Note that the special dict keys 'currency' and 'flag' are used to
specify which currency to use for the inserted postings, and if set, which flag
to mark these postings with.
"""

__author__ = 'Martin Blais <blais@furius.ca>'

from beancount.core import data
from beancount.core import account_types
from beancount.core import amount
from beancount.core import position
from beancount.parser import printer


__plugins__ = ('add_ira_contribs',)


DEBUG = 0


def add_ira_contribs(entries, options_map, config):
    """Add legs for 401k employer match contributions.

    See module docstring for an example configuration.

    Args:
      entries: a list of entry instances
      options_map: a dict of options parsed from the file
      config: A configuration string, which is intended to be a Python dict
        mapping match-accounts to a pair of (negative-account, position-account)
        account names.
    Returns:
      A tuple of entries and errors.
    """
    # Parse and extract configuration values.
    config_obj = eval(config, {}, {})
    if not isinstance(config_obj, dict):
        raise RuntimeError("Invalid plugin configuration: should be a single dict.")

    currency = config_obj.pop('currency', 'UNKNOWN')
    flag = config_obj.pop('flag', None)
    account_transforms = config_obj.pop('accounts', {})

    new_entries = []
    for entry in entries:
        if isinstance(entry, data.Transaction):
            orig_entry = entry
            for posting in entry.postings:
                if (posting.account in account_transforms and
                    posting.position and
                    (account_types.get_account_sign(posting.account) *
                     posting.position.number) > 0):

                    # Get the new account legs to insert.
                    neg_account, pos_account = account_transforms[posting.account]
                    assert posting.position.cost is None

                    # Insert income/expense entries for 401k.
                    entry = add_postings(
                        entry,
                        amount.Amount(abs(posting.position.number), currency),
                        neg_account.format(year=entry.date.year),
                        pos_account.format(year=entry.date.year),
                        flag)

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
    pos = position.Position.from_amounts(amount_)
    return entry._replace(postings=entry.postings + [
        data.Posting(neg_account, -pos, None, flag, None),
        data.Posting(pos_account, pos, None, flag, None),
        ])

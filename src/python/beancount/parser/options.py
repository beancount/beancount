"""
Declaration of options and their default values.
"""
from beancount.core import account_types
from beancount.core import account


DEFAULT_OPTIONS = {

    # The name of the top-level Beancoount input file parsed from which the
    # contents of the ledger have been extracted. This may be None, if no file
    # was used.
    "filename": None,

    # The title of this ledger / input file. This shows up at the top of every
    # page.
    "title": "Beancount",

    # Root names of every account. This can be used to customize your category
    # names, so that if you prefer "Revenue" over "Income" or "Capital" over
    # "Equity", you can set them here. The account names in your input files
    # must match, and the parser will validate these.
    "name_assets"      : account_types.DEFAULT_ACCOUNT_TYPES.assets,
    "name_liabilities" : account_types.DEFAULT_ACCOUNT_TYPES.liabilities,
    "name_equity"      : account_types.DEFAULT_ACCOUNT_TYPES.equity,
    "name_income"      : account_types.DEFAULT_ACCOUNT_TYPES.income,
    "name_expenses"    : account_types.DEFAULT_ACCOUNT_TYPES.expenses,

    # Leaf name of the equity account used for summarizing previous transactions
    # into opening balances.
    "account_previous_balances": "Opening-Balances",

    # Leaf name of the equity account used for transferring previous retained
    # earnings from income and expenses accrued before the beginning of the
    # exercise into the balance sheet.
    "account_previous_earnings": "Earnings:Previous",

    # Leaf name of the equity account used for inserting conversions that will
    # zero out remaining amounts due to transfers before the opening date. This
    # will essentially "fixup" the basic accounting equation due to the errors
    # that priced conversions introduce.
    "account_previous_conversions": "Conversions:Previous",

    # Leaf name of the equity account used for transferring current retained
    # earnings from income and expenses accrued during the current exercise into
    # the balance sheet. This is most often called "Net Income".
    "account_current_earnings": "Earnings:Current",

    # Leaf name of the equity account used for inserting conversions that will
    # zero out remaining amounts due to transfers during the exercise period.
    "account_current_conversions": "Conversions:Current",

    # The imaginary currency used to convert all units for conversions at a
    # degenerate rate of zero. This can be any currency name that isn't used in
    # the rest of the ledger. Choose something unique that makes sense in your
    # language.
    "conversion_currency": "NOTHING",

    # A list of directory roots, relative to the CWD, which should be searched
    # for document files. For the document files to be automatically found they
    # must have the following filename format: YYYY-MM-DD.(.*)
    "documents": [],

    # A list of currencies that we single out during reporting and create
    # dedicated columns for. This is used to indicate the main currencies that
    # you work with in real life. (Refrain from listing all the possible
    # currencies here, this is not what it is made for; just list the very
    # principal currencies you use daily only.)
    #
    # Because our system is agnostic to any unit definition that occurs in the
    # input file, we use this to display these values in table cells without
    # their associated unit strings. This allows you to import the numbers in a
    # spreadsheet (e.g, "101.00 USD" does not get parsed by a spreadsheet
    # import, but "101.00" does).
    "operating_currency": [],

    # A list of Python modules containing transformation functions to run the
    # entries through after parsing. The parser reads the entries as they are,
    # transforms them through a list of standard functions, such as balance
    # checks and inserting padding entries, and then hands the entries over to
    # those plugins to add more auto-generated goodies. The list is a list of
    # strings, each string should be the name of a Python module to import, and
    # within the module we expect a special '__plugins__' attribute that should
    # list the name of transform functions to run the entries through. Each
    # function accepts a pair of (entries, options_map) and should return a pair
    # of (new entries, error instances). Errors should not be printed out the
    # output, they will be converted to strins by the loader and displayed as
    # dictacted by the output medium.
    "plugin": [],
}


# A list of options that cannot be modified.
READ_ONLY_OPTIONS = {"filename"}


def get_account_types(options):
    """Extract the account type names from the parser's options.

    Args:
      options: a dict of ledger options.
    Returns:
      An instance of AccountTypes, that contains all the prefixes.
    """
    return account_types.AccountTypes(
        *[options[key]
          for key in ("name_assets",
                      "name_liabilities",
                      "name_equity",
                      "name_income",
                      "name_expenses")])


def get_previous_accounts(options):
    """Return Account objects for the opening, earnings, and conversion accounts.

    Args:
      options: a dict of ledger options.
    Returns:
      A tuple of 3 account objects, for booking previous earnings,
      previous balances, and previous conversions.
    """
    equity = options['name_equity']
    account_previous_earnings = account.join(equity,
                                             options['account_previous_earnings'])
    account_previous_balances = account.join(equity,
                                             options['account_previous_balances'])
    account_previous_conversions = account.join(equity,
                                                options['account_previous_conversions'])
    return (account_previous_earnings,
            account_previous_balances,
            account_previous_conversions)


def get_current_accounts(options):
    """Return Account objects for the opening, earnings, and conversion accounts.

    Args:
      options: a dict of ledger options.
    Returns:
      A tuple of 2 account objects, one for booking current earnings, and one
      for current conversions.
    """
    equity = options['name_equity']
    account_current_earnings = account.join(equity,
                                            options['account_current_earnings'])
    account_current_conversions = account.join(equity,
                                               options['account_current_conversions'])
    return (account_current_earnings,
            account_current_conversions)

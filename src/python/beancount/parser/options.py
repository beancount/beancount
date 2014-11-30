"""
Declaration of options and their default values.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import collections
import io
import textwrap

from beancount.core import account_types
from beancount.core import account
from beancount.core import display_context


# list of option groups, with their description, option names and default
# values.
OptGroup = collections.namedtuple('OptGroup',
                                  'description options')

OptDesc = collections.namedtuple('OptDesc',
                                 'name default_value example_value')

_TYPES = account_types.DEFAULT_ACCOUNT_TYPES


# Options that are not to be shown to the user.
PRIVATE_OPTION_GROUPS = [

    OptGroup("""
      The name of the top-level Beancount input file parsed from which the
      contents of the ledger have been extracted. This may be None, if no file
      was used.
    """, [OptDesc("filename", None, None)]),

    OptGroup("""
      An instance of DisplayContext, which is used to format numbers for output
      with precision inferred from that in the input file. This is created
      automatically by the parser.
    """, [OptDesc("display_context",
                  display_context.DisplayContext(), display_context.DisplayContext())]),
    ]


# Options that are visible to the user and that can be set.
PUBLIC_OPTION_GROUPS = [

    OptGroup("""
      The title of this ledger / input file. This shows up at the top of every
      page.
    """, [OptDesc("title", "Beancount", "Joe Smith's Personal Ledger")]),

    OptGroup("""
      Root names of every account. This can be used to customize your category
      names, so that if you prefer "Revenue" over "Income" or "Capital" over
      "Equity", you can set them here. The account names in your input files
      must match, and the parser will validate these. You should place these
      options at the beginning of your file, because they affect how the parser
      recognizes account names.
    """, [
        OptDesc("name_assets", _TYPES.assets, _TYPES.assets),
        OptDesc("name_liabilities", _TYPES.liabilities, _TYPES.liabilities),
        OptDesc("name_equity", _TYPES.equity, _TYPES.equity),
        OptDesc("name_income", _TYPES.income, _TYPES.income),
        OptDesc("name_expenses", _TYPES.expenses, _TYPES.expenses),
    ]),

    OptGroup("""
      Leaf name of the equity account used for summarizing previous transactions
      into opening balances.
    """, [OptDesc("account_previous_balances",
                  "Opening-Balances", "Opening-Balances")]),

    OptGroup("""
      Leaf name of the equity account used for transferring previous retained
      earnings from income and expenses accrued before the beginning of the
      exercise into the balance sheet.
    """, [OptDesc("account_previous_earnings",
                  "Earnings:Previous", "Earnings:Previous")]),

    OptGroup("""
      Leaf name of the equity account used for inserting conversions that will
      zero out remaining amounts due to transfers before the opening date. This
      will essentially "fixup" the basic accounting equation due to the errors
      that priced conversions introduce.
    """, [OptDesc("account_previous_conversions",
                  "Conversions:Previous", "Conversions:Previous")]),

    OptGroup("""
      Leaf name of the equity account used for transferring current retained
      earnings from income and expenses accrued during the current exercise into
      the balance sheet. This is most often called "Net Income".
    """, [OptDesc("account_current_earnings",
                  "Earnings:Current", "Earnings:Current")]),

    OptGroup("""
      Leaf name of the equity account used for inserting conversions that will
      zero out remaining amounts due to transfers during the exercise period.
    """, [OptDesc("account_current_conversions",
                  "Conversions:Current", "Conversions:Current")]),

    OptGroup("""
      The imaginary currency used to convert all units for conversions at a
      degenerate rate of zero. This can be any currency name that isn't used in
      the rest of the ledger. Choose something unique that makes sense in your
      language.
    """, [OptDesc("conversion_currency",
                  "NOTHING", "NOTHING")]),

    OptGroup("""
      The tolerance allowed for balance checks and padding directives. In the
      real world, rounding occurs in various places, and we need to allow a
      small (but very small) amount of tolerance in checking the balance of
      transactions and in requiring padding entries to be auto-inserted. This is
      the tolerance amount, which you can override.
    """, [OptDesc("tolerance", "0.015", "0.015")]),

    OptGroup("""
      A list of directory roots, relative to the CWD, which should be searched
      for document files. For the document files to be automatically found they
      must have the following filename format: YYYY-MM-DD.(.*)
    """, [OptDesc("documents", [], "/path/to/your/documents/archive")]),

    OptGroup("""
      A list of currencies that we single out during reporting and create
      dedicated columns for. This is used to indicate the main currencies that
      you work with in real life. (Refrain from listing all the possible
      currencies here, this is not what it is made for; just list the very
      principal currencies you use daily only.)

      Because our system is agnostic to any unit definition that occurs in the
      input file, we use this to display these values in table cells without
      their associated unit strings. This allows you to import the numbers in a
      spreadsheet (e.g, "101.00 USD" does not get parsed by a spreadsheet
      import, but "101.00" does).
    """, [OptDesc("operating_currency", [], "USD")]),

    OptGroup("""
      A boolean, true if the number formatting routines should output commas
      as thousand separators in numbers.
    """, [OptDesc("render_commas", "", "")]),

    OptGroup("""
      A string that defines which set of plugins is to be run by the loader: if
      the mode is "default", a preset list of plugins are automatically run
      before any user plugin. If the mode is "raw", no preset plugins are run at
      all, only user plugins are run (the user should explicitly load the
      desired list of plugins by using the 'plugin' option. This is useful in case the
      user wants full control over the ordering in which the plugins are run).
    """, [OptDesc("plugin_processing_mode", "default", "raw")]),

    OptGroup("""
      A list of Python modules containing transformation functions to run the
      entries through after parsing. The parser reads the entries as they are,
      transforms them through a list of standard functions, such as balance
      checks and inserting padding entries, and then hands the entries over to
      those plugins to add more auto-generated goodies. The list is a list of
      pairs/tuples, in the format (plugin-name, plugin-configuration). The
      plugin-name should be the name of a Python module to import, and within
      the module we expect a special '__plugins__' attribute that should list
      the name of transform functions to run the entries through. The
      plugin-configuration argument is an optional string to be provided by the
      user. Each function accepts a pair of (entries, options_map) and should
      return a pair of (new entries, error instances). If a plugin configuration
      is provided, it is provided as an extra argument to the plugin function.
      Errors should not be printed out the output, they will be converted to
      strins by the loader and displayed as dictacted by the output medium.
    """, [OptDesc("plugin", [], "beancount.plugins.module_name")]),

    OptGroup("""
      The number of lines beyond which a multi-line string will trigger a
      overly long line warning. This warning is meant to help detect a dangling
      quote by warning users of unexpectedly long strings.
    """, [OptDesc("long_string_maxlines", 64, 64)]),

    ]


OPTION_GROUPS = PRIVATE_OPTION_GROUPS + PUBLIC_OPTION_GROUPS


# A dict of the option names to their default value.
DEFAULT_OPTIONS = {desc.name: desc.default_value
                   for group in OPTION_GROUPS
                   for desc in group.options}


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
    """Return account names for the previous earnings, balances and conversion accounts.

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
    """Return account names for the current earnings and conversion accounts.

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


def list_options():
    """Produce a formatted text of the available options and their description.

    Returns:
      A string, formatted nicely to be printed in 80 columns.
    """
    oss = io.StringIO()
    for group in PUBLIC_OPTION_GROUPS:
        for desc in group.options:
            oss.write('{}: {}\n'.format(desc.name, repr(desc.default_value)))
        description = ' '.join(line.strip()
                               for line in group.description.strip().splitlines())
        oss.write(textwrap.fill(description,
                                initial_indent='  ',
                                subsequent_indent='  '))
        oss.write('\n\n')
    return oss.getvalue()

"""
Declaration of options and their default values.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import collections
import io
import re
import textwrap

from beancount.core.amount import D
from beancount.core import account_types
from beancount.core import account
from beancount.core import display_context


def options_validate_processing_mode(value):
    """Validate the options processing mode.

    Args:
      value: A string, the value provided as option.
    Returns:
      The new value, converted, if the conversion is successful.
    Raises:
      ValueError: If the value is invalid.
    """
    if value not in ('raw', 'default'):
        raise ValueError("Invalid value '{}'".format(value))
    return value


def options_validate_plugin(value):
    """Validate the plugin option.

    Args:
      value: A string, the value provided as option.
    Returns:
      The new value, converted, if the conversion is successful.
    Raises:
      ValueError: If the value is invalid.
    """
    # Process the 'plugin' option specially: accept an optional
    # argument from it. NOTE: We will eventually phase this out and
    # replace it by a dedicated 'plugin' directive.
    match = re.match('(.*):(.*)', value)
    if match:
        plugin_name, plugin_config = match.groups()
    else:
        plugin_name, plugin_config = value, None
    return (plugin_name, plugin_config)


def options_validate_tolerance(value):
    """Validate the tolerance option.

    Args:
      value: A string, the value provided as option.
    Returns:
      The new value, converted, if the conversion is successful.
    Raises:
      ValueError: If the value is invalid.
    """
    return D(value)


def options_validate_default_tolerance(value):
    """Validate the default_tolerance option.

    Args:
      value: A string, the value provided as option.
    Returns:
      The new value, converted, if the conversion is successful.
    Raises:
      ValueError: If the value is invalid.
    """
    # Process the setting of a key-value, whereby the value is a Decimal
    # representation.
    match = re.match('(.*):(.*)', value)
    if not match:
        raise ValueError("Invalid value '{}'".format(value))
    currency, tolerance_str = match.groups()
    return (currency, D(tolerance_str))


def options_validate_boolean(value):
    """Validate a boolean option.

    Args:
      value: A string, the value provided as option.
    Returns:
      The new value, converted, if the conversion is successful.
    Raises:
      ValueError: If the value is invalid.
    """
    return value.lower() in ('1', 'true', 'yes')


# List of option groups, with their description, option names and default
# values.
OptGroup = collections.namedtuple('OptGroup',
                                  'description options')


# An option description.
#
# Attributes:
#  name: A string, the short name of the option, as used in the syntax.
#  default_value: The default value for the option. If an option may
#    show up multiple times, should be a list or a dict.
#  example_value: The value to be rendered in the documentation. Even if
#    the value may be specified multiple times, this should just be an
#    example string for the user to model itself on.
#  converter: A function object to be called to convert or validate the
#    option during parsing, or None, if no conversion is necessary. The
#    callable must either succesfully return with the parsed value, or
#    raise a ValueError for the handler to report an error to the parser.
#  deprecated: A string, a message set if the option is deprecated. This is
#    used to issue suitable warnings when options aren't honored or about
#    not to be anymore.
OptDesc = collections.namedtuple(
    'OptDesc',
    'name default_value example_value converter deprecated')

UNSET = object()

# pylint: disable=invalid-name
def Opt(name, default_value,
        example_value=UNSET,
        converter=None,
        deprecated=False):
    """Alternative constructor for OptDesc, with default values.

    Args:
      name: See OptDesc.
      default_value: See OptDesc.
      example_value: See OptDesc.
      converter: See OptDesc.
      deprecated: See OptDesc.
    Returns:
      An instance of OptDesc.
    """
    if example_value is UNSET:
        example_value = default_value
    return OptDesc(name, default_value, example_value, converter, deprecated)


_TYPES = account_types.DEFAULT_ACCOUNT_TYPES


# Options that are not to be shown to the user.
PRIVATE_OPTION_GROUPS = [

    OptGroup("""
      The name of the top-level Beancount input file parsed from which the
      contents of the ledger have been extracted. This may be None, if no file
      was used.
    """, [Opt("filename", None)]),

    OptGroup("""
      A list of other filenames to include. This is output from the parser and
      processed by the loader but the list should otherwise have been cleared by the
      time it gets to the top-level loader.load_*() function that invoked it.
      The filenames are absolute. Relative include filenames are resolved against
      the file that contains the include directives.
    """, [Opt("include", [], "some-other-file.beancount")]),

    OptGroup("""
      An instance of DisplayContext, which is used to format numbers for output
      with precision inferred from that in the input file. This is created
      automatically by the parser.
    """, [Opt("display_context", display_context.DisplayContext())]),

    OptGroup("""
      A set of all the commodities that we have seen in the file.
      This is mainly used for efficiency, best computed once at parse time.
    """, [Opt("commodities", set())]),

    ]


# Options that are visible to the user and that can be set.
PUBLIC_OPTION_GROUPS = [

    OptGroup("""
      The title of this ledger / input file. This shows up at the top of every
      page.
    """, [Opt("title", "Beancount", "Joe Smith's Personal Ledger")]),

    OptGroup("""
      Root names of every account. This can be used to customize your category
      names, so that if you prefer "Revenue" over "Income" or "Capital" over
      "Equity", you can set them here. The account names in your input files
      must match, and the parser will validate these. You should place these
      options at the beginning of your file, because they affect how the parser
      recognizes account names.
    """, [
        Opt("name_assets", _TYPES.assets),
        Opt("name_liabilities", _TYPES.liabilities),
        Opt("name_equity", _TYPES.equity),
        Opt("name_income", _TYPES.income),
        Opt("name_expenses", _TYPES.expenses),
    ]),

    OptGroup("""
      Leaf name of the equity account used for summarizing previous transactions
      into opening balances.
    """, [Opt("account_previous_balances", "Opening-Balances")]),

    OptGroup("""
      Leaf name of the equity account used for transferring previous retained
      earnings from income and expenses accrued before the beginning of the
      exercise into the balance sheet.
    """, [Opt("account_previous_earnings", "Earnings:Previous")]),

    OptGroup("""
      Leaf name of the equity account used for inserting conversions that will
      zero out remaining amounts due to transfers before the opening date. This
      will essentially "fixup" the basic accounting equation due to the errors
      that priced conversions introduce.
    """, [Opt("account_previous_conversions", "Conversions:Previous")]),

    OptGroup("""
      Leaf name of the equity account used for transferring current retained
      earnings from income and expenses accrued during the current exercise into
      the balance sheet. This is most often called "Net Income".
    """, [Opt("account_current_earnings", "Earnings:Current")]),

    OptGroup("""
      Leaf name of the equity account used for inserting conversions that will
      zero out remaining amounts due to transfers during the exercise period.
    """, [Opt("account_current_conversions", "Conversions:Current")]),

    OptGroup("""
      The name of an account to be used to post to and accumulate rounding error.
      This is unset and this feature is disabled by default; setting this value to
      an account name will automatically enable the addition of postings on all
      transactions that have a residual amount.
    """, [Opt("account_rounding", None, "Equity:Rounding")]),

    OptGroup("""
      The imaginary currency used to convert all units for conversions at a
      degenerate rate of zero. This can be any currency name that isn't used in
      the rest of the ledger. Choose something unique that makes sense in your
      language.
    """, [Opt("conversion_currency", "NOTHING")]),

    OptGroup("""
      Mappings of currency to the tolerance used when it cannot be inferred
      automatically. The tolerance at hand is the one used for verifying (1)
      that transactions balance, (2) that explicit balance checks from 'balance'
      directives balance, and (3) in the precision used for padding (from the
      'pad' directive).

      The values must be strings in the following format:
        <currency>:<tolerance>
      for example, 'USD:0.005'.

      By default, the default tolerance used for currencies without an explicit
      value is zero (which means infinite precision). As a special case, this
      default value, that is, the default value used for all currencies without
      an explicit default can be overridden using the '*' currency, like this:
      '*:0.5'. Used by itself, this last example sets the default tolerance as
      '0.5' for all currencies.

      For detailed documentation about how precision is handled, see this doc:
      http://furius.ca/beancount/doc/precision
    """, [Opt("default_tolerance", {}, "CHF:0.01",
              converter=options_validate_default_tolerance)]),

    # Note: This option will go away. Its behavior has been replaced by
    # precision/tolerance inference.
    # See this for details: http://furius.ca/beancount/doc/precision
    OptGroup("""
      The tolerance allowed for balance checks and padding directives. In the
      real world, rounding occurs in various places, and we need to allow a
      small (but very small) amount of tolerance in checking the balance of
      transactions and in requiring padding entries to be auto-inserted. This is
      the tolerance amount, which you can override.
    """, [Opt("tolerance", D("0.015"), "0.015",
              converter=options_validate_tolerance,
              deprecated=("The 'tolerance' option has been deprecated "
                          "and has no effect."))]),

    OptGroup("""
      Restore the legacy fixed handling of tolerances. Balance and Pad directives
      have a fixed tolerance of 0.015 units, and Transactions balance at 0.005 units.
      For any units. This is intended as a way for people to revert the behavior of
      Beancount to ease the transition to the new inferred tolerance logic. See
      http://furius.ca/beancount/doc/tolerances for more details.
    """, [Opt("use_legacy_fixed_tolerances", False, True)]),

    OptGroup("""
      A list of directory roots, relative to the CWD, which should be searched
      for document files. For the document files to be automatically found they
      must have the following filename format: YYYY-MM-DD.(.*)
    """, [Opt("documents", [], "/path/to/your/documents/archive")]),

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
    """, [Opt("operating_currency", [], "USD")]),

    OptGroup("""
      A boolean, true if the number formatting routines should output commas
      as thousand separators in numbers.
    """, [Opt("render_commas", False, "TRUE",
              converter=options_validate_boolean)]),

    OptGroup("""
      A string that defines which set of plugins is to be run by the loader: if
      the mode is "default", a preset list of plugins are automatically run
      before any user plugin. If the mode is "raw", no preset plugins are run at
      all, only user plugins are run (the user should explicitly load the
      desired list of plugins by using the 'plugin' option. This is useful in case the
      user wants full control over the ordering in which the plugins are run).
    """, [Opt("plugin_processing_mode", "default", "raw",
              converter=options_validate_processing_mode)]),

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
    """, [Opt("plugin", [], "beancount.plugins.module_name",
              converter=options_validate_plugin,
              deprecated=("The 'plugin' option is deprecated; it should be "
                          "replaced by the 'plugin' directive"))]),

    OptGroup("""
      The number of lines beyond which a multi-line string will trigger a
      overly long line warning. This warning is meant to help detect a dangling
      quote by warning users of unexpectedly long strings.
    """, [Opt("long_string_maxlines", 64)]),

    OptGroup("""
      Enable an EXPERIMENTAL feature that supports an explicit tolerance value
      on Balance assertions. If enabled, the balance amount supports a tolerance in
      the input, with this syntax: <number> ~ <tolerance> <currency>, for example,
      "532.23 ~ 0.001 USD".

      See the document on tolerances for more details:
      http://furius.ca/beancount/doc/tolerances

      WARNING: This feature may go away at any time. It is an exploration to see
      if it is truly useful. We may be able to do without.
    """, [Opt("experiment_explicit_tolerances", False, True)]),

    OptGroup("""
      Enable an EXPERIMENTAL feature that expands the maximum tolerance inferred
      on transactions to include values on cost currencies inferred by postings
      held at-cost or converted at price. Those postings can imply a tolerance
      value by multiplying the smallest digit of the unit by the cost or price value
      and taking half of that value.

      For example, if a posting has an amount of "2.345 RGAGX {45.00 USD}"
      attached to it, it implies a tolerance of 0.001 x 45.00 / 2 = 0.045 USD
      and this is added to the mix to enlarge the tolerance allowed for units of
      USD on that transaction. All the normally inferred tolerances (see
      http://furius.ca/beancount/doc/tolerances) are still taken into account.
      Enabling this flag only makes the tolerances potentially wider.

      WARNING: This feature may go away in the future. It is an exploration to see
      if it is truly useful.
    """, [Opt("experiment_infer_tolerance_from_cost", False, True)]),

    ]


OPTION_GROUPS = PRIVATE_OPTION_GROUPS + PUBLIC_OPTION_GROUPS

# A dict of the option names to their descriptors.
OPTIONS = {desc.name: desc
           for group in OPTION_GROUPS
           for desc in group.options}


# A dict of the option names to their default value.
OPTIONS_DEFAULTS = {desc.name: desc.default_value
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
            oss.write('option "{}" "{}"\n'.format(desc.name, desc.example_value))
            if desc.deprecated:
                oss.write(textwrap.fill(
                    "THIS OPTION IS DEPRECATED: {}".format(desc.deprecated),
                    initial_indent="  ",
                    subsequent_indent="  "))
                oss.write('\n\n')
        description = ' '.join(line.strip()
                               for line in group.description.strip().splitlines())
        oss.write(textwrap.fill(description,
                                initial_indent='  ',
                                subsequent_indent='  '))
        oss.write('\n')

        if isinstance(desc.default_value, (list, dict, set)):
            oss.write('\n')
            oss.write('  (This option may be supplied multiple times.)\n')

        oss.write('\n\n')

    return oss.getvalue()

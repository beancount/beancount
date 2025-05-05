"""
Declaration of options and their default values.
"""

from __future__ import annotations

__copyright__ = "Copyright (C) 2013-2021, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import io
import re
import textwrap
from typing import Any
from typing import Callable
from typing import NamedTuple

from beancount.core import account
from beancount.core import account_types
from beancount.core import data
from beancount.core import display_context
from beancount.core.number import D


def options_validate_processing_mode(value):
    """Validate the options processing mode.

    Args:
      value: A string, the value provided as option.
    Returns:
      The new value, converted, if the conversion is successful.
    Raises:
      ValueError: If the value is invalid.
    """
    if value not in ("raw", "default"):
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
    match = re.match("(.*):(.*)", value)
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


def options_validate_tolerance_map(value):
    """Validate an option with a map of currency/tolerance pairs in a string.

    Args:
      value: A string, the value provided as option.
    Returns:
      The new value, converted, if the conversion is successful.
    Raises:
      ValueError: If the value is invalid.
    """
    # Process the setting of a key-value, whereby the value is a Decimal
    # representation.
    match = re.match("(.*):(.*)", value)
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
    return value.lower() in ("1", "true", "yes")


def options_validate_booking_method(value):
    """Validate a booking method name.

    Args:
      value: A string, the value provided as option.
    Returns:
      The new value, converted, if the conversion is successful.
    Raises:
      ValueError: If the value is invalid.
    """
    try:
        return data.Booking[value]
    except KeyError as exc:
        raise ValueError(str(exc)) from exc


def options_validate_root_account(value):
    """Validate a root account name.

    Args:
      value: A string, the value provided as option.
    Returns:
      The root account name, if the validation succedes.
    Raises:
      ValueError: If the value is invalid.
    """
    if not account.is_valid_root(value):
        raise ValueError(f"Invalid root account name: {value!r}")
    return value


def options_validate_leaf_account(value):
    """Validate a leaf account name.

    Args:
      value: A string, the value provided as option.
    Returns:
      The leaf account name, if the validation succedes.
    Raises:
      ValueError: If the value is invalid.
    """
    if not account.is_valid_leaf(value):
        raise ValueError(f"Invalid leaf account name: {value!r}")
    return value


class OptDesc(NamedTuple):
    """An option description.

    Attributes:
      name: A string, the short name of the option, as used in the syntax.
      default_value: The default value for the option. If an option may
        show up multiple times, should be a list or a dict.
      example_value: The value to be rendered in the documentation. Even if
        the value may be specified multiple times, this should just be an
        example string for the user to model itself on.
      converter: A function object to be called to convert or validate the
        option during parsing, or None, if no conversion is necessary. The
        callable must either successfully return with the parsed value, or
        raise a ValueError for the handler to report an error to the parser.
      deprecated: A string, a message set if the option is deprecated. This is
        used to issue suitable warnings when options aren't honored or about
        not to be anymore.
      alias: A string or None; if set, this option automatically gets
        translated to this alias. This is present to support renaming of
        option names.
    """

    name: str
    default_value: Any
    example_value: Any
    converter: Callable | None
    deprecated: str
    alias: str | None


class OptGroup(NamedTuple):
    """List of option groups, with their description,
    option names and default values.
    """

    description: str
    options: list[OptDesc]


UNSET = object()


def Opt(
    name: str,
    default_value: Any,
    example_value=UNSET,
    converter=None,
    deprecated: str = "",
    alias=None,
) -> OptDesc:
    """Alternative constructor for OptDesc, with default values.

    Args:
      name: See OptDesc.
      default_value: See OptDesc.
      example_value: See OptDesc.
      converter: See OptDesc.
      deprecated: See OptDesc.
      alias: See OptDesc.
    Returns:
      An instance of OptDesc.
    """
    if example_value is UNSET:
        example_value = default_value
    return OptDesc(name, default_value, example_value, converter, deprecated, alias)


_TYPES = account_types.DEFAULT_ACCOUNT_TYPES


# Options that consist of data produced as a by-product of the parsing process.
# These options cannot be input by the user. This is essentially read-only state
# that is conceptually separate from the input options.
OUTPUT_OPTION_GROUPS = [
    OptGroup(
        """
      The name of the top-level Beancount input file parsed from which the
      contents of the ledger have been extracted. This may be None, if no file
      was used.
    """,
        [Opt("filename", None)],
    ),
    OptGroup(
        """
      A list of other filenames to include. This is output from the parser and
      processed by the loader but the list should otherwise have been cleared by the
      time it gets to the top-level loader.load_*() function that invoked it.
      The filenames are absolute. Relative include filenames are resolved against
      the file that contains the include directives.

      This is used in the parser, but also, the loader sets this list to the
      full list of parsed absolute filenames in the options map. This is how you
      can find out the entire list of files involved in a Beancount load
      procedure.
    """,
        [Opt("include", [], "some-other-file.beancount")],
    ),
    OptGroup(
        """
      A hash of some of the input data. This is used to supplement the
      timestamps of the input files for the purpose of load caching. We
      typically hash the sizes of the files or perhaps even some of the
      contents, or determine any of the inputs have changed beyond the
      timestamps of the input files. (Internal use only; do not rely on this.)
    """,
        [Opt("input_hash", "", "841ee3be9acef165feba2342")],
    ),
    OptGroup(
        """
      An instance of DisplayContext, which is used to format numbers for output
      with precision inferred from that in the input file. This is created
      automatically by the parser.
    """,
        [Opt("dcontext", display_context.DisplayContext())],
    ),
    OptGroup(
        """
      A set of all the commodities that we have seen in the file.
      This is mainly used for efficiency, best computed once at parse time.
    """,
        [Opt("commodities", set())],
    ),
    OptGroup(
        """
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
      strings by the loader and displayed as dictated by the output medium.
    """,
        [
            Opt(
                "plugin",
                [],
                "beancount.plugins.module_name",
                converter=options_validate_plugin,
            )
        ],
    ),
]


# Options that are visible to the user and that can be set.
PUBLIC_OPTION_GROUPS = [
    OptGroup(
        """
      The title of this ledger / input file. This shows up at the top of every
      page.
    """,
        [Opt("title", "Beancount", "Joe Smith's Personal Ledger")],
    ),
    OptGroup(
        """
      Root names of every account. This can be used to customize your category
      names, so that if you prefer "Revenue" over "Income" or "Capital" over
      "Equity", you can set them here. The account names in your input files
      must match, and the parser will validate these. You should place these
      options at the beginning of your file, because they affect how the parser
      recognizes account names.
    """,
        [
            Opt("name_assets", _TYPES.assets, converter=options_validate_root_account),
            Opt(
                "name_liabilities",
                _TYPES.liabilities,
                converter=options_validate_root_account,
            ),
            Opt("name_equity", _TYPES.equity, converter=options_validate_root_account),
            Opt("name_income", _TYPES.income, converter=options_validate_root_account),
            Opt("name_expenses", _TYPES.expenses, converter=options_validate_root_account),
        ],
    ),
    OptGroup(
        """
      Leaf name of the equity account used for summarizing previous transactions
      into opening balances.
    """,
        [
            Opt(
                "account_previous_balances",
                "Opening-Balances",
                converter=options_validate_leaf_account,
            )
        ],
    ),
    OptGroup(
        """
      Leaf name of the equity account used for transferring previous retained
      earnings from income and expenses accrued before the beginning of the
      exercise into the balance sheet.
    """,
        [
            Opt(
                "account_previous_earnings",
                "Earnings:Previous",
                converter=options_validate_leaf_account,
            )
        ],
    ),
    OptGroup(
        """
      Leaf name of the equity account used for inserting conversions that will
      zero out remaining amounts due to transfers before the opening date. This
      will essentially "fixup" the basic accounting equation due to the errors
      that priced conversions introduce.
    """,
        [
            Opt(
                "account_previous_conversions",
                "Conversions:Previous",
                converter=options_validate_leaf_account,
            )
        ],
    ),
    OptGroup(
        """
      Leaf name of the equity account used for transferring current retained
      earnings from income and expenses accrued during the current exercise into
      the balance sheet. This is most often called "Net Income".
    """,
        [
            Opt(
                "account_current_earnings",
                "Earnings:Current",
                converter=options_validate_leaf_account,
            )
        ],
    ),
    # This common option can be used by reporting systems and is ignored by
    # Beancount itself.
    OptGroup(
        """
      Leaf name of the equity account used for inserting conversions that will
      zero out remaining amounts due to transfers during the exercise period.
    """,
        [
            Opt(
                "account_current_conversions",
                "Conversions:Current",
                converter=options_validate_leaf_account,
            )
        ],
    ),
    # This common option can be used by reporting systems and is ignored by
    # Beancount itself.
    OptGroup(
        """
      The name of an account to be used to post unrealized gains to. This is used
      when making any kind of conversion from cost to price on a balance sheet
      (or any realization). The amount inserted - the difference between book
      value and market value - has to be posted to a gains account to keep the
      balance on the sheet. This has no effect on behavior, other than providing
      a configurable account name for such postings to occur.
    """,
        [
            Opt(
                "account_unrealized_gains",
                "Earnings:Unrealized",
                "Earnings:Unrealized",
                converter=options_validate_leaf_account,
            )
        ],
    ),
    OptGroup(
        """
      The name of an account to be used to post to and accumulate rounding error.
      This is unset and this feature is disabled by default; setting this value to
      an account name will automatically enable the addition of postings on all
      transactions that have a residual amount.
    """,
        [
            Opt(
                "account_rounding",
                None,
                "Rounding",
                converter=options_validate_leaf_account,
            )
        ],
    ),
    OptGroup(
        """
      The imaginary currency used to convert all units for conversions at a
      degenerate rate of zero. This can be any currency name that isn't used in
      the rest of the ledger. Choose something unique that makes sense in your
      language.
    """,
        [Opt("conversion_currency", "NOTHING")],
    ),
    OptGroup(
        """
      Mappings of currency to the tolerance used when it cannot be inferred
      automatically. The tolerance at hand is the one used for verifying (1)
      that transactions balance, (2) explicit balance checks from 'balance'
      directives balance, and (3) in the tolerance used for padding (from the
      'pad' directive).

      The values must be strings in the following format:
        <currency>:<tolerance>
      for example, 'USD:0.005'.

      By default, the tolerance allowed for currencies without an inferred value
      is zero. As a special case, this value, that is, the fallback value used
      for all currencies without an explicit default can be overridden using the
      '*' currency, like this:  '*:0.5'. Used by itself, this last example sets
      the fallabck tolerance as '0.5' for all currencies.

      For detailed documentation about how tolerances are handled, see this doc:
      http://furius.ca/beancount/doc/tolerances
    """,
        [
            Opt(
                "inferred_tolerance_default",
                {},
                "CHF:0.01",
                converter=options_validate_tolerance_map,
            )
        ],
    ),
    OptGroup(
        """
      A multiplier for inferred tolerance values.

      When the tolerance values aren't specified explicitly via the
      'inferred_tolerance_default' option, the tolerance is inferred from the
      numbers in the input file. For example, if a transaction has posting with
      a value like '32.424 CAD', the tolerance for CAD will be inferred to be
      0.001 times some multiplier. This is the muliplier value.

      We normally assume that the institution we're reproducing this posting
      from applies rounding, and so the default value for the multiplier is
      0.5, that is, half of the smallest digit encountered.

      You can customize this multiplier by changing this option, typically
      expanding it to account for amounts slightly beyond the usual tolerance,
      for example, if you deal with institutions with bad of unexpected rounding
      behaviour.

      For detailed documentation about how tolerances are handled, see this doc:
      http://furius.ca/beancount/doc/tolerances
    """,
        [Opt("inferred_tolerance_multiplier", D("0.5"), "1.1", converter=D)],
    ),
    OptGroup(
        """
      Enable a feature that expands the maximum tolerance inferred on
      transactions to include values on cost currencies inferred by postings
      held at-cost or converted at price. Those postings can imply a tolerance
      value by multiplying the smallest digit of the unit by the cost or price
      value and taking half of that value.

      For example, if a posting has an amount of "2.345 RGAGX {45.00 USD}"
      attached to it, it implies a tolerance of 0.001 x 45.00 * M = 0.045 USD
      (where M is the inferred_tolerance_multiplier) and this is added to the
      mix to enlarge the tolerance allowed for units of USD on that transaction.
      All the normally inferred tolerances (see
      http://furius.ca/beancount/doc/tolerances) are still taken into account.
      Enabling this flag only makes the tolerances potentially wider.
    """,
        [Opt("infer_tolerance_from_cost", False, True)],
    ),
    OptGroup(
        """
      A list of directory roots, relative to the CWD, which should be searched
      for document files. For the document files to be automatically found they
      must have the following filename format: YYYY-MM-DD.(.*)
    """,
        [Opt("documents", [], "/path/to/your/documents/archive")],
    ),
    OptGroup(
        """
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

      If you need to enter a list of operating currencies, you may input this
      option multiple times, that is, you repeat the entire directive once for
      each desired operating currency.
    """,
        [Opt("operating_currency", [], "USD")],
    ),
    OptGroup(
        """
      A boolean, true if the number formatting routines should output commas
      as thousand separators in numbers.
    """,
        [Opt("render_commas", False, "TRUE", converter=options_validate_boolean)],
    ),
    OptGroup(
        """
      A string that defines which set of plugins is to be run by the loader: if
      the mode is "default", a preset list of plugins are automatically run
      before any user plugin. If the mode is "raw", no preset plugins are run at
      all, only user plugins are run (the user should explicitly load the
      desired list of plugins by using the 'plugin' option. This is useful in case the
      user wants full control over the ordering in which the plugins are run).
    """,
        [
            Opt(
                "plugin_processing_mode",
                "default",
                "raw",
                converter=options_validate_processing_mode,
            )
        ],
    ),
    OptGroup(
        """
      The number of lines beyond which a multi-line string will trigger an
      overly long line warning. This warning is meant to help detect a dangling
      quote by warning users of unexpectedly long strings.
    """,
        [Opt("long_string_maxlines", 64)],
    ),
    OptGroup(
        """
      The booking method to apply to ambiguous reductions of inventory lots.
      When a posting is matched against the contents of an account's inventory
      to reduce its contents and multiple lots match, the method dictates how
      this ambiguity is resolved. Methods include "STRICT" which raises an
      error, "FIFO" which selects the oldest lot, and "NONE" which allows any
      reduction to be added to the inventory despite the absence of a match
      (resulting in mixed inventories).

      See the following documents for details:
        http://furius.ca/beancount/doc/inventories
        http://furius.ca/beancount/doc/proposal-booking
    """,
        [
            Opt(
                "booking_method",
                data.Booking.STRICT,
                "STRICT",
                converter=options_validate_booking_method,
            )
        ],
    ),
    OptGroup(
        """
      Support the pipe (|) symbol to for transaction separator.

      This is only provided as a temporary stopgap to ease transition, and will
      be removed eventually. This is why this option is marked as deprecated.
    """,
        [
            Opt(
                "allow_pipe_separator",
                False,
                "TRUE",
                converter=options_validate_boolean,
                deprecated="Allowing pipe separator temporary; this will go away eventually.",
            )
        ],
    ),
    OptGroup(
        """
      Allow plugins to produce a None object for the 'tags' and 'links'
      attributes of a Transaction instance. By default, without this, those
      attributes are now ensured to be a Set type, and an empty frozenset()
      instance if there are no values

      This is only provided as a temporary mechanism to allow you some time to
      port your plugins code.
    """,
        [
            Opt(
                "allow_deprecated_none_for_tags_and_links",
                False,
                "TRUE",
                converter=options_validate_boolean,
                deprecated=("Allowing None for tags and link will go away eventually."),
            )
        ],
    ),
    OptGroup(
        """
      A boolean, if true, prepend the directory name of the top-level file to
      the PYTHONPATH.
    """,
        [Opt("insert_pythonpath", False, "TRUE", converter=options_validate_boolean)],
    ),
]


OPTION_GROUPS = OUTPUT_OPTION_GROUPS + PUBLIC_OPTION_GROUPS

# A dict of the option names to their descriptors.
OPTIONS = {desc.name: desc for group in OPTION_GROUPS for desc in group.options}


# A dict of the option names to their default value.
OPTIONS_DEFAULTS = {
    desc.name: desc.default_value for group in OPTION_GROUPS for desc in group.options
}


# A list of options that cannot be modified.
READ_ONLY_OPTIONS = {"filename", "plugin"}


def get_account_types(options):
    """Extract the account type names from the parser's options.

    Args:
      options: a dict of ledger options.
    Returns:
      An instance of AccountTypes, that contains all the prefixes.
    """
    return account_types.AccountTypes(
        *[
            options[key]
            for key in (
                "name_assets",
                "name_liabilities",
                "name_equity",
                "name_income",
                "name_expenses",
            )
        ]
    )


def get_previous_accounts(options):
    """Return account names for the previous earnings, balances and conversion accounts.

    Args:
      options: a dict of ledger options.
    Returns:
      A tuple of 3 account objects, for booking previous earnings,
      previous balances, and previous conversions.
    """
    equity = options["name_equity"]
    account_previous_earnings = account.join(equity, options["account_previous_earnings"])
    account_previous_balances = account.join(equity, options["account_previous_balances"])
    account_previous_conversions = account.join(
        equity, options["account_previous_conversions"]
    )
    return (
        account_previous_earnings,
        account_previous_balances,
        account_previous_conversions,
    )


def get_current_accounts(options):
    """Return account names for the current earnings and conversion accounts.

    Args:
      options: a dict of ledger options.
    Returns:
      A tuple of 2 account objects, one for booking current earnings, and one
      for current conversions.
    """
    equity = options["name_equity"]
    account_current_earnings = account.join(equity, options["account_current_earnings"])
    account_current_conversions = account.join(
        equity, options["account_current_conversions"]
    )
    return (account_current_earnings, account_current_conversions)


def get_unrealized_account(options):
    """Return the full account name for the unrealized account.

    Args:
      options: a dict of ledger options.
    Returns:
      A tuple of 2 account objects, one for booking current earnings, and one
      for current conversions.
    """
    income = options["name_income"]
    return account.join(income, options["account_unrealized_gains"])


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
                oss.write(
                    textwrap.fill(
                        "THIS OPTION IS DEPRECATED: {}".format(desc.deprecated),
                        initial_indent="  ",
                        subsequent_indent="  ",
                    )
                )
                oss.write("\n\n")
        description = " ".join(
            line.strip() for line in group.description.strip().splitlines()
        )
        oss.write(textwrap.fill(description, initial_indent="  ", subsequent_indent="  "))
        oss.write("\n")

        if isinstance(desc.default_value, (list, dict, set)):
            oss.write("\n")
            oss.write("  (This option may be supplied multiple times.)\n")

        oss.write("\n\n")

    return oss.getvalue()

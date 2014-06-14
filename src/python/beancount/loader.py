"""Loader code. This is the main entry point to load up a file.
"""
import functools
import textwrap
import importlib
import sys

from beancount.utils import misc_utils
from beancount.core import data
from beancount.parser import parser
from beancount.parser import options
from beancount.parser import documents
from beancount.parser import printer
from beancount.ops import pad
from beancount.ops import validation
from beancount.ops import check
from beancount.ops import unrealized
from beancount.ops import prices


def load(filename,
         add_unrealized_gains=True,
         do_print_errors=False,
         quiet=False,
         parse_method='filename'):
    """Load an input file: open the file and parse it, pad, check and validate it.
    This also optionally prints out the error messages.

    This file provides convenience routines that do all that's necessary to obtain a
    list of entries ready for realization and working with them. This is the most
    common entry point.

    Args:
      filename: the name of the file to be parsed.
      add_unrealized_gains: a boolean, true if the unrealized gains should be
                            inserted automatically in the list of entries, based
                            on the current price of things held at cost.
      do_print_errors: a boolean, true if this function should format and print out
                       errors. This is only available here because it's a common
                       thing to do with this function.

      quiet: a boolean, if true, the timing of each section of the parsing and
             validation process will be printed out on logging.info.

      parse_method: a string, 'filename' or 'string', that describes the contents
                    of 'filename'.
    Returns:
      A triple of (sorted list of entries from the file, a list of errors
      generated while parsing and validating the file, and a dict of the options
      parsed from the file).
    """

    # Parse the input file.
    if parse_method == 'filename':
        parse_fun = parser.parse
    elif parse_method == 'string':
        parse_fun = parser.parse_string
    else:
        raise NotImplementedError("Method: {}".format(parse_method))
    with misc_utils.print_time('parse', quiet):
        entries, parse_errors, options_map = parse_fun(filename)

    # Transform the entries.
    entries, errors = run_transformations(entries, parse_errors, options_map,
                                          filename,
                                          add_unrealized_gains, quiet)

    # Validate the list of entries.
    with misc_utils.print_time('validate', quiet):
        valid_errors = validation.validate(entries)
        errors.extend(valid_errors)

    # Print out the list of errors.
    if do_print_errors and errors:
        print(',----------------------------------------------------------------------')
        printer.print_errors(errors, file=sys.stdout)
        print('`----------------------------------------------------------------------')

    return entries, errors, options_map


def run_transformations(entries, parse_errors, options_map,
                        filename,
                        add_unrealized_gains, quiet):
    """Run the various transformations on the entries.

    This is where entries are being synthesized, checked, plugins are run, etc.

    Args:
      entries: A list of directives as read from the parser.
      parse_errors: A list of errors so far.
      options_map: An options dict as read from the parser.
      filename: A string, the name of the file that's just been parsed.
      add_unrealized_gains: A boolean, true if we should add the unrealized gains.
      quiet: A boolean, true if we should be quiet.
    Returns:
      A list of modified entries, and a list of errors, also possibly modified.
    """

    # A list of error lists to flatten.
    errors = list(parse_errors)

    # Pad the resulting entries (create synthetic Pad entries to balance checks
    # where desired).
    #
    # Note: I think a lot of these should be moved to plugins!
    with misc_utils.print_time('pad', quiet):
        entries, pad_errors = pad.pad(entries)
        errors.extend(pad_errors)

    # Add implicitly defined prices.
    with misc_utils.print_time('prices', quiet):
        entries, price_errors = prices.add_implicit_prices(entries)
        errors.extend(price_errors)

    with misc_utils.print_time('check', quiet):
        entries, check_errors = check.check(entries)
        errors.extend(check_errors)

    # Process the document entries and find documents automatically.
    with misc_utils.print_time('documents', quiet):
        # FIXME: Maybe the filename can be passed in through the options_map in
        # order to comply with the interface of all other plugins. Maybe
        # documents can just become yet another plugin...
        entries, doc_errors = documents.process_documents(entries, options_map, filename)
        errors.extend(doc_errors)

    # Add unrealized gains.
    if add_unrealized_gains:
        with misc_utils.print_time('unrealized', quiet):
            account_types = options.get_account_types(options_map)
            entries, unrealized_errors = unrealized.add_unrealized_gains(
                entries,
                account_types,
                options_map['account_unrealized'])
            errors.extend(unrealized_errors)

    # Ensure that the entries are sorted.
    entries.sort(key=data.entry_sortkey)

    # Run the load_filters on top of the results.
    for load_filter_function in LOAD_PLUGINS:
        entries, plugin_errors = load_filter_function(entries, options_map)
        errors.extend(plugin_errors)

    return entries, errors


def loaddoc(fun):
    """A decorator that will load the docstring and call the wrapped function with
    the results."""
    @functools.wraps(fun)
    def wrapper(self):
        contents = textwrap.dedent(fun.__doc__)
        entries, errors, options_map = load(contents,
                                            add_unrealized_gains=False,
                                            parse_method='string',
                                            quiet=True)
        return fun(self, entries, errors, options_map)
    wrapper.__doc__ = None
    return wrapper


# A global list of filter functions to be applied on all subsequent loads.
# Each function should accept a triplet of (entries, errors, options_map) and
# return a similar triplet.
LOAD_PLUGINS = []

def install_load_plugin(callback):
    """Register a ledger load filter, that gets invoked after every time we load or
    reload the ledger file.

    Args:
      callback: a callable that gets invoked with the result of load(), that is,
                 with entries, errors, options_map. The function should return new
                 values for these, that is, a triple of entries, errors, options_map.
    """
    LOAD_PLUGINS.append(callback)


def uninstall_load_plugin(callback):
    """Unregister a ledger load filter.

    Args:
      callback: See install_load_plugin.
    """
    LOAD_PLUGINS.remove(callback)


def install_plugins(plugin_names):
    """Install a list of plugin names.

    Args:
      plugin_name: A list of string, the names of modules to import.
    """
    for plugin_name in plugin_names:
        module = importlib.import_module(plugin_name)
        if hasattr(module, '__plugins__'):
            for function_name in module.__plugins__:
                callback = getattr(module, function_name)
                install_load_plugin(callback)

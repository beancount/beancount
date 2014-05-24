"""Loader code. This is the main entry point to load up a file.
"""
import functools
import textwrap
import importlib

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
        raise NotImplementedError
    with misc_utils.print_time('parse', quiet):
        entries, parse_errors, options_map = parse_fun(filename)

    # Pad the resulting entries (create synthetic Pad entries to balance checks
    # where desired).
    with misc_utils.print_time('pad', quiet):
        entries, pad_errors = pad.pad(entries)

    with misc_utils.print_time('check', quiet):
        entries, check_errors = check.check(entries)

    # Process the document entries and find documents automatically.
    with misc_utils.print_time('documents', quiet):
        entries, doc_errors = documents.process_documents(entries,
                                                          filename,
                                                          options_map['documents'])

    # Add unrealized gains.
    if add_unrealized_gains:
        with misc_utils.print_time('unrealized', quiet):
            account_types = options.get_account_types(options_map)
            entries = unrealized.add_unrealized_gains(
                entries,
                account_types,
                options_map['account_unrealized'])

    # Validate the list of entries.
    with misc_utils.print_time('validate', quiet):
        valid_errors = validation.validate(entries)

    # Print out the list of errors.
    errors = parse_errors + pad_errors + check_errors + valid_errors + doc_errors
    if do_print_errors:
        error_text = printer.format_errors(errors)
        if error_text:
            print(',----------------------------------------------------------------------')
            print(error_text)
            print('`----------------------------------------------------------------------')

    # Run the load_filters on top of the results.
    for load_filter_function in LOAD_PLUGINS:
        entries, errors, options_map = load_filter_function(
            entries, errors, options_map)

    # Ensure that the entries are sorted.
    entries.sort(key=data.entry_sortkey)

    return entries, errors, options_map


def loaddoc(fun):
    """A decorator that will load the docstring and call the wrapped function with
    the results."""
    @functools.wraps(fun)
    def wrapper(self):
        contents = textwrap.dedent(fun.__doc__)
        entries, errors, options_map = load(contents, parse_method='string', quiet=True)
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

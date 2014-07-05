"""Loader code. This is the main entry point to load up a file.
"""
import functools
import textwrap
import importlib
import collections
import re
import itertools

from beancount.utils import misc_utils
from beancount.core import data
from beancount.parser import parser
from beancount.ops import validation


LoadError = collections.namedtuple('LoadError', 'fileloc message entry')


# List of default plugins to run.
DEFAULT_PLUGINS = [
    "beancount.ops.pad",
    "beancount.ops.prices",
    "beancount.ops.balance",
    "beancount.ops.documents",
    ]


def load(filename, log_function=None):
    """Open a Beancount input file, parse it, run transformations and validate.

    Args:
      filename: The name of the file to be parsed.
      log_function: A function to write timing log entries to, or None, if it
        should be quiet.
    Returns:
      A triple of:
        entries: A date-sorted list of entries from the file.
        errors: A list of error objects generated while parsing and validating
          the file.
        options_map: A dict of the options parsed from the file.
    """
    return _load(filename, log_function, parser.parse)


def load_string(string, log_function=None):
    """Open a Beancount input string, parse it, run transformations and validate.

    Args:
      string: A Beancount input string.
      log_function: A function to write timing log entries to, or None, if it
        should be quiet.
    Returns:
      A triple of:
        entries: A date-sorted list of entries from the file.
        errors: A list of error objects generated while parsing and validating
          the file.
        options_map: A dict of the options parsed from the file.
    """
    return _load(string, log_function, parser.parse_string)


def _load(file_or_string, log_function, parse_function):
    """Parse Beancount input, run its transformations and validate it.

    (This is an internal method.)
    This routine does all that is necessary to obtain a list of entries ready
    for realization and working with them. This is the principal call for of the
    scripts that load a ledger. It returns a list of entries transformed and
    ready for reporting, a list of errors, and parser's options dict.

    Args:
      file_or_string: The name of the file to be parsed, or an input string.
      log_function: A function to write timing log entries to, or None, if it
        should be quiet.
      parse_function: A function used to parse file_or_string. Either
        parser.parse() or parser.parse_string().
    Returns:
      See load() or load_string().
    """
    # Parse the input file.
    with misc_utils.print_time('beancount.parser.parser', log_function):
        entries, parse_errors, options_map = parse_function(file_or_string)

    # Transform the entries.
    entries, errors = run_transformations(entries, parse_errors, options_map, log_function)

    # Validate the list of entries.
    with misc_utils.print_time('beancount.ops.validate', log_function):
        valid_errors = validation.validate(entries, options_map)
        errors.extend(valid_errors)

        # FIXME: Check here that the entries haven't been modified, by comparing
        # hashes before and after.

    return entries, errors, options_map


def run_transformations(entries, parse_errors, options_map, log_function):
    """Run the various transformations on the entries.

    This is where entries are being synthesized, checked, plugins are run, etc.

    Args:
      entries: A list of directives as read from the parser.
      parse_errors: A list of errors so far.
      options_map: An options dict as read from the parser.
      log_function: A function to write timing log entries to, or None, if it
        should be quiet.
    Returns:
      A list of modified entries, and a list of errors, also possibly modified.
    """
    # A list of errors to extend (make a copy to avoid modifying the input).
    errors = list(parse_errors)

    # Ensure that the entries are sorted before running the plugins.
    entries.sort(key=data.entry_sortkey)

    # Process the plugins.
    for plugin_name in itertools.chain(DEFAULT_PLUGINS,
                                       options_map["plugin"]):

        # Parse out the option if one was specified.
        mo = re.match('(.*):(.*)', plugin_name)
        if mo:
            plugin_name, plugin_option = mo.groups()
        else:
            plugin_option = None

        # Try to import the module.
        try:
            module = importlib.import_module(plugin_name)
            if not hasattr(module, '__plugins__'):
                continue

            with misc_utils.print_time(plugin_name, log_function):

                # Run each transformer function in the plugin.
                for function_name in module.__plugins__:
                    callback = getattr(module, function_name)
                    callback_name = '{}.{}'.format(plugin_name, function_name)

                    if plugin_option is not None:
                        entries, plugin_errors = callback(entries, options_map,
                                                          plugin_option)
                    else:
                        entries, plugin_errors = callback(entries, options_map)
                    errors.extend(plugin_errors)

        except ImportError as exc:
            # Upon failure, just issue an error.
            errors.append(LoadError(data.FileLocation("<load>", 0),
                                    'Error importing "{}": {}'.format(
                                        plugin_name, str(exc)), None))

    # Ensure that the entries are sorted.
    entries.sort(key=data.entry_sortkey)

    return entries, errors


def loaddoc(fun):
    """A decorator that loads the docstring and calls the function with parsed entries.

    This is an incredibly convenient tool to write lots of tests. Write a
    unittest using the standard TestCase class and put the input entries in the
    function's docstring.

    Args:
      fun: A callable method, that accepts the three return arguments that load() returns.
    Returns:
      A wrapped method that accepts a single 'self' argument.
    """
    @functools.wraps(fun)
    def wrapper(self):
        contents = textwrap.dedent(fun.__doc__)
        entries, errors, options_map = load_string(contents)
        return fun(self, entries, errors, options_map)
    wrapper.__doc__ = None
    return wrapper

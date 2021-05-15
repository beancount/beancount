"""Loader code. This is the main entry point to load up a file.
"""
__copyright__ = "Copyright (C) 2013-2016  Martin Blais"
__license__ = "GNU GPLv2"

from os import path
import collections
import functools
import glob
import hashlib
import importlib
import io
import itertools
import logging
import os
import pickle
import struct
import traceback
import textwrap
import time
import warnings
from typing import Optional

from beancount.utils import misc_utils
from beancount.core import data
from beancount.parser import parser
from beancount.parser import booking
from beancount.parser import options
from beancount.parser import printer
from beancount.ops import validation
from beancount.utils import encryption
from beancount.utils import file_utils


LoadError = collections.namedtuple('LoadError', 'source message entry')


# List of default plugins to run.
DEFAULT_PLUGINS_PRE = [
    ("beancount.ops.pad", None),
    ("beancount.ops.documents", None),
    ]

DEFAULT_PLUGINS_POST = [
    ("beancount.ops.balance", None),
    ]

# A mapping of modules to warn about, to their renamed names.
RENAMED_MODULES = {}


# Filename pattern for the pickle-cache.
PICKLE_CACHE_FILENAME = '.{filename}.picklecache'

# The runtime threshold below which we don't bother creating a cache file, in
# seconds.
PICKLE_CACHE_THRESHOLD = 1.0


def load_file(filename, log_timings=None, log_errors=None, extra_validations=None,
              encoding=None):
    """Open a Beancount input file, parse it, run transformations and validate.

    Args:
      filename: The name of the file to be parsed.
      log_timings: A file object or function to write timings to,
        or None, if it should remain quiet.
      log_errors: A file object or function to write errors to,
        or None, if it should remain quiet.
      extra_validations: A list of extra validation functions to run after loading
        this list of entries.
      encoding: A string or None, the encoding to decode the input filename with.
    Returns:
      A triple of (entries, errors, option_map) where "entries" is a date-sorted
      list of entries from the file, "errors" a list of error objects generated
      while parsing and validating the file, and "options_map", a dict of the
      options parsed from the file.
    """
    filename = path.expandvars(path.expanduser(filename))
    if not path.isabs(filename):
        filename = path.normpath(path.join(os.getcwd(), filename))

    if encryption.is_encrypted_file(filename):
        # Note: Caching is not supported for encrypted files.
        entries, errors, options_map = load_encrypted_file(
            filename,
            log_timings, log_errors,
            extra_validations, False, encoding)
    else:
        entries, errors, options_map = _load_file(
            filename, log_timings,
            extra_validations, encoding)
        _log_errors(errors, log_errors)
    return entries, errors, options_map


def load_encrypted_file(filename, log_timings=None, log_errors=None, extra_validations=None,
                        dedent=False, encoding=None):
    """Load an encrypted Beancount input file.

    Args:
      filename: The name of an encrypted file to be parsed.
      log_timings: See load_string().
      log_errors: See load_string().
      extra_validations: See load_string().
      dedent: See load_string().
      encoding: See load_string().
    Returns:
      A triple of (entries, errors, option_map) where "entries" is a date-sorted
      list of entries from the file, "errors" a list of error objects generated
      while parsing and validating the file, and "options_map", a dict of the
      options parsed from the file.
    """
    contents = encryption.read_encrypted_file(filename)
    return load_string(contents,
                       log_timings=log_timings,
                       log_errors=log_errors,
                       extra_validations=extra_validations,
                       encoding=encoding)


def _log_errors(errors, log_errors):
    """Log errors, if 'log_errors' is set.

    Args:
      log_errors: A file object or function to write errors to,
        or None, if it should remain quiet.
    """
    if log_errors and errors:
        if hasattr(log_errors, 'write'):
            printer.print_errors(errors, file=log_errors)
        else:
            error_io = io.StringIO()
            printer.print_errors(errors, file=error_io)
            log_errors(error_io.getvalue())


def get_cache_filename(pattern: str, filename: str) -> str:
    """Compute the cache filename from a given pattern and the top-level filename.

    Args:
      pattern: A cache filename or pattern. If the pattern contains '{filename}' this
        will get replaced by the top-level filename. This may be absolute or relative.
      filename: The top-level filename.
    Returns:
      The resolved cache filename.
    """
    abs_filename = path.abspath(filename)
    if path.isabs(pattern):
        abs_pattern = pattern
    else:
        abs_pattern = path.join(path.dirname(abs_filename), pattern)
    return abs_pattern.format(filename=path.basename(filename))


def pickle_cache_function(cache_getter, time_threshold, function):
    """Decorate a loader function to make it loads its result from a pickle cache.

    This considers the first argument as a top-level filename and assumes the
    function to be cached returns an (entries, errors, options_map) triple. We
    use the 'include' option value in order to check whether any of the included
    files has changed. It's essentially a special case for an on-disk memoizer.
    If any of the included files are more recent than the cache, the function is
    recomputed and the cache refreshed.

    Args:
      cache_getter: A function of one argument, the top-level filename, which
        will return the name of the corresponding cache file.
      time_threshold: A float, the number of seconds below which we don't bother
        caching.
      function: A function object to decorate for caching.
    Returns:
      A decorated function which will pull its result from a cache file if
      it is available.
    """
    @functools.wraps(function)
    def wrapped(toplevel_filename, *args, **kw):
        cache_filename = cache_getter(toplevel_filename)

        # Read the cache if it exists in order to get the list of files whose
        # timestamps to check.
        exists = path.exists(cache_filename)
        if exists:
            with open(cache_filename, 'rb') as file:
                try:
                    result = pickle.load(file)
                except Exception as exc:
                    # Note: Not a big fan of doing this, but here we handle all
                    # possible exceptions because unpickling of an old or
                    # corrupted pickle file manifests as a variety of different
                    # exception types.

                    # The cache file is corrupted; ignore it and recompute.
                    logging.error("Cache file is corrupted: %s; recomputing.", exc)
                    result = None

                else:
                    # Check that the latest timestamp has not been written after the
                    # cache file.
                    entries, errors, options_map = result
                    if not needs_refresh(options_map):
                        # All timestamps are legit; cache hit.
                        return result

        # We failed; recompute the value.
        if exists:
            try:
                os.remove(cache_filename)
            except OSError as exc:
                # Warn for errors on read-only filesystems.
                logging.warning("Could not remove picklecache file %s: %s",
                                cache_filename, exc)

        time_before = time.time()
        result = function(toplevel_filename, *args, **kw)
        time_after = time.time()

        # Overwrite the cache file if the time it takes to compute it
        # justifies it.
        if time_after - time_before > time_threshold:
            try:
                with open(cache_filename, 'wb') as file:
                    pickle.dump(result, file)
            except Exception as exc:
                logging.warning("Could not write to picklecache file %s: %s",
                                cache_filename, exc)

        return result
    return wrapped


def delete_cache_function(cache_getter, function):
    """A wrapper that removes the cached filename.

    Args:
      cache_getter: A function of one argument, the top-level filename, which
        will return the name of the corresponding cache file.
      function: A function object to decorate for caching.
    Returns:
      A decorated function which will delete the cached filename, if it exists.
    """
    @functools.wraps(function)
    def wrapped(toplevel_filename, *args, **kw):
        # Delete the cache.
        cache_filename = cache_getter(toplevel_filename)
        if path.exists(cache_filename):
            os.remove(cache_filename)

        # Invoke the original function.
        return function(toplevel_filename, *args, **kw)
    return wrapped


def _uncached_load_file(filename, *args, **kw):
    """Delegate to _load. Note: This gets conditionally advised by caching below."""
    return _load([(filename, True)], *args, **kw)


def needs_refresh(options_map):
    """Predicate that returns true if at least one of the input files may have changed.

    Args:
      options_map: An options dict as per the parser.
      mtime: A modified time, to check if it covers the include files in the options_map.
    Returns:
      A boolean, true if the input is obsoleted by changes in the input files.
    """
    if options_map is None:
        return True
    input_hash = compute_input_hash(options_map['include'])
    return 'input_hash' not in options_map or input_hash != options_map['input_hash']


def compute_input_hash(filenames):
    """Compute a hash of the input data.

    Args:
      filenames: A list of input files. Order is not relevant.
    """
    md5 = hashlib.md5()
    for filename in sorted(filenames):
        md5.update(filename.encode('utf8'))
        if not path.exists(filename):
            continue
        stat = os.stat(filename)
        md5.update(struct.pack('dd', stat.st_mtime_ns, stat.st_size))
    return md5.hexdigest()


def load_string(string, log_timings=None, log_errors=None, extra_validations=None,
                dedent=False, encoding=None):

    """Open a Beancount input string, parse it, run transformations and validate.

    Args:
      string: A Beancount input string.
      log_timings: A file object or function to write timings to,
        or None, if it should remain quiet.
      log_errors: A file object or function to write errors to,
        or None, if it should remain quiet.
      extra_validations: A list of extra validation functions to run after loading
        this list of entries.
      dedent: A boolean, if set, remove the whitespace in front of the lines.
      encoding: A string or None, the encoding to decode the input string with.
    Returns:
      A triple of (entries, errors, option_map) where "entries" is a date-sorted
      list of entries from the string, "errors" a list of error objects
      generated while parsing and validating the string, and "options_map", a
      dict of the options parsed from the string.
    """
    if dedent:
        string = textwrap.dedent(string)
    entries, errors, options_map = _load([(string, False)], log_timings,
                                         extra_validations, encoding)
    _log_errors(errors, log_errors)
    return entries, errors, options_map


def _parse_recursive(sources, log_timings, encoding=None):
    """Parse Beancount input, run its transformations and validate it.

    Recursively parse a list of files or strings and their include files and
    return an aggregate of parsed directives, errors, and the top-level
    options-map. If the same file is being parsed twice, ignore it and issue an
    error.

    Args:
      sources: A list of (filename-or-string, is-filename) where the first
        element is a string, with either a filename or a string to be parsed directly,
        and the second argument is a boolean that is true if the first is a filename.
        You may provide a list of such arguments to be parsed. Filenames must be absolute
        paths.
      log_timings: A function to write timings to, or None, if it should remain quiet.
      encoding: A string or None, the encoding to decode the input filename with.
    Returns:
      A tuple of (entries, parse_errors, options_map).
    """
    assert isinstance(sources, list) and all(isinstance(el, tuple) for el in sources)

    # Current parse state.
    entries, parse_errors = [], []
    options_map = None

    # A stack of sources to be parsed.
    source_stack = list(sources)

    # A list of absolute filenames that have been parsed in the past, used to
    # detect and avoid duplicates (cycles).
    filenames_seen = set()

    with misc_utils.log_time('beancount.parser.parser', log_timings, indent=1):
        while source_stack:
            source, is_file = source_stack.pop(0)
            is_top_level = options_map is None

            # If the file is encrypted, read it in and process it as a string.
            if is_file:
                cwd = path.dirname(source)
                source_filename = source
                if encryption.is_encrypted_file(source):
                    source = encryption.read_encrypted_file(source)
                    is_file = False
            else:
                # If we're parsing a string, the CWD is the current process
                # working directory.
                cwd = os.getcwd()
                source_filename = None

            if is_file:
                # All filenames here must be absolute.
                assert path.isabs(source)
                filename = path.normpath(source)

                # Check for file previously parsed... detect duplicates.
                if filename in filenames_seen:
                    parse_errors.append(
                        LoadError(data.new_metadata("<load>", 0),
                                  'Duplicate filename parsed: "{}"'.format(filename),
                                  None))
                    continue

                # Check for a file that does not exist.
                if not path.exists(filename):
                    parse_errors.append(
                        LoadError(data.new_metadata("<load>", 0),
                                  'File "{}" does not exist'.format(filename), None))
                    continue

                # Parse a file from disk directly.
                filenames_seen.add(filename)
                with misc_utils.log_time('beancount.parser.parser.parse_file',
                                         log_timings, indent=2):
                    (src_entries,
                     src_errors,
                     src_options_map) = parser.parse_file(filename, encoding=encoding)

                cwd = path.dirname(filename)
            else:
                # Encode the contents if necessary.
                if encoding:
                    if isinstance(source, bytes):
                        source = source.decode(encoding)
                    source = source.encode('ascii', 'replace')

                # Parse a string buffer from memory.
                with misc_utils.log_time('beancount.parser.parser.parse_string',
                                         log_timings, indent=2):
                    (src_entries,
                     src_errors,
                     src_options_map) = parser.parse_string(source, source_filename)

            # Merge the entries resulting from the parsed file.
            entries.extend(src_entries)
            parse_errors.extend(src_errors)

            # We need the options from the very top file only (the very
            # first file being processed). No merging of options should
            # occur.
            if is_top_level:
                options_map = src_options_map
            else:
                aggregate_options_map(options_map, src_options_map)

            # Add includes to the list of sources to process. chdir() for glob,
            # which uses it indirectly.
            include_expanded = []
            with file_utils.chdir(cwd):
                for include_filename in src_options_map['include']:
                    matched_filenames = glob.glob(include_filename, recursive=True)
                    if matched_filenames:
                        include_expanded.extend(matched_filenames)
                    else:
                        parse_errors.append(
                            LoadError(data.new_metadata("<load>", 0),
                                      'File glob "{}" does not match any files'.format(
                                          include_filename), None))
            for include_filename in include_expanded:
                if not path.isabs(include_filename):
                    include_filename = path.join(cwd, include_filename)
                include_filename = path.normpath(include_filename)

                # Add the include filenames to be processed later.
                source_stack.append((include_filename, True))

    # Make sure we have at least a dict of valid options.
    if options_map is None:
        options_map = options.OPTIONS_DEFAULTS.copy()

    # Save the set of parsed filenames in options_map.
    options_map['include'] = sorted(filenames_seen)

    return entries, parse_errors, options_map


def aggregate_options_map(options_map, src_options_map):
    """Aggregate some of the attributes of options map.

    Args:
      options_map: The target map in which we want to aggregate attributes.
        Note: This value is mutated in-place.
      src_options_map: A source map whose values we'd like to see aggregated.
    """
    op_currencies = options_map["operating_currency"]
    for currency in src_options_map["operating_currency"]:
        if currency not in op_currencies:
            op_currencies.append(currency)
    options_map["dcontext"].update_from(src_options_map["dcontext"])


def _load(sources, log_timings, extra_validations, encoding):
    """Parse Beancount input, run its transformations and validate it.

    (This is an internal method.)
    This routine does all that is necessary to obtain a list of entries ready
    for realization and working with them. This is the principal call for of the
    scripts that load a ledger. It returns a list of entries transformed and
    ready for reporting, a list of errors, and parser's options dict.

    Args:
      sources: A list of (filename-or-string, is-filename) where the first
        element is a string, with either a filename or a string to be parsed directly,
        and the second argument is a boolean that is true if the first is a filename.
        You may provide a list of such arguments to be parsed. Filenames must be absolute
        paths.
      log_timings: A file object or function to write timings to,
        or None, if it should remain quiet.
      extra_validations: A list of extra validation functions to run after loading
        this list of entries.
      encoding: A string or None, the encoding to decode the input filename with.
    Returns:
      See load() or load_string().
    """
    assert isinstance(sources, list) and all(isinstance(el, tuple) for el in sources)

    if hasattr(log_timings, 'write'):
        log_timings = log_timings.write

    # Parse all the files recursively. Ensure that the entries are sorted before
    # running any processes on them.
    with misc_utils.log_time('parse', log_timings, indent=1):
        entries, parse_errors, options_map = _parse_recursive(
            sources, log_timings, encoding)
        entries.sort(key=data.entry_sortkey)

    # Run interpolation on incomplete entries.
    with misc_utils.log_time('booking', log_timings, indent=1):
        entries, balance_errors = booking.book(entries, options_map)
        parse_errors.extend(balance_errors)

    # Transform the entries.
    with misc_utils.log_time('run_transformations', log_timings, indent=1):
        entries, errors = run_transformations(entries, parse_errors, options_map,
                                              log_timings)

    # Validate the list of entries.
    with misc_utils.log_time('beancount.ops.validate', log_timings, indent=1):
        valid_errors = validation.validate(entries, options_map, log_timings,
                                           extra_validations)
        errors.extend(valid_errors)

        # Note: We could go hardcore here and further verify that the entries
        # haven't been modified by user-provided validation routines, by
        # comparing hashes before and after. Not needed for now.

    # Compute the input hash.
    options_map['input_hash'] = compute_input_hash(options_map['include'])

    return entries, errors, options_map


def run_transformations(entries, parse_errors, options_map, log_timings):
    """Run the various transformations on the entries.

    This is where entries are being synthesized, checked, plugins are run, etc.

    Args:
      entries: A list of directives as read from the parser.
      parse_errors: A list of errors so far.
      options_map: An options dict as read from the parser.
      log_timings: A function to write timing log entries to, or None, if it
        should be quiet.
    Returns:
      A list of modified entries, and a list of errors, also possibly modified.
    """
    # A list of errors to extend (make a copy to avoid modifying the input).
    errors = list(parse_errors)

    # Process the plugins.
    if options_map['plugin_processing_mode'] == 'raw':
        plugins_iter = options_map["plugin"]
    elif options_map['plugin_processing_mode'] == 'default':
        plugins_iter = itertools.chain(DEFAULT_PLUGINS_PRE,
                                       options_map["plugin"],
                                       DEFAULT_PLUGINS_POST)
    else:
        assert "Invalid value for plugin_processing_mode: {}".format(
            options_map['plugin_processing_mode'])

    for plugin_name, plugin_config in plugins_iter:

        # Issue a warning on a renamed module.
        renamed_name = RENAMED_MODULES.get(plugin_name, None)
        if renamed_name:
            warnings.warn("Deprecation notice: Module '{}' has been renamed to '{}'; "
                          "please adjust your plugin directive.".format(
                              plugin_name, renamed_name))
            plugin_name = renamed_name

        # Try to import the module.
        #
        # Note: We intercept import errors and continue but let other plugin
        # import time exceptions fail a run, by choice.
        try:
            module = importlib.import_module(plugin_name)
            if not hasattr(module, '__plugins__'):
                continue
        except ImportError:
            # Upon failure, just issue an error.
            formatted_traceback = traceback.format_exc().replace("\n", "\n  ")
            errors.append(LoadError(data.new_metadata("<load>", 0),
                                    'Error importing "{}": {}'.format(
                                        plugin_name, formatted_traceback), None))
            continue

        # Apply it.
        with misc_utils.log_time(plugin_name, log_timings, indent=2):
            # Run each transformer function in the plugin.
            for function_name in module.__plugins__:
                if isinstance(function_name, str):
                    # Support plugin functions provided by name.
                    callback = getattr(module, function_name)
                else:
                    # Support function types directly, not just names.
                    callback = function_name

                # Provide arguments if config is provided.
                # TODO(blais): Make this consistent in v3, not conditional.
                args = () if plugin_config is None else (plugin_config,)

                # Catch all exceptions raised in running the plugin, except exits.
                try:
                    entries, plugin_errors = callback(entries, options_map, *args)
                    errors.extend(plugin_errors)
                except Exception as exc:
                    # Allow the user to exit in a plugin.
                    if isinstance(exc, SystemExit):
                        raise

                    # Upon failure, just issue an error.
                    formatted_traceback = traceback.format_exc().replace("\n", "\n  ")
                    errors.append(LoadError(data.new_metadata("<load>", 0),
                                            'Error applying plugin "{}": {}'.format(
                                                plugin_name, formatted_traceback), None))
                    continue

            # Ensure that the entries are sorted. Don't trust the plugins
            # themselves.
            entries.sort(key=data.entry_sortkey)

    return entries, errors


def combine_plugins(*plugin_modules):
    """Combine the plugins from the given plugin modules.

    This is used to create plugins of plugins.
    Args:
      *plugins_modules: A sequence of module objects.
    Returns:
      A list that can be assigned to the new module's __plugins__ attribute.
    """
    modules = []
    for module in plugin_modules:
        modules.extend([getattr(module, name)
                        for name in module.__plugins__])
    return modules


def load_doc(expect_errors=False):
    """A factory of decorators that loads the docstring and calls the function with entries.

    This is an incredibly convenient tool to write lots of tests. Write a
    unittest using the standard TestCase class and put the input entries in the
    function's docstring.

    Args:
      expect_errors: A boolean or None, with the following semantics,
        True: Expect errors and fail if there are none.
        False: Expect no errors and fail if there are some.
        None: Do nothing, no check.
    Returns:
      A wrapped method that accepts a single 'self' argument.
    """
    def decorator(fun):
        """A decorator that parses the function's docstring as an argument.

        Args:
          fun: A callable method, that accepts the three return arguments that
              load() returns.
        Returns:
          A decorated test function.
        """
        @functools.wraps(fun)
        def wrapper(self):
            entries, errors, options_map = load_string(fun.__doc__, dedent=True)

            if expect_errors is not None:
                if expect_errors is False and errors:
                    oss = io.StringIO()
                    printer.print_errors(errors, file=oss)
                    self.fail("Unexpected errors found:\n{}".format(oss.getvalue()))
                elif expect_errors is True and not errors:
                    self.fail("Expected errors, none found:")

            # Note: Even if we expected no errors, we call this function with an
            # empty 'errors' list. This is so that the interface does not change
            # based on the arguments to the decorator, which would be somewhat
            # ugly and which would require explanation.
            return fun(self, entries, errors, options_map)

        wrapper.__input__ = wrapper.__doc__
        wrapper.__doc__ = None
        return wrapper

    return decorator


def initialize(use_cache: bool, cache_filename: Optional[str] = None):
    """Initialize the loader."""

    # Unless an environment variable disables it, use the pickle load cache
    # automatically. Note that this works across all Python programs running the
    # loader which is why it's located here.
    # pylint: disable=invalid-name
    global _load_file

    # Make a function to compute the cache filename.
    cache_pattern = (cache_filename or
                     os.getenv('BEANCOUNT_LOAD_CACHE_FILENAME') or
                     PICKLE_CACHE_FILENAME)
    cache_getter = functools.partial(get_cache_filename, cache_pattern)

    if use_cache:
        _load_file = pickle_cache_function(cache_getter, PICKLE_CACHE_THRESHOLD,
                                           _uncached_load_file)
    else:
        if cache_filename is not None:
            logging.warning("Cache disabled; "
                            "Explicitly overridden cache filename %s will be ignored.",
                            cache_filename)
        _load_file = delete_cache_function(cache_getter,
                                           _uncached_load_file)


# Default is to use the cache every time.
initialize(os.getenv('BEANCOUNT_DISABLE_LOAD_CACHE') is None)

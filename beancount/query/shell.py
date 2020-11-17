"""An interactive command-line shell interpreter for the Beancount Query Language.
"""
__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import atexit
import cmd
import codecs
import io
import logging
import os
import re
import sys
import shlex
import textwrap
import traceback
from os import path

try:
    import readline
except ImportError:
    readline = None

from beancount.query import query_parser
from beancount.query import query_compile
from beancount.query import query_env
from beancount.query import query_execute
from beancount.query import query_render
from beancount.query import numberify
from beancount.parser import printer
from beancount.core import data
from beancount.utils import misc_utils
from beancount.utils import pager
from beancount.parser import version
from beancount import loader


HISTORY_FILENAME = "~/.bean-shell-history"


def load_history(filename):
    """Load the shell's past history.

    Args:
      filename: A string, the name of the file containing the shell history.
    """
    readline.parse_and_bind("tab:complete")
    if hasattr(readline, "read_history_file"):
        try:
            readline.read_history_file(filename)
        except IOError:
            # Don't error on absent file.
            pass
        atexit.register(save_history, filename)


def save_history(filename):
    """Save the shell history. This should be invoked on exit.

    Args:
      filename: A string, the name of the file to save the history to.
    """
    readline.write_history_file(filename)


def get_history(max_entries):
    """Return the history in the readline buffer.

    Args:
      max_entries: An integer, the maximum number of entries to return.
    Returns:
      A list of string, the previous history of commands.
    """
    num_entries = readline.get_current_history_length()
    assert num_entries >= 0
    start = max(0, num_entries - max_entries)
    return [readline.get_history_item(index+1)
            for index in range(start, num_entries)]


def convert_bool(string):
    """Convert a string to a boolean.

    Args:
      string: A string representing a boolean.
    Returns:
      The corresponding boolean.
    """
    return not string.lower() in ('f', 'false', '0')


class DispatchingShell(cmd.Cmd):
    """A usable convenient shell for interpreting commands, with history."""

    # The maximum number of entries.
    max_entries = 64

    # Header for parsed commands.
    doc_header = "Shell utility commands (type help <topic>):"
    misc_header = "Beancount query commands:"

    def __init__(self, is_interactive, parser, outfile, default_format, do_numberify):
        """Create a shell with history.

        Args:
          is_interactive: A boolean, true if this serves an interactive tty.
          parser: A command parser.
          outfile: An output file object to write communications to.
          default_format: A string, the default output format.
        """
        super().__init__()
        if is_interactive and readline is not None:
            load_history(path.expanduser(HISTORY_FILENAME))
        self.is_interactive = is_interactive
        self.parser = parser
        self.initialize_vars(default_format, do_numberify)
        self.add_help()
        self.outfile = outfile

    def initialize_vars(self, default_format, do_numberify):
        """Initialize the setting variables of the interactive shell."""
        self.vars_types = {
            'pager': str,
            'format': str,
            'boxed': convert_bool,
            'spaced': convert_bool,
            'expand': convert_bool,
            'numberify': convert_bool,
            }
        self.vars = {
            'pager': os.environ.get('PAGER', None),
            'format': default_format,
            'boxed': False,
            'spaced': False,
            'expand': False,
            'numberify': do_numberify,
            }

    def add_help(self):
        "Attach help functions for each of the parsed token handlers."
        for attrname, func in list(self.__class__.__dict__.items()):
            match = re.match('on_(.*)', attrname)
            if not match:
                continue
            command_name = match.group(1)
            setattr(self.__class__, 'help_{}'.format(command_name.lower()),
                    lambda _, fun=func: print(textwrap.dedent(fun.__doc__).strip(),
                                              file=self.outfile))

    def get_pager(self):
        """Create and return a context manager to write to, a pager subprocess if required.

        Returns:
          A pair of a file object to write to, and a pipe object to wait on (or
        None if not necessary to wait).
        """
        if self.is_interactive:
            return pager.ConditionalPager(self.vars.get('pager', None),
                                          minlines=misc_utils.get_screen_height())
        else:
            file = (codecs.getwriter("utf-8")(sys.stdout.buffer)
                    if hasattr(sys.stdout, 'buffer') else
                    sys.stdout)
            return pager.flush_only(file)

    def cmdloop(self):
        """Override cmdloop to handle keyboard interrupts."""
        while True:
            try:
                super().cmdloop()
                break
            except KeyboardInterrupt:
                print('\n(Interrupted)', file=self.outfile)

    def do_help(self, command):
        """Strip superfluous semicolon."""
        super().do_help(command.rstrip('; \t'))

    def do_history(self, _):
        "Print the command-line history statement."
        if readline is not None:
            for index, line in enumerate(get_history(self.max_entries)):
                print(line, file=self.outfile)

    def do_clear(self, _):
        "Clear the history."
        readline.clear_history()

    def do_set(self, line):
        "Get/set shell settings variables."
        if not line:
            for varname, value in sorted(self.vars.items()):
                print('{}: {}'.format(varname, value), file=self.outfile)
        else:
            components = shlex.split(line)
            varname = components[0]
            if len(components) == 1:
                try:
                    value = self.vars[varname]
                    print('{}: {}'.format(varname, value), file=self.outfile)
                except KeyError:
                    print("Variable '{}' does not exist.".format(varname),
                          file=self.outfile)
            elif len(components) == 2:
                value = components[1]
                try:
                    converted_value = self.vars_types[varname](value)
                    self.vars[varname] = converted_value
                    print('{}: {}'.format(varname, converted_value), file=self.outfile)
                except KeyError:
                    print("Variable '{}' does not exist.".format(varname),
                          file=self.outfile)
            else:
                print("Invalid number of arguments.", file=self.outfile)

    def do_lex(self, line):
        "Just run the lexer on the following command and print the output."
        try:
            self.parser.tokenize(line)
        except query_parser.ParseError as exc:
            print(exc, file=self.outfile)

    do_tokenize = do_lex

    def do_parse(self, line):
        "Just run the parser on the following command and print the output."
        print("INPUT: {}".format(repr(line)), file=self.outfile)
        try:
            statement = self.parser.parse(line, True)
            print(statement, file=self.outfile)
        except (query_parser.ParseError,
                query_compile.CompilationError) as exc:
            print(exc, file=self.outfile)
        except Exception as exc:
            traceback.print_exc(file=self.outfile)

    def dispatch(self, statement):
        """Dispatch the given statement to a suitable method.

        Args:
          statement: An instance provided by the parser.
        Returns:
          Whatever the invoked method happens to return.
        """
        try:
            method = getattr(self, 'on_{}'.format(type(statement).__name__))
        except AttributeError:
            print("Internal error: statement '{}' is unsupported.".format(statement),
                  file=self.outfile)
        else:
            return method(statement)

    def default(self, line):
        """Default handling of lines which aren't recognized as native shell commands.

        Args:
          line: The string to be parsed.
        """
        self.run_parser(line)

    def run_parser(self, line, default_close_date=None):
        """Handle statements via our parser instance and dispatch to appropriate methods.

        Args:
          line: The string to be parsed.
          default_close_date: A datetimed.date instance, the default close date.
        """
        try:
            statement = self.parser.parse(line,
                                          default_close_date=default_close_date)
            self.dispatch(statement)
        except query_parser.ParseError as exc:
            print(exc, file=self.outfile)
        except Exception as exc:
            traceback.print_exc(file=self.outfile)

    def emptyline(self):
        """Do nothing on an empty line."""

    def exit(self, _):
        """Exit the parser."""
        print('exit', file=self.outfile)
        return 1

    # Commands to exit.
    do_exit = exit
    do_quit = exit
    do_EOF = exit


class BQLShell(DispatchingShell):
    """An interactive shell interpreter for the Beancount query language.
    """
    prompt = 'beancount> '

    def __init__(self, is_interactive, loadfun, outfile,
                 default_format='text', do_numberify=False):
        super().__init__(is_interactive, query_parser.Parser(), outfile,
                         default_format, do_numberify)

        self.loadfun = loadfun
        self.entries = None
        self.errors = None
        self.options_map = None

        self.env_targets = query_env.TargetsEnvironment()
        self.env_entries = query_env.FilterEntriesEnvironment()
        self.env_postings = query_env.FilterPostingsEnvironment()

    def on_Reload(self, unused_statement=None):
        """
        Reload the input file without restarting the shell.
        """
        self.entries, self.errors, self.options_map = self.loadfun()
        if self.is_interactive:
            print_statistics(self.entries, self.options_map, self.outfile)

    def on_Errors(self, errors_statement):
        """
        Print the errors that occurred during parsing.
        """
        if self.errors:
            printer.print_errors(self.errors)
        else:
            print('(No errors)', file=self.outfile)

    def on_Print(self, print_stmt):
        """
        Print entries in Beancount format.

        The general form of a PRINT statement includes an SQL-like FROM
        selector:

           PRINT [FROM <from_expr> ...]

        Where:

          from_expr: A logical expression that matches on the attributes of
            the directives. See SELECT command for details (this FROM expression
            supports all the same expressions including its OPEN, CLOSE and
            CLEAR operations).

        """
        # Compile the print statement.
        try:
            c_print = query_compile.compile(print_stmt,
                                            self.env_targets,
                                            self.env_postings,
                                            self.env_entries)
        except query_compile.CompilationError as exc:
            print('ERROR: {}.'.format(str(exc).rstrip('.')), file=self.outfile)
            return

        if self.outfile is sys.stdout:
            query_execute.execute_print(c_print, self.entries, self.options_map,
                                        file=self.outfile)
        else:
            with self.get_pager() as file:
                query_execute.execute_print(c_print, self.entries, self.options_map, file)

    def on_Select(self, statement):
        """
        Extract data from a query on the postings.

        The general form of a SELECT statement loosely follows SQL syntax, with
        some mild and idiomatic extensions:

           SELECT [DISTINCT] [<targets>|*]
           [FROM <from_expr> [OPEN ON <date>] [CLOSE [ON <date>]] [CLEAR]]
           [WHERE <where_expr>]
           [GROUP BY <groups>]
           [ORDER BY <groups> [ASC|DESC]]
           [LIMIT num]

        Where:

          targets: A list of desired output attributes from the postings, and
            expressions on them. Some of the attributes of the parent transaction
            directive are made available in this context as well. Simple functions
            (that return a single value per row) and aggregation functions (that
            return a single value per group) are available. For the complete
            list of supported columns and functions, see help on "targets".
            You can also provide a wildcard here, which will select a reasonable
            default set of columns for rendering a journal.

          from_expr: A logical expression that matches on the attributes of
            the directives (not postings). This allows you to select a subset of
            transactions, so the accounting equation is respected for balance
            reports. For the complete list of supported columns and functions,
            see help on "from".

          where_expr: A logical expression that matches on the attributes of
            postings. The available columns are similar to those in the targets
            clause, without the aggregation functions.

          OPEN clause: replace all the transactions before the given date by
            summarizing entries and transfer Income and Expenses balances to
            Equity.

          CLOSE clause: Remove all the transactions after the given date and

          CLEAR: Transfer final Income and Expenses balances to Equity.

        """
        # Compile the SELECT statement.
        try:
            c_query = query_compile.compile(statement,
                                            self.env_targets,
                                            self.env_postings,
                                            self.env_entries)
        except query_compile.CompilationError as exc:
            print('ERROR: {}.'.format(str(exc).rstrip('.')), file=self.outfile)
            return

        # Execute it to obtain the result rows.
        rtypes, rrows = query_execute.execute_query(c_query,
                                                    self.entries,
                                                    self.options_map)

        # Output the resulting rows.
        if not rrows:
            print("(empty)", file=self.outfile)
        else:
            output_format = self.vars['format']
            if output_format == 'text':
                kwds = dict(boxed=self.vars['boxed'],
                            spaced=self.vars['spaced'],
                            expand=self.vars['expand'])
                if self.outfile is sys.stdout:
                    with self.get_pager() as file:
                        query_render.render_text(rtypes, rrows,
                                                 self.options_map['dcontext'],
                                                 file,
                                                 **kwds)
                else:
                    query_render.render_text(rtypes, rrows,
                                             self.options_map['dcontext'],
                                             self.outfile,
                                             **kwds)

            elif output_format == 'csv':
                # Numberify CSV output if requested.
                if self.vars['numberify']:
                    dformat = self.options_map['dcontext'].build()
                    rtypes, rrows = numberify.numberify_results(rtypes, rrows, dformat)

                query_render.render_csv(rtypes, rrows,
                                        self.options_map['dcontext'],
                                        self.outfile,
                                        expand=self.vars['expand'])

            else:
                assert output_format not in _SUPPORTED_FORMATS
                print("Unsupported output format: '{}'.".format(output_format),
                      file=self.outfile)


    def on_Journal(self, journal):
        """
        Select a journal of some subset of postings. This command is a
        convenience and converts into an equivalent Select statement, designed
        to extract the most sensible list of columns for the register of a list
        of entries as a table.

        The general form of a JOURNAL statement loosely follows SQL syntax:

           JOURNAL <account-regexp> [FROM_CLAUSE]

        See the SELECT query help for more details on the FROM clause.
        """
        return self.on_Select(journal)

    def on_Balances(self, balance):
        """
        Select balances of some subset of postings. This command is a
        convenience and converts into an equivalent Select statement, designed
        to extract the most sensible list of columns for the register of a list
        of entries as a table.

        The general form of a JOURNAL statement loosely follows SQL syntax:

           BALANCE [FROM_CLAUSE]

        See the SELECT query help for more details on the FROM clause.
        """
        return self.on_Select(balance)

    def on_Explain(self, explain):
        """
        Compile and print a compiled statement for debugging.
        """
        pr = lambda *args: print(*args, file=self.outfile)
        pr("Parsed statement:")
        pr("  {}".format(explain.statement))
        pr()

        # Compile the select statement and print it uot.
        try:
            query = query_compile.compile(explain.statement,
                                          self.env_targets,
                                          self.env_postings,
                                          self.env_entries)
        except query_compile.CompilationError as exc:
            pr(str(exc).rstrip('.'))
            return

        pr("Compiled query:")
        pr("  {}".format(query))
        pr()
        pr("Targets:")
        for c_target in query.c_targets:
            pr("  '{}'{}: {}".format(
                c_target.name or '(invisible)',
                ' (aggregate)' if query_compile.is_aggregate(c_target.c_expr) else '',
                c_target.c_expr.dtype.__name__))
        pr()

    def on_RunCustom(self, run_stmt):
        """
        Run a custom query instead of a SQL command.

           RUN <custom-query-name>

        Where:

          custom-query-name: Should be the name of a custom query to be defined
            in the Beancount input file.

        """
        custom_query_map = create_custom_query_map(self.entries)
        name = run_stmt.query_name
        if name is None:
            # List the available queries.
            for name in sorted(custom_query_map):
                print(name)
        elif name == "*":
            for name, query in sorted(custom_query_map.items()):
                print('{}:'.format(name))
                self.run_parser(query.query_string, default_close_date=query.date)
                print()
                print()
        else:
            query = None
            if name in custom_query_map:
                query = custom_query_map[name]
            else:  # lookup best query match using name as prefix
                queries = [q for q in custom_query_map if q.startswith(name)]
                if len(queries) == 1:
                    name = queries[0]
                    query = custom_query_map[name]
            if query:
                statement = self.parser.parse(query.query_string)
                self.dispatch(statement)
            else:
                print("ERROR: Query '{}' not found".format(name))

    def help_targets(self):
        template = textwrap.dedent("""

          The list of comma-separated target expressions may consist of columns,
          simple functions and aggregate functions. If you use any aggregate
          function, you must also provide a GROUP-BY clause.

          Available columns:
          {columns}

          Simple functions:
          {functions}

          Aggregate functions:
          {aggregates}

        """).strip()
        print(template.format(**generate_env_attribute_list(self.env_targets)),
              file=self.outfile)

    def help_from(self):
        template = textwrap.dedent("""

          A logical expression that consist of columns on directives (mostly
          transactions) and simple functions.

          Available columns:
          {columns}

          Simple functions:
          {functions}

        """).strip()
        print(template.format(**generate_env_attribute_list(self.env_entries)),
              file=self.outfile)

    def help_where(self):
        template = textwrap.dedent("""

          A logical expression that consist of columns on postings and simple
          functions.

          Available columns:
          {columns}

          Simple functions:
          {functions}

        """).strip()
        print(template.format(**generate_env_attribute_list(self.env_postings)),
              file=self.outfile)

    def help_attributes(self):
        template = textwrap.dedent("""

          The attribute names on postings and directives equivalent to the names
          of columns that we make available for query.

          Entries:
          {entry_attributes}

          Postings:
          {posting_attributes}

        """).strip()

        entry_pairs = sorted(
            (getattr(column_cls, '__equivalent__', '-'), name)
            for name, column_cls in sorted(self.env_entries.columns.items()))

        posting_pairs = sorted(
            (getattr(column_cls, '__equivalent__', '-'), name)
            for name, column_cls in sorted(self.env_postings.columns.items()))

        # pylint: disable=possibly-unused-variable
        entry_attributes = ''.join(
            "  {:40}: {}\n".format(*pair) for pair in entry_pairs)
        posting_attributes = ''.join(
            "  {:40}: {}\n".format(*pair) for pair in posting_pairs)
        print(template.format(**locals()), file=self.outfile)


def generate_env_attribute_list(env):
    """Generate a dictionary of rendered attribute lists for help.

    Args:
      env: An instance of an environment.
    Returns:
      A dict with keys 'columns', 'functions' and 'aggregates' to rendered
      and formatted strings.
    """
    wrapper = textwrap.TextWrapper(initial_indent='  ',
                                   subsequent_indent='    ',
                                   drop_whitespace=True,
                                   width=80)

    str_columns = generate_env_attributes(
        wrapper, env.columns)
    str_simple = generate_env_attributes(
        wrapper, env.functions,
        lambda node: not issubclass(node, query_compile.EvalAggregator))
    str_aggregate = generate_env_attributes(
        wrapper, env.functions,
        lambda node: issubclass(node, query_compile.EvalAggregator))

    return dict(columns=str_columns,
                functions=str_simple,
                aggregates=str_aggregate)


def generate_env_attributes(wrapper, field_dict, filter_pred=None):
    """Generate a string of all the help functions of the attributes.

    Args:
      wrapper: A TextWrapper instance to format the paragraphs.
      field_dict: A dict of the field-names to the node instances, fetch from an
        environment.
      filter_pred: A predicate to filter the desired columns. This is applied to
        the evaluator node instances.
    Returns:
      A formatted multiline string, ready for insertion in a help text.
    """
    # Expand the name if its key has argument types.
    #
    # FIXME: Render the __intypes__ here nicely instead of the key.
    flat_items = []
    for name, column_cls in field_dict.items():
        if isinstance(name, tuple):
            name = name[0]

        if issubclass(column_cls, query_compile.EvalFunction):
            name = name.upper()
            args = []
            for dtypes in column_cls.__intypes__:
                if isinstance(dtypes, (tuple, list)):
                    arg = '|'.join(dtype.__name__ for dtype in dtypes)
                else:
                    arg = dtypes.__name__
                args.append(arg)
            name = "{}({})".format(name, ','.join(args))

        flat_items.append((name, column_cls))

    # Render each of the attributes.
    oss = io.StringIO()
    for name, column_cls in sorted(flat_items):
        if filter_pred and not filter_pred(column_cls):
            continue
        docstring = column_cls.__doc__ or "[See class {}]".format(column_cls.__name__)
        if issubclass(column_cls, query_compile.EvalColumn):
            docstring += " Type: {}.".format(column_cls().dtype.__name__)
            # if hasattr(column_cls, '__equivalent__'):
            #     docstring += " Attribute:{}.".format(column_cls.__equivalent__)

        text = re.sub('[ \t]+', ' ', docstring.strip().replace('\n', ' '))
        doc = "'{}': {}".format(name, text)
        oss.write(wrapper.fill(doc))
        oss.write('\n')

    return oss.getvalue().rstrip()


def summary_statistics(entries):
    """Calculate basic summary statistics to output a brief welcome message.

    Args:
      entries: A list of directives.
    Returns:
      A tuple of three integers, the total number of directives parsed, the total number
      of transactions and the total number of postings there in.
    """
    num_directives = len(entries)
    num_transactions = 0
    num_postings = 0
    for entry in entries:
        if isinstance(entry, data.Transaction):
            num_transactions += 1
            num_postings += len(entry.postings)
    return (num_directives, num_transactions, num_postings)


def print_statistics(entries, options_map, outfile):
    """Print summary statistics to stdout.

    Args:
      entries: A list of directives.
      options_map: An options map. as produced by the parser.
      outfile: A file object to write to.
    """
    num_directives, num_transactions, num_postings = summary_statistics(entries)
    if 'title' in options_map:
        print('Input file: "{}"'.format(options_map['title']), file=outfile)
    print("Ready with {} directives ({} postings in {} transactions).".format(
        num_directives, num_postings, num_transactions),
          file=outfile)


def create_custom_query_map(entries):
    """Extract a mapping of the custom queries from the list of entries.

    Args:
      entries: A list of entries.
    Returns:
      A map of query-name strings to Query directives.
    """
    query_map = {}
    for entry in entries:
        if not isinstance(entry, data.Query):
            continue
        if entry.name in query_map:
            logging.warning("Duplicate query: %s", entry.name)
        query_map[entry.name] = entry
    return query_map


_SUPPORTED_FORMATS = ('text', 'csv')


def main():
    parser = version.ArgumentParser(description=__doc__)

    parser.add_argument('-f', '--format', action='store', default=_SUPPORTED_FORMATS[0],
                        choices=_SUPPORTED_FORMATS, # 'html', 'htmldiv', 'beancount', 'xls',
                        help="Output format.")

    parser.add_argument('-m', '--numberify', action='store_true', default=False,
                        help="Numberify the output, removing the currencies.")

    parser.add_argument('-o', '--output', action='store',
                        help=("Output filename. If not specified, the output goes "
                              "to stdout. The filename is inspected to select a "
                              "sensible default format, if one is not requested."))

    parser.add_argument('-q', '--no-errors', action='store_true',
                        help='Do not report errors')

    parser.add_argument('filename', metavar='FILENAME.beancount',
                        help='The Beancount input filename to load')

    parser.add_argument('query', nargs='*',
                        help='A query to run directly')

    args = parser.parse_args()

    # Parse the input file.
    def load():
        errors_file = None if args.no_errors else sys.stderr
        with misc_utils.log_time('beancount.loader (total)', logging.info):
            return loader.load_file(args.filename,
                                    log_timings=logging.info,
                                    log_errors=errors_file)

    # Create a receiver for output.
    outfile = sys.stdout if args.output is None else open(args.output, 'w')

    # Create the shell.
    is_interactive = sys.stdin.isatty() and not args.query
    shell_obj = BQLShell(is_interactive, load, outfile, args.format, args.numberify)
    shell_obj.on_Reload()

    # Run interactively if we're a TTY and no query is supplied.
    if is_interactive:
        try:
            shell_obj.cmdloop()
        except KeyboardInterrupt:
            print('\nExit')
    else:
        # Run in batch mode (Non-interactive).
        if args.query:
            # We have a query to run.
            query = ' '.join(args.query)
        else:
            # If we have no query and we're not a TTY, read the BQL command from
            # standard input.
            query = sys.stdin.read()

        shell_obj.onecmd(query)

    return 0


if __name__ == '__main__':
    main()

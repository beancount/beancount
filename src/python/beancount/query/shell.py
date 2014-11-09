"""An interactive command-line shell interpreter for the Beancount Query Language.
"""
import os
import sys
import cmd
import io
import readline
import re
import atexit
import textwrap
import traceback
from os import path

from beancount.query import query_parser
from beancount.query import query_compile
from beancount.query import query_env
from beancount.query import query_execute
from beancount.query import query_render
from beancount.parser import printer
from beancount.core import data
from beancount.utils import misc_utils
from beancount.utils import pager


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
    return [readline.get_history_item(index+1)
            for index in range(min(num_entries, max_entries))]


class DispatchingShell(cmd.Cmd):
    """A usable convenient shell for interpreting commands, with history."""

    # The maximum number of entries.
    max_entries = 64

    # Header for parsed commands.
    doc_header = "Shell utility commands (type help <topic>):"
    misc_header = "Beancount query commands:"

    def __init__(self, parser):
        super().__init__()
        load_history(path.expanduser(HISTORY_FILENAME))
        self.parser = parser
        self.initialize_vars()
        self.add_help()

    def initialize_vars(self):
        """Initialize the setting variables of the interactive shell."""
        self.vars = {
            'pager': os.environ.get('PAGER', None)
            }

    def add_help(self):
        "Attach help functions for each of the parsed token handlers."
        for attrname, func in list(self.__class__.__dict__.items()):
            mo = re.match('on_(.*)', attrname)
            if not mo:
                continue
            command_name = mo.group(1)
            setattr(self.__class__, 'help_{}'.format(command_name.lower()),
                    lambda _, fun=func: print(textwrap.dedent(fun.__doc__).strip()))

    def get_pager(self):
        """Create and return a context manager to write to, a pager subprocess if required.

        Returns:
          A pair of a file object to write to, and a pipe object to wait on (or
        None if not necessary to wait).
        """
        return pager.ConditionalPager(self.vars.get('pager', None),
                                      minlines=misc_utils.get_screen_height())

    def cmdloop(self):
        """Override cmdloop to handle keyboard interrupts."""
        while True:
            try:
                super().cmdloop()
                break
            except KeyboardInterrupt:
                print('\n(Interrupted)')

    def do_history(self, _):
        "Print the command-line history statement."
        for index, line in enumerate(get_history(self.max_entries)):
            print(line)

    def do_clear(self, _):
        "Clear the history."
        readline.clear_history()

    def do_lex(self, line):
        "Just run the lexer on the following command and print the output."
        try:
            self.parser.tokenize(line)
        except query_parser.ParseError as exc:
            print(exc)

    do_tokenize = do_lex

    def do_parse(self, line):
        "Just run the parser on the following command and print the output."
        print("INPUT: {}".format(repr(line)))
        try:
            statement = self.parser.parse(line, True)
            print(statement)
        except (query_parser.ParseError,
                query_compile.CompilationError) as exc:
            print(exc)
        except Exception as exc:
            traceback.print_exc()

    def dispatch(self, statement):
        """Dispatch the given statement to a suitable method.

        Args:
          statement: An instance provided by the parser.
        Returns:
          Whatever the invoked method happens to return.
        """
        method = getattr(self, 'do_{}'.format(type(statement).__name__.lower()))
        return method(statement)

    def default(self, line):
        """Handle statements via our parser instance and dispatch to appropriate methods.

        Args:
          line: The string to be parsed.
        """
        try:
            statement = self.parser.parse(line)
            self.dispatch(statement)
        except query_parser.ParseError as exc:
            print(exc)
        except Exception as exc:
            traceback.print_exc()

    def emptyline(self):
        """Do nothing on an empty line."""
        pass

    def exit(self, _):
        """Exit the parser."""
        print('exit')
        return 1

    # Commands to exit.
    do_exit = exit
    do_quit = exit
    do_EOF = exit


class BQLShell(DispatchingShell):
    """An interactive shell interpreter for the Beancount query language.
    """
    prompt = 'beancount> '

    def __init__(self, entries, errors, options_map):
        super().__init__(query_parser.Parser())

        self.entries = entries
        self.errors = errors
        self.options_map = options_map

        self.env_targets = query_env.TargetsEnvironment()
        self.env_entries = query_env.FilterEntriesEnvironment()
        self.env_postings = query_env.FilterPostingsEnvironment()

    def on_Errors(self, errors_statement):
        """
        Print the errors that occurred during parsing.
        """
        if self.errors:
            printer.print_errors(self.errors)
        else:
            print('(No errors)')

    def on_Print(self, print_stmt):
        """
        Print entries in Beancount format.
        """
        # Compile the print statement.
        c_from = query_compile.compile_from(print_stmt.from_clause, self.env_entries)
        c_print = query_parser.Print(c_from)

        with self.get_pager() as file:
            query_execute.execute_print(c_print, self.entries, self.options_map, file)

    def on_Select(self, select):
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
        # Compile the select statement.
        try:
            query = query_compile.compile_select(select,
                                                 self.env_targets,
                                                 self.env_postings,
                                                 self.env_entries)
        except query_compile.CompilationError as exc:
            print('ERROR: {}.'.format(str(exc).rstrip('.')))
            return

        # Execute it to obtain the result rows.
        result_types, result_rows = query_execute.execute_query(query,
                                                                self.entries,
                                                                self.options_map)

        # Output the resulting rows.
        if not result_rows:
            print("(empty)")
        else:
            with self.get_pager() as file:
                query_render.render_text(result_types, result_rows, file)

    def on_Journal(self, journal):
        """
        Select a journal of some subset of postings. This command is a
        convenience

        """
        raise NotImplementedError

    def on_Balances(self, balances):
        """
        Print balances on some aggregations of postings.
        """
        raise NotImplementedError

    def on_Explain(self, explain):
        """
        Compile and print a compiled statement for debugging.
        """
        if isinstance(explain.statement, query_parser.Select):
            # Compile the select statement and print it uot.
            query = query_compile.compile_select(explain.statement,
                                                 self.env_targets,
                                                 self.env_postings,
                                                 self.env_entries)
            print("Compiled query:")
            print("  {}".format(query))
            print()
            print("Targets:")
            for c_target in query.c_targets:
                print("  '{}'{}: {}".format(
                    c_target.name or '(invisible)',
                    ' (aggregate)' if query_compile.is_aggregate(c_target.c_expr) else '',
                    c_target.c_expr.dtype.__name__))
            print()
        else:
            print("(Unsupported statement)")

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
        print(template.format(**generate_env_attribute_list(self.env_targets)))

    def help_from(self):
        template = textwrap.dedent("""

          A logical expression that consist of columns on directives (mostly
          transactions) and simple functions.

          Available columns:
          {columns}

          Simple functions:
          {functions}

        """).strip()
        print(template.format(**generate_env_attribute_list(self.env_entries)))

    def help_where(self):
        template = textwrap.dedent("""

          A logical expression that consist of columns on postings and simple
          functions.

          Available columns:
          {columns}

          Simple functions:
          {functions}

        """).strip()
        print(template.format(**generate_env_attribute_list(self.env_entries)))


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
    flat_items = [(("{}({})".format(name[0], ','.join(cls.__name__ for cls in name[1:]))
                    if isinstance(name, tuple)
                    else name), column_cls)
                  for name, column_cls in field_dict.items()]

    # Render each of the attributes.
    oss = io.StringIO()
    for name, column_cls in sorted(flat_items):
        if filter_pred and not filter_pred(column_cls):
            continue
        docstring = column_cls.__doc__ or "[See class {}]".format(column_cls.__name__)
        text = re.sub('[ \t]+', ' ', docstring.strip().replace('\n', ' '))
        doc = "{}: {}".format(name, text)
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


def run_noargs(entries, errors, options_map):
    """Create and run a shell, possibly consuming stdin if not interactive.
    If we're running in a TTY, start an interactive shell.

    Args:
      entries: A list of directives.
      errors: A list of errors, as provided by the parser.
      options_map: A list of options, as produced by the parser.
    """
    shell = BQLShell(entries, errors, options_map)
    if os.isatty(sys.stdin.fileno()):
        # If we're a TTY, run interactively.
        num_directives, num_transactions, num_postings = summary_statistics(entries)
        if 'title' in options_map:
            print('Input file: "{}"'.format(options_map['title']))
        print("Ready with {} directives ({} postings in {} transactions).".format(
            num_directives, num_postings, num_transactions))
        try:
            shell.cmdloop()
        except KeyboardInterrupt:
            print('\nExit')
    else:
        # If we're not a TTY, read the BQL command from standard input.
        script = sys.stdin.read()
        pass ## FIXME: TODO - shell.process_command(script)

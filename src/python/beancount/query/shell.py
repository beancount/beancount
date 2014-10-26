"""An interactive command-line shell interpreter for the Beancount Query Language.
"""
import collections
import os
import sys
import cmd
import readline
import atexit
import pprint
import traceback
from os import path

from beancount.query import query_parser
from beancount.query import query_compile
from beancount.query import query_env
from beancount.query import query_execute
from beancount.core import data
from beancount.reports import table


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



class BQLShell(cmd.Cmd):
    """An interactive shell interpreter for the Beancount query language.
    """
    prompt = 'beancount> '

    def __init__(self, entries, options_map):
        super().__init__()
        load_history(path.expanduser(HISTORY_FILENAME))
        self.parser = query_parser.Parser()
        self.entries = entries
        self.options_map = options_map

        self.env_targets = query_env.TargetsEnvironment()
        self.env_entries = query_env.FilterEntriesEnvironment()
        self.env_postings = query_env.FilterPostingsEnvironment()

    def cmdloop(self):
        "Override cmdloop to handle keyboard interrupts."
        while True:
            try:
                super().cmdloop()
                break
            except KeyboardInterrupt:
                print('\n(Interrupted)')

    def do_lex(self, line):
        """Debug lexer."""
        try:
            self.parser.tokenize(line)
        except query_parser.ParseError as exc:
            print(exc)
    do_tokenize = do_lex

    def do_parse(self, line):
        """Debug parser."""
        print("INPUT: {}".format(repr(line)))
        try:
            statement = self.parser.parse(line, True)
            print(statement)
        except (query_parser.ParseError,
                query_compile.CompilationError) as exc:
            print(exc)
        except Exception as exc:
            traceback.print_exc()

    def on_Print(self, print_stmt):
        # Compile the print statement.
        c_from = query_compile.compile_from(print_stmt.from_clause, self.env_entries)
        c_print = query_parser.Print(c_from)
        query_execute.execute_print(c_print, self.entries, self.options_map, sys.stdout)

    def on_Select(self, select):
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
        rows = query_execute.execute_query(query, self.entries, self.options_map)

        # Output the resulting rows.
        if not rows:
            print("(empty)")
        else:
            table_ = table.create_table(rows)
            table.render_table(table_, sys.stdout, 'text')

    def on_Journal(self, select):
        raise NotImplementedError

    def on_Balances(self, select):
        raise NotImplementedError

    def on_Explain(self, explain):
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

    def dispatch(self, statement):
        """Disatpch the given statement to a suitable method.

        Args:
          statement: An instance provided by the parser.
        Returns:
          Whatever the invoked method happens to return.
        """
        method = getattr(self, 'on_{}'.format(type(statement).__name__))
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
        pass  # Do nothing on an empty line.

    # Methods to exit.
    def do_exit(self, _):
        return 1

    do_quit = do_exit

    def do_EOF(self, _):
        print('exit')
        return 1


def run_noargs(entries, options_map):
    """Create and run a shell, possibly consuming stdin if not interactive.
    If we're running in a TTY, start an interactive shell.

    Args:
      entries: A list of directives.
      options_map: A list of options, as produced by the parser.
    """
    shell = BQLShell(entries, options_map)
    if os.isatty(sys.stdin.fileno()):
        # If we're a TTY, run interactively.
        print("Ready with {} entries.".format(len(entries)))
        try:
            shell.cmdloop()
        except KeyboardInterrupt:
            print('\nExit')
    else:
        # If we're not a TTY, read the BQL command from standard input.
        script = sys.stdin.read()
        pass ## FIXME: TODO - shell.process_command(script)

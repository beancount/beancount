"""An interactive command-line shell interpreter for the Beancount Query Language.
"""
import collections
import os
import sys
import cmd
import readline
import atexit
import traceback
from os import path

from beancount.query import query_parser
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

    def __init__(self, entries):
        super().__init__()
        load_history(path.expanduser(HISTORY_FILENAME))
        self.parser = query_parser.Parser()
        self.entries = entries

    def do_select(self, line_rest):
        line = 'SELECT ' + line_rest
        try:
            statement = self.parser.parse(line)
            run_statement(self.entries, statement)
        except query_parser.ParseError as exc:
            print(exc)
        except Exception as exc:
            traceback.print_exc()

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
            result = self.parser.parse(line, True)
            print(result)
        except query_parser.ParseError as exc:
            print(exc)
        except Exception as exc:
            traceback.print_exc()
    do_explain = do_parse

    def emptyline(self):
        # Do nothing on an empty line.
        pass

    # Methods to exit.
    def do_exit(self, _):
        return 1

    do_quit = do_exit

    def do_EOF(self, _):
        print('exit')
        return 1


def run_noargs(entries):
    """Create and run a shell, possibly consuming stdin if not interactive.
    If we're running in a TTY, start an interactive shell.

    Args:
      entries: A list of directives.
    """
    shell = BQLShell(entries)
    if os.isatty(sys.stdin.fileno()):
        # If we're a TTY, run interactively.
        print("Ready with {} entries.".format(len(entries)))
        shell.cmdloop()
    else:
        # If we're not a TTY, read the BQL command from standard input.
        script = sys.stdin.read()
        pass ## FIXME: TODO - shell.process_command(script)


def run_statement(entries, query):
    """Process some query statements.

    Args:
      entries: A list of directives.
      query: An instance of Query.
    """

    # Create a class for the row.
    Tuple = collections.namedtuple('Tuple',
                                   [target.name for target in query.targets])

    # Filter the entries.
    if query.entry_filter:
        expression = query.entry_filter
        filtered_entries = []
        for entry in entries:
            if isinstance(entry, data.Transaction):
                if expression(entry):
                    filtered_entries.append(entry)
            else:
                filtered_entries.append(entry)
    else:
        filtered_entries = entries

    # Process all the postings.
    rows = []
    expression = query.posting_filter
    for entry in filtered_entries:
        if isinstance(entry, data.Transaction):
            for posting in entry.postings:
                if expression(posting):
                    row = Tuple(*[target(posting) for target in query.targets])
                    rows.append(row)

    if not rows:
        print("(empty)")
    else:
        table_ = table.create_table(rows)
        table.render_table(table_, sys.stdout, 'text')

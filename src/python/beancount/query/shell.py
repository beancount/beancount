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

    def cmdloop(self):
        "Override cmdloop to handle keyboard interrupts."
        while True:
            try:
                super().cmdloop()
                break
            except KeyboardInterrupt:
                print('\n(Interrupted)')

    def do_select(self, line_rest):
        line = 'SELECT ' + line_rest
        try:
            statement = self.parser.parse(line)

            # FIXME: Make this eventually do more than just select.
            select = statement

            xcontext_targets = query_env.TargetsEnvironment()
            xcontext_entries = query_env.FilterEntriesEnvironment()
            xcontext_postings = query_env.FilterPostingsEnvironment()

            query = query_compile.compile_select(select,
                                                 xcontext_targets,
                                                 xcontext_postings,
                                                 xcontext_entries)

            rows = query_execute.execute_query(query, self.entries, self.options_map)

            if not rows:
                print("(empty)")
            else:
                table_ = table.create_table(rows)
                table.render_table(table_, sys.stdout, 'text')

        except (query_parser.ParseError,
                query_compile.CompilationError) as exc:
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
            statement = self.parser.parse(line, True)
            print(statement)
        except (query_parser.ParseError,
                query_compile.CompilationError) as exc:
            print(exc)
        except Exception as exc:
            traceback.print_exc()

    def do_explain(self, line):
        """Debug compilation."""
        try:
            statement = self.parser.parse(line)
            print(statement)

            # FIXME: Generalize this too.
            xcontext_targets = query_env.TargetsEnvironment()
            xcontext_entries = query_env.FilterEntriesEnvironment()
            xcontext_postings = query_env.FilterPostingsEnvironment()

            c_select = query_compile.compile_select(statement,
                                                    xcontext_targets,
                                                    xcontext_postings,
                                                    xcontext_entries)
            print(c_select)

        except (query_parser.ParseError,
                query_compile.CompilationError) as exc:
            print(exc)
        except Exception as exc:
            traceback.print_exc()

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

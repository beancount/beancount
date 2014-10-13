"""An interactive command-line shell interpreter for the Beancount Query Language.
"""
import os
import sys
import cmd
import readline
import atexit
from os import path

from beancount.query import query_parser


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
    def __init__(self):
        super().__init__()
        load_history(path.expanduser(HISTORY_FILENAME))
        self.parser = query_parser.Parser()

    def do_select(self, line_rest):
        line = 'SELECT ' + line_rest
        try:
            select = self.parser.parse(line)
            print(select)
        except query_parser.ParseError as exc:
            print(exc)

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


def run_noargs():
    """Create and run a shell, possibly consuming stdin if not interactive.
    If we're running in a TTY, start an interactive shell.
    """
    shell = BQLShell()
    if os.isatty(sys.stdin.fileno()):
        # If we're a TTY, run interactively.
        shell.cmdloop()
    else:
        # If we're not a TTY, read the BQL command from standard input.
        script = sys.stdin.read()
        pass ## FIXME: TODO - shell.process_command(script)

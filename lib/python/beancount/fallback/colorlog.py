"""
A logging module Formatter class that adds colors to the output.
"""

# stdlib imports
import logging
from logging import Formatter

# local imports
from termctrl import TerminalController


class ColorFormatter(Formatter):
    "A logging Formatter class that adds colors to the output."
    
    colordef = (
        (logging.CRITICAL , 'BOLD MAGENTA'),
        (logging.ERROR    , 'BOLD RED'),
        (logging.WARNING  , 'BOLD YELLOW'),
        (logging.INFO     , 'BOLD GREEN'),
        (logging.DEBUG    , 'BOLD CYAN'),
        )

    def __init__(self, stream, *args, **kwds):
        Formatter.__init__(self, *args, **kwds)

        # Create a mapping of levels to format string which is prepared to
        # contain the escape sequences.
        term = TerminalController(stream)
        self.color_fmt = {}
        for levelno, colstr in self.colordef:
            slist = [getattr(term, c) for c in colstr.split()] + ['%s', term.NORMAL]
            self.color_fmt[levelno] = ''.join(slist)

    def format(self, record):
        return self.color_fmt[record.levelno] % Formatter.format(self, record)


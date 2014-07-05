"""Beancount syntax lexer.
"""
import tempfile

from beancount.parser import _parser


class LexOnlyBuilder(object):
    """A builder used only for getting the lexer to pass.
    The methods do nothing."""

    # pylint: disable=bad-whitespace
    def DATE(self, year, month, day): pass
    def ACCOUNT(self, s):             pass
    def CURRENCY(self, s):            pass
    def STRING(self, s):              pass
    def NUMBER(self, s):              pass
    def TAG(self, s):                 pass
    def LINK(self, s):                pass


def dump_lexer(filename, outfile):
    """Parse an input file and print the tokens to an output file.

    Args:
      filename: a str, the name of the file to be parsed.
    """
    _parser.lexer_init(filename, LexOnlyBuilder())
    while 1:
        x = _parser.lexer_next()
        if x is None:
            break
        token, text, lineno = x
        outfile.write('{:12} {:6d} {}\n'.format(token, lineno, repr(text)))


def dump_lexer_string(input_string):
    """Parse an input string and print the tokens to an output file.

    Args:
      input_string: a str, the contents of the ledger to be parsed.
    """
    with tempfile.NamedTemporaryFile('w') as tmp_file:
        tmp_file.write(input_string)
        tmp_file.flush()
        return dump_lexer(tmp_file.name)

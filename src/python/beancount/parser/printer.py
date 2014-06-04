"""Conversion from internal data structures to text.
"""
import io
import sys
import textwrap

from beancount.core import amount


def render_fileloc(fileloc):
    """Render the fileloc for errors in a way that it will be both detected by
    Emacs and align and rendered nicely.

    Args:
      fileloc: an instance of FileLoc.
    Returns:
      A string, rendered to be interpretable as a message location for Emacs or
      other editors.
    """
    return '{}:{:8}'.format(fileloc.filename, '{}:'.format(fileloc.lineno))


def format_errors(errors):
    """Given a list of error objects, return a formatted string of all the
    errors.

    Args:
      errors: a list of namedtuple objects representing errors.
    Returns:
      A string, the errors rendered.
    """
    file = io.StringIO()
    for error in errors:
        file.write('{} {}\n'.format(render_fileloc(error.fileloc), error.message))
        if error.entry is not None:
            error_string = format_entry(error.entry)
            file.write('\n')
            file.write(textwrap.indent(error_string, '   '))
            file.write('\n')
    return file.getvalue()


class EntryPrinter:
    "Multi-method for printing an entry."

    @classmethod
    def __call__(cls, obj):
        oss = io.StringIO()
        getattr(cls, obj.__class__.__name__)(cls, obj, oss)
        return oss.getvalue()

    def Transaction(cls, entry, oss):
        # Compute the string for the payee and narration line.
        strings = []
        if entry.payee:
            strings.append('"{}" |'.format(entry.payee))
            string_quote(entry.payee)
        if entry.narration:
            strings.append('"{}"'.format(entry.narration))

        if entry.tags:
            for tag in entry.tags:
                strings.append('#{}'.format(tag))
        if entry.links:
            for link in entry.links:
                strings.append('^{}'.format(link))

        oss.write('{e.date} {e.flag} {}\n'.format(' '.join(strings), e=entry))

        for posting in entry.postings:
            cls.Posting(cls, posting, oss)

    def Posting(_, posting, oss):
        flag = '{} '.format(posting.flag) if posting.flag else ''
        assert posting.account is not None
        position = str(posting.position) if posting.position else ''
        price_str = ('@ {}'.format(posting.price.str(amount.MAXDIGITS_PRINTER))
                     if posting.price
                     else '')
        oss.write('  {}{:64} {:>16} {:>16}'.format(flag, posting.account,
                                                   position, price_str).rstrip())
        oss.write('\n')

    def Balance(_, entry, oss):
        oss.write('{e.date} balance {e.account:48} {e.amount}\n'.format(e=entry))

    def Note(_, entry, oss):
        oss.write('{e.date} note {e.account} "{e.comment}"\n'.format(e=entry))

    def Document(_, entry, oss):
        oss.write('{e.date} document {e.account} "{e.filename}"\n'.format(e=entry))

    def Pad(_, entry, oss):
        oss.write('{e.date} pad {e.account} {e.account_pad}\n'.format(e=entry))

    def Open(_, entry, oss):
        oss.write('{e.date} open {e.account} {currencies}\n'.format(
            e=entry, currencies=','.join(entry.currencies or [])))

    def Close(_, entry, oss):
        oss.write('{e.date} close {e.account}\n'.format(e=entry))

    def Price(_, entry, oss):
        oss.write('{e.date} price {e.currency} {e.amount}\n'.format(e=entry))

    def Event(_, entry, oss):
        oss.write('{e.date} event "{e.type}" "{e.description}"\n'.format(e=entry))


def string_quote(obj):
    """Wrap the value into a string with double-quotes; if null, return an empty string.

    Args:
      obj: Any type of object, but mostly strings or None.
    Returns:
      A string instance.
    """
    return '"%s"' % obj if obj is not None else ''


def format_entry(entry):
    """Format an entry into a string in the same input syntax the parser accepts.

    Args:
      entry: An entry instance.
    Returns:
      A string, the formatted entry.
    """
    return EntryPrinter()(entry)


def print_entries(entries, file=None):
    """A convenience function that prints a list of entries to a file.

    Args:
      entries: A list of directives.
      file: An optional file object to write the entries to.
    """
    output = file or sys.stdout
    for entry in entries:
        output.write(format_entry(entry))
        output.write('\n')

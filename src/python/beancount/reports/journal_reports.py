"""Report classes for all reports that display ending journals of accounts.
"""
from beancount.reports import report
from beancount.reports import journal
from beancount.core import data
from beancount.core import realization
from beancount.utils import misc_utils


class JournalReport(report.HTMLReport,
                    metaclass=report.RealizationMeta):
    """Print out an account register/journal."""

    names = ['journal', 'register', 'account']
    default_format = 'text'

    @classmethod
    def add_args(cls, parser):
        parser.add_argument('-a', '--account',
                            action='store', default=None,
                            help="Account to render")

        parser.add_argument('-w', '--width', action='store', type=int, default=0,
                            help="The number of characters wide to render the report to")

        parser.add_argument('-k', '--precision', action='store', type=int, default=2,
                            help="The number of digits to render after the period")

        parser.add_argument('-b', '--render-balance', '--balance', action='store_true',
                            help="If true, render a running balance")

        parser.add_argument('-c', '--at-cost', '--cost', action='store_true',
                            help="If true, render values at cost")

        parser.add_argument('--compact', dest='verbosity', action='store_const',
                            const=journal.COMPACT, default=journal.NORMAL,
                            help="Rendering compactly")
        parser.add_argument('--verbose', dest='verbosity', action='store_const',
                            const=journal.VERBOSE,
                            help="Rendering verbosely")

    def get_postings(self, real_root):
        """Return the postings corresponding to the account filter option.

        Args:
          real_root: A RealAccount node for the root of all accounts.
        Returns:
          A list of posting or directive instances.
        """
        if self.args.account:
            real_account = realization.get(real_root, self.args.account)
            if real_account is None:
                raise report.ReportError(
                    "Invalid account name: {}".format(self.args.account))
        else:
            real_account = real_root

        return realization.get_postings(real_account)

    def render_real_text(self, real_root, options_map, file):
        width = self.args.width or misc_utils.get_screen_width()
        postings = self.get_postings(real_root)
        try:
            journal.text_entries_table(file, postings, width,
                                       self.args.at_cost,
                                       self.args.render_balance,
                                       self.args.precision,
                                       self.args.verbosity)
        except ValueError as exc:
            raise report.ReportError(exc)

    def render_real_htmldiv(self, real_root, options_map, file):
        postings = self.get_postings(real_root)
        journal.html_entries_table_with_balance(file, postings, self.formatter)


class ConversionsReport(report.HTMLReport):
    """Print out a report of all conversions."""

    names = ['conversions']

    def render_htmldiv(self, entries, errors, options_map, file):
        # Return the subset of transaction entries which have a conversion.
        conversion_entries = [entry
                              for entry in misc_utils.filter_type(entries, data.Transaction)
                              if data.transaction_has_conversion(entry)]

        journal.html_entries_table(file, conversion_entries, self.formatter,
                              render_postings=True)

        # Note: Can we somehow add a balance at the bottom? Do we really need one?
        # conversion_balance = complete.compute_entries_balance(conversion_entries)


class DocumentsReport(report.HTMLReport):
    """Print out a report of documents."""

    names = ['documents']

    def render_htmldiv(self, entries, errors, options_map, file):
        document_entries = list(misc_utils.filter_type(entries, data.Document))
        if document_entries:
            journal.html_entries_table(file, document_entries, self.formatter)
        else:
            file.write("<p>(No documents.)</p>")


__reports__ = [
    JournalReport,
    ConversionsReport,
    DocumentsReport,
    ]

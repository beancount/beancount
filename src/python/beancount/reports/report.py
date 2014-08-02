"""Base class for all reports classes.

Each report class should be able to render a filtered list of entries to a
variety of formats. Each report has a name, some command-line options, and
supports some subset of formats.
"""
import argparse
import io
import re

from beancount.reports import table


class ReportError(Exception):
    "Error that occurred during report generation."


class Report:
    """Base class for all reports.

    Attributes:
      names: A list of strings, the various names of this report. The first name
        is taken to be the authoritative name of the report; the rest are
        considered aliases.
      parser: The parser for the command's arguments. This is used to raise errors.
      args: An object that contains the values of this command's parsed arguments.
    """

    # The names of this report. Must be overridden by derived classes.
    names = None

    def __init__(self, args, parser):
        self.parser = parser
        self.args = args

    @classmethod
    def from_args(cls, argv=None):
        """A convenience method used to create an instance from arguments.

        This creates an instance of the report with default arguments. This is a
        convenience that may be used for tests. Our actual script uses subparsers
        and invokes add_args() and creates an appropriate instance directly.

        Args:
          argv: A list of strings, command-line arguments to use to construct tbe report.
        Returns:
          A new instace of the report.
        """
        parser = argparse.ArgumentParser()
        cls.add_args(parser)
        return cls(parser.parse_args(argv or []), parser)

    @classmethod
    def add_args(cls, parser):
        """Add arguments to parse for this report.

        Args:
          parser: An instance of argparse.ArgumentParser.
        """
        # No-op.

    @classmethod
    def get_supported_formats(cls):
        """Enumerates the list of supported formats, by inspecting methods of this object.

        Returns:
          A list of strings, such as ['html', 'text'].
        """
        formats = []
        for name in dir(cls):
            mo = re.match('render_(.*)', name)
            if mo:
                formats.append(mo.group(1))
        return sorted(formats)

    def render(self, entries, errors, options_map, output_format, file=None):
        """Render a report of filtered entries to any format.

        This function dispatches to a specific method.

        Args:
          entries: A list of directives to render.
          errors: A list of errors that occurred during processing.
          options_map: A dict of options, as produced by the parser.
          output_format: A string, the name of the format.
          file: The file to write the output to.
        Returns:
          If no 'file' is provided, return the contents of the report as a
          string.
        Raises:
          ReportError: If the requested format is not supported.
        """
        try:
            render_method = getattr(self, 'render_{}'.format(output_format or
                                                             self.default_format))
        except AttributeError:
            raise ReportError("Unsupported format: '{}'".format(output_format))

        outfile = io.StringIO() if file is None else file
        result = render_method(entries, errors, options_map, outfile)
        assert result is None, "Render method must write to file."
        if file is None:
            return outfile.getvalue()

    __call__ = render


class TableReport(Report):
    """A base class for reports that supports automatic conversions from Table."""

    default_format = 'text'

    def generate_table(self, entries, errors, options_map):
        """Render the report to a Table instance.

        Args:
          entries: A list of directives to render.
          errors: A list of errors that occurred during processing.
          options_map: A dict of options, as produced by the parser.
        Returns:
          An instance of Table, that will get converted to another format.
        """
        raise NotImplementedError

    def render_text(self, entries, errors, options_map, file):
        table_ = self.generate_table(entries, errors, options_map)
        table.generate_table(table_, file, 'text')

    def render_html(self, entries, errors, options_map, file):
        table_ = self.generate_table(entries, errors, options_map)
        table.generate_table(table_, file, 'html')

    def render_htmldiv(self, entries, errors, options_map, file):
        table_ = self.generate_table(entries, errors, options_map)
        table.generate_table(table_, file, 'htmldiv')

    def render_csv(self, entries, errors, options_map, file):
        table_ = self.generate_table(entries, errors, options_map)
        table.generate_table(table_, file, 'csv')

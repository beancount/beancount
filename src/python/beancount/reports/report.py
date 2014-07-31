"""Base class for all reports classes.

Each report class should be able to render a filtered list of entries to a
variety of formats. Each report has a name, some command-line options, and
supports some subset of formats.
"""
import argparse
import io
import re


class Report:

    # The names of this report. Must be overridden by derived classes.
    names = None

    def parse_args(self, args):
        parser = argparse.ArgumentParser()
        self.add_args(parser)
        self.opts, rest_args = parser.parse_known_args(args)
        return rest_args

    def add_args(self, parser):
        """Add arguments to parse for this report.

        Args:
          parser: An instance of argparse.ArgumentParser.
        """
        # No-op.

    def get_supported_formats(self):
        """Enumerates the list of supported formats, by inspecting methods of this object.

        Returns:
          A list of strings, such as ['html', 'text'].
        """
        formats = []
        for name in dir(self):
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


class ReportError(Exception):
    "Error that occurred during report generation."

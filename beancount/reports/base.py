"""Base class for all reports classes.

Each report class should be able to render a filtered list of entries to a
variety of formats. Each report has a name, some command-line options, and
supports some subset of formats.
"""
__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

import io
import re
from os import path

from beancount.utils import table
from beancount.reports import html_formatter
from beancount.parser import options
from beancount.core import realization
from beancount.core import prices
from beancount.core import display_context
from beancount.parser import version


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

    # The default format to use.
    default_format = None

    def __init__(self, args, parser):
        self.parser = parser
        self.args = args
        assert self.default_format, "You must provide a default format."

    @classmethod
    def from_args(cls, argv=None, **kwds):
        """A convenience method used to create an instance from arguments.

        This creates an instance of the report with default arguments. This is a
        convenience that may be used for tests. Our actual script uses subparsers
        and invokes add_args() and creates an appropriate instance directly.

        Args:
          argv: A list of strings, command-line arguments to use to construct the report.
          kwds: A dict of other keyword arguments to pass to the report's constructor.
        Returns:
          A new instance of the report.
        """
        parser = version.ArgumentParser()
        cls.add_args(parser)
        return cls(parser.parse_args(argv or []), parser, **kwds)

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
            match = re.match('render_([a-z0-9]+)$', name)
            if match:
                formats.append(match.group(1))
        return sorted(formats)

    def render(self, entries, errors, options_map, output_format=None, file=None):
        """Render a report of filtered entries to any format.

        This function dispatches to a specific method.

        Args:
          entries: A list of directives to render.
          errors: A list of errors that occurred during processing.
          options_map: A dict of options, as produced by the parser.
          output_format: A string, the name of the format. If not specified, use
            the default format.
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
        except AttributeError as exc:
            raise ReportError("Unsupported format: '{}'".format(output_format)) from exc

        outfile = io.StringIO() if file is None else file
        result = render_method(entries, errors, options_map, outfile)
        assert result is None, "Render method must write to file."
        if file is None:
            return outfile.getvalue()

    __call__ = render


class HTMLReport(Report):
    """A mixin for reports that support forwarding html to htmldiv implementation."""

    default_format = 'html'

    def __init__(self, *args, formatter=None, css_id=None, css_class=None):
        super().__init__(*args)
        if formatter is None:
            formatter = html_formatter.HTMLFormatter(
                display_context.DEFAULT_DISPLAY_CONTEXT)
        self.formatter = formatter
        self.css_id = css_id
        self.css_class = css_class

    def render_html(self, entries, errors, options_map, file):
        template = get_html_template()
        oss = io.StringIO()
        self.render_htmldiv(entries, errors, options_map, oss)
        file.write(template.format(body=oss.getvalue(),
                                   title=''))



class TableReport(HTMLReport):
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
        table.render_table(table_, file, 'text')

    def render_htmldiv(self, entries, errors, options_map, file):
        table_ = self.generate_table(entries, errors, options_map)
        table.render_table(table_, file, 'htmldiv',
                           css_id=self.css_id, css_class=self.css_class)

    def render_csv(self, entries, errors, options_map, file):
        table_ = self.generate_table(entries, errors, options_map)
        table.render_table(table_, file, 'csv')


class RealizationMeta(type):
    """A metaclass for reports that render a realization.

    The main use of this metaclass is to allow us to create report classes with
    render_real_*() methods that accept a RealAccount instance as the basis for
    producing a report.

    RealAccount can be expensive to build, and may be pre-computed and kept
    around to generate the various reports related to a particular filter of a
    subset of transactions, and it would be inconvenient to have to recalculate
    it every time we need to produce a report. In particular, this is the case
    for the web interface: the user selects a particular subset of transactions
    to view, and can then click to the various reports related to this subset of
    transactions. This is why this is useful.

    The classes generated with this metaclass respond to the same interface as
    the regular report classes, so that if invoked from the command-line, it
    will automatically build the realization from the given set of entries. This
    metaclass looks at the class' existing render_real_*() methods and generate
    the corresponding render_*() methods automatically.
    """

    # Note: I'm not a big fan of metaclass magic, but this use case is squarely
    # relevant for it, so I'm using it.
    def __new__(mcs, name, bases, namespace):
        new_type = super(RealizationMeta, mcs).__new__(mcs, name, bases, namespace)

        # Go through the methods of the new type and look for render_real() methods.
        new_methods = {}
        for attr, value in new_type.__dict__.items():
            match = re.match('render_real_(.*)', attr)
            if not match:
                continue

            # Make sure that if an explicit version of render_*() has already
            # been declared, that we don't override it.
            render_function_name = 'render_{}'.format(match.group(1))
            if render_function_name in new_type.__dict__:
                continue

            # Define a render_*() method on the class.
            def forward_method(self, entries, errors, options_map, file, fwdfunc=value):
                account_types = options.get_account_types(options_map)
                real_root = realization.realize(entries, account_types)
                price_map = prices.build_price_map(entries)
                # Note: When we forward, use the latest date (None).
                return fwdfunc(self, real_root, price_map, None, options_map, file)
            forward_method.__name__ = render_function_name
            new_methods[render_function_name] = forward_method

        # Update the type with the newly defined methods..
        for mname, mvalue in new_methods.items():
            setattr(new_type, mname, mvalue)

        # Auto-generate other methods if necessary.
        if hasattr(new_type, 'render_real_htmldiv'):
            setattr(new_type, 'render_real_html', mcs.render_real_html)

        return new_type

    def render_real_html(cls, real_root, price_map, price_date, options_map, file):
        """Wrap an htmldiv into our standard HTML template.

        Args:
          real_root: An instance of RealAccount.
          price_map: A price database.
          price_date: A date for evaluating prices.
          options_map: A dict, options as produced by the parser.
          file: A file object to write the output to.
        """
        template = get_html_template()
        oss = io.StringIO()
        cls.render_real_htmldiv(real_root, price_map, price_date, options_map, oss)
        file.write(template.format(body=oss.getvalue(),
                                   title=''))


def get_html_template():
    """Returns our vanilla HTML template for embedding an HTML div.

    Returns:
      A string, with a formatting style placeholders:
        {title}: for the title of the page.
        {body}: for the body, where the div goes.
    """
    with open(path.join(path.dirname(__file__), 'template.html')) as infile:
        return infile.read()

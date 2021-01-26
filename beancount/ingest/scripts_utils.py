"""Common front-end to all ingestion tools.
"""
__copyright__ = "Copyright (C) 2016,2018  Martin Blais"
__license__ = "GNU GPLv2"

import os
import sys

from beancount.parser import version
from beancount.ingest import identify
from beancount.ingest import extract
from beancount.ingest import file


DESCRIPTION = ("Identify, extract or file away data downloaded from "
               "financial institutions.")


def ingest(importers_list, hooks=None):
    """Driver function that calls all the ingestion tools.

    Put a call to this function at the end of your importer configuration to
    make your import script; this should be its main function, like this:

      from beancount.ingest.scripts_utils import ingest
      my_importers = [ ... ]
      ingest(my_importers)

    This more explicit way of invoking the ingestion is now the preferred way to
    invoke the various tools, and replaces calling the bean-identify,
    bean-extract, bean-file tools with a --config argument. When you call the
    import script itself (as as program) it will parse the arguments, expecting
    a subcommand ('identify', 'extract' or 'file') and corresponding
    subcommand-specific arguments.

    Here you can override some importer values, such as installing a custom
    duplicate finding hook, and eventually more. Note that this newer invocation
    method is optional and if it is not present, a call to ingest() is generated
    implicitly, and it functions as it used to. Future configurable
    customization of the ingestion process will be implemented by inserting new
    arguments to this function, this is the motivation behind doing this.

    Note that invocation by the three bean-* ingestion tools is still supported,
    and calling ingest() explicitly from your import configuration file will not
    break these tools either, if you invoke them on it; the values you provide
    to this function will be used by those tools.

    Args:
      importers_list: A list of importer instances. This is used as a
        chain-of-responsibility, called on each file.
      hooks: An optional list of hook functions to apply to the list of extract
        (filename, entries) pairs, in order. This replaces
        'detect_duplicates_func'.
    """

    parser = version.ArgumentParser(description=DESCRIPTION)

    # Use required on subparsers.
    # FIXME: Remove this when we require version 3.7 or above.
    kwargs = {}
    if sys.version_info >= (3, 7):
        kwargs['required'] = True
    subparsers = parser.add_subparsers(dest='command', **kwargs)

    parser.add_argument('--downloads', '-d', metavar='DIR-OR-FILE',
                        action='append', default=[],
                        help='Filenames or directories to search for files to import')

    for cmdname, module in [('identify', identify),
                            ('extract', extract),
                            ('file', file)]:
        parser_cmd = subparsers.add_parser(cmdname, help=module.DESCRIPTION)
        parser_cmd.set_defaults(command=module.run)
        module.add_arguments(parser_cmd)

    args = parser.parse_args()

    if not args.downloads:
        args.downloads.append(os.getcwd())

    # Implement required ourselves.
    # FIXME: Remove this when we require version 3.7 or above.
    if not (sys.version_info >= (3, 7)):
        if not hasattr(args, 'command'):
            parser.error("Subcommand is required.")

    abs_downloads = list(map(os.path.abspath, args.downloads))
    args.command(args, parser, importers_list, abs_downloads, hooks=hooks)
    return 0

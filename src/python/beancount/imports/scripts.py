"""Support functions to create importer scripts.

Call the import_main() function to build your own very personal import script.
"""
import sys
import argparse

from beancount.imports import imports
from beancount.imports import filing
from beancount import parser
from beancount.loader import load


def create_parser(beancount_file=None,
                  files_or_directories=None):
    """Create a command-line parser for default import scripts.
    The arguments are used to set the default values on the parser.

    Args:
      beancount_file: An optional parameter that specifies a beancount input
                      filename to load up in order to de-duplicate entries.
                      This can be overridden by one of the command-line options.

      files_or_directories: An optional list of default files or directories to
                            be imported by the script. If the user specifies
                            scripts on the command-line, they override those
                            defaults.

    Returns:
      An argparse ArgumentParser instance.
    """
    parser = argparse.ArgumentParser("Import the files given on the command-line.")

    parser.add_argument('files_or_directories', nargs='+',
                        default=files_or_directories or [],
                        help='Filenames or directories to search for files to import')

    parser.add_argument('-o', '--output', action='store',
                        help='Output filename (default: stdout)')

    parser.add_argument('-m', '--mindate', metavar='YYYY-MM-DD', action='store',
                        help="Exclude imported entries earlier than this date")

    parser.add_argument('-n', '--dry-run', action='store_true',
                        help="Just print the identification of files out; don't import anything")

    parser.add_argument('-d', '--debug', action='store_true',
                        help="Print extra debugging output.")

    parser.add_argument('-f', '--beancount', metavar='FILENAME',
                        default=beancount_file,
                        help='Beancount files to parse for cross-checking, de-duplication')

    parser.add_argument('--file-only', '--file', action='store', metavar='DIRECTORY',
                        help=("Don't import from the files; "
                              "just file them under the given directory."))

    return parser


def import_with_options(importer_config, opts):
    """Parse command-line options and run an importer.

    This function allows you to create a new import script in a single line.

    Args:
      importer_config: A configuration object that is used to match files and
                       perform the import proces.

      opts: An object with the parsed attributes defined on create_parser.
            (You get this by calling parser.parse_arg()).

    Returns:
      Nothing, but the output is sent to stdout (if not overridden by the user's
      command-line options).
    """
    assert isinstance(opts, argparse.Namespace), opts

    if opts.file_only:
        # Run the filer.
        filing.run_filer_loop(importer_config,
                              opts.files_or_directories,
                              opts.file_only,
                              opts.dry_run, opts.debug,
                              mkdirs=True)
    else:
        # Create a suitable output file.
        output = open(opts.output, 'w') if opts.output else sys.stdout

        # Parse the options.
        if opts.mindate:
            mo = re.match('(\d\d\d\d)-(\d\d)-(\d\d)', opts.mindate)
            if not mo:
                parser.error("Invalid date format; must be YYYY-MM-DD")
            opts.mindate = datetime.date(*map(int, mo.groups()))

        # If specified, parse the ledger and get the entries from it.
        if opts.beancount:
            crosscheck_entries, _, _ = load(opts.beancount, quiet=True)
        else:
            crosscheck_entries = []

        # Run the importer.
        imports.run_importer_loop(importer_config,
                                  opts.files_or_directories,
                                  output,
                                  crosscheck_entries,
                                  opts.mindate, opts.dry_run, opts.debug)


def load_module_attribute(filename, attribute_name='CONFIG'):
    """Load the user configuration file and extract a configuration object.

    This can be used to execute a file as Python and get a particular attribute
    from it. This can be used to load up a file with an importer configuration
    object embedded in it.
    """
    code = compile(open(filename).read(), filename, 'exec')
    config_env = {}
    exec(code, globals(), config_env)
    config = config_env[attribute_name]
    assert isinstance(config, list)
    assert all(isinstance(element, (tuple, list)) for element in config)
    return config


def import_main(importer_config,
                beancount_file=None,
                files_or_directories=None):
    """Main program for the importer.

    Call this to create your own importer program. This is meant to be used in a
    new script that you write with your own configuration object in it, calling
    this function at the end, like this:

       MY_IMPORT_CONFIG = [
         ...
       ]

       from beancount.imports.scripts
       scripts.import_main(MY_IMPORT_CONFIG)

    Args:
      importer_config: See import_with_options().
      beancount_file: See create_parser().
      files_or_directories: See create_parser().

    Returns:
      Nothing, but it outputs the imported list of entries to the specified
      output file (stdout by default).
    """

    try:
        # Create a parser, parse the arguments, and run the driver function.
        # If you like, you can copy this and add your own custom arguments to
        # the parser. See bean-import for an example.
        parser = create_parser(beancount_file, files_or_directories)
        opts = parser.parse_args()
        import_with_options(importer_config, opts)
    except KeyboardInterrupt:
        sys.stderr.write("INTERRUPTED. Exiting.\n")

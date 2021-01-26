"""Code to help identify, extract, and file external downloads.

This package contains code to help you build importers and drive the process of
identifying which importer to run on an externally downloaded file, extract
transactions from them and file away these files under a clean and rigidly named
hierarchy for preservation.
"""
__copyright__ = "Copyright (C) 2016,2018  Martin Blais"
__license__ = "GNU GPLv2"

import os
import sys

import click

from beancount import loader
from beancount.ingest import extract
from beancount.ingest import file
from beancount.ingest import identify
from beancount.parser.version import VERSION


@click.command('extract')
@click.argument('src', nargs=-1, type=click.Path(exists=True, resolve_path=True))
@click.option('--output', '-o', type=click.File('w'), default='-',
              help='Output file.')
@click.option('--existing', '-e', type=click.Path(exists=True),
              help='Existing Beancount ledger for de-duplication.')
@click.option('--reverse', '-r', is_flag=True,
              help='Sort entries in reverse order.')
@click.pass_obj
def _extract(ctx, src, output, existing, reverse):
    """Extract transactions from documents."""

    # Load the ledger, if one is specified.
    if existing:
        entries, _, options_map = loader.load_file(existing)
    else:
        entries, options_map = None, None

    ctx.extract(src, output,
                entries=entries,
                options_map=options_map,
                mindate=None,
                ascending=not reverse,
                hooks=None)


@click.command('file')
@click.argument('src', nargs=-1, type=click.Path(exists=True, resolve_path=True))
@click.option('--destination', '-o',
              type=click.Path(exists=True, file_okay=False), metavar='DIR',
              help='The destination documents tree root directory.')
@click.option('--dry-run', '-n', is_flag=True,
              help='Just print where the files would be moved.')
@click.option('--overwrite', '-f', is_flag=True,
              help='Overwrite destination files with the same name.')
@click.pass_obj
def _file(ctx, src, destination, dry_run, overwrite):
    """File away documents.

    Walk the list of SRC filenames or directories of downloaded files,
    and for each of those files, move the file under a filing
    directory corresponding to the assocaited account.

    """
    # If the output directory is not specified, move the files at the
    # root where the import script is located. Providing this default
    # seems better than using a required option.
    if destination is None:
        # pylint: disable=import-outside-toplevel
        import __main__
        destination = os.path.dirname(os.path.abspath(__main__.__file__))

    ctx.file(src, destination,
             dry_run=dry_run,
             mkdirs=True,
             overwrite=overwrite,
             idify=True,
             logfile=sys.stdout)


@click.command('identify')
@click.argument('src', nargs=-1, type=click.Path(exists=True, resolve_path=True))
@click.pass_obj
def _identify(ctx, src):
    """Identify files for import."""
    return ctx.identify(src)


class Ingest:
    def __init__(self, importers=None):
        self.importers = importers or []

    def extract(self, what, *args, **kwargs):
        extract.extract(self.importers, what, *args, **kwargs)

    def file(self, what, *args, **kwargs):
        file.file(self.importers, what, *args, **kwargs)

    def identify(self, what, *args, **kwargs):
        identify.identify(self.importers, what, *args, **kwargs)

    def cli(self):
        @click.group()
        @click.version_option(message=VERSION)
        @click.pass_context
        def main(ctx):
            """Import data from and file away documents from finantial institutions."""
            ctx.obj = self

        main.add_command(_extract)
        main.add_command(_file)
        main.add_command(_identify)

        return main

    def __call__(self):
        main = self.cli()
        main()

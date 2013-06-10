"""Driver for the code that files away imported files under a directory
hierarchy mirroring the chart of accounts.
"""
import sys

from beancount2.imports import imports


def run_filer(importer_config, files_or_directories, destination, dry_run=False):
    """Attempt to automatically file the importable files."""

    trace = lambda *args: print(*args, file=sys.stdout)
    for filename, match_text, matches in imports.find_imports(importer_config, files_or_directories):
        # Print the filename and which modules matched.
        trace('=== {}'.format(filename))
        # if matches:
        #     trace('')
        for module, module_config in matches:
            # trace('  Importer: {}'.format(module.__name__ if module else '-'))
            trace(module_config['FILE'])
            trace('')

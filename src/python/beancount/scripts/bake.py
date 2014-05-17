"""Bake a Beancount input file's web files to a directory hierarchy.

You provide a Beancount filename, an output directory, and this script
runs a server and a scraper that puts all the files in the directory,
and if your output name has an archive suffix, we automatically the
fetched directory contents to the archive and delete them.
"""
import argparse
import subprocess
import shutil
from os import path
from beancount.web import web


def bake_to_directory(webargs, output, quiet_subproc=False, quiet_server=False):
    """Serve and bake a Beancount's web to a directory.

    Args:
      webargs: An argparse parsed options object with the web app arguments.
      output: A directory name. We don't check here whether it exists or not.
      quiet_subproc: A boolean, True to suppress output from the subprocesses.
    Returns:
      True on success, False otherwise.
    """
    # Start a server thread locally.
    thread = web.thread_server_start(webargs)

    # Define a command that will run and convert all the links to work in a
    # local mirror of the server (your accountant!) so a user can browse the
    # static files extracted.
    url = 'http://localhost:{}/'.format(webargs.port)
    command = ['wget',
               '--recursive',
               '--no-host-directories',
               '--page-requisites',
               '--convert-links',
               '--html-extension',
               '--waitretry=0.05',
               '--exclude-directories=/{}/'.format(web.doc_name),
               url,
               '--directory-prefix', output]

    p = subprocess.Popen(command,
                         shell=False,
                         stdout=subprocess.PIPE if quiet_subproc else None,
                         stderr=subprocess.PIPE if quiet_subproc else None)
    _, _ = p.communicate()

    # Shutdown the server thread.
    web.thread_server_shutdown(thread)

    return (p.returncode == 0)


def archive_targz(directory, archive_name, quiet=False):
    """Archive the directory to the given tar/gz filename.

    Args:
      directory: A string, the name of the directory to archive.
      archive_name: A string, the name of the file to output.
      quiet: A boolean, True to suppress output.
    Raises:
      IOError: if the directory does not exist or if the archive name already
      exists.
    """
    if not path.exists(directory):
        raise IOError("Directory to archive '{}' does not exist".format(
            directory))
    if path.exists(archive_name):
        raise IOError("Output archive name '{}' already exists".format(
            archive_name))

    p = subprocess.Popen(['tar',
                          '-C', path.dirname(directory),
                          '-zcvf', archive_name,
                          path.basename(directory)],
                         shell=False,
                         stdout=subprocess.PIPE if quiet else None,
                         stderr=subprocess.PIPE if quiet else None)
    _, _ = p.communicate()
    if p.returncode != 0:
        raise OSError("Archive failure.")


ARCHIVERS = {
    '.tar.gz': archive_targz,
    '.tgz': archive_targz,
    }


# FIXME: Move this to utils.
def path_greedy_split(filename):
    """Split a path, returning the longest possible extension.

    Args:
      filename: A string, the filename to split.
    Returns:
      A pair of basename, extension (which includes the leading period).
    """
    basename = path.basename(filename)
    index = basename.find('.')
    if index == -1:
        extension = None
    else:
        extension = basename[index:]
        basename = basename[:index]
    return (path.join(path.dirname(filename), basename), extension)


def main():
    parser = argparse.ArgumentParser(__doc__)

    web_group = web.add_web_arguments(parser)
    web_group.set_defaults(port=9475)

    group = parser.add_argument_group("Bake process arguments")

    group.add_argument('output',
                       help=('The output directory or archive name. If you '
                             'specify a filename with a well-known extension,'
                             'we automatically archive the fetched directory '
                             'contents to this archive name and delete them.'))

    group.add_argument('--verbose', action='store_true',
                       help="Let subcommand output through.")

    group.add_argument('--quiet', action='store_true',
                       help="Don't even print out web server log")

    opts = parser.parse_args()

    if opts.verbose and opts.quiet:
        parser.error("Invalid options, cannot specify both --verbose and --quiet")

    # Figure out the directory to actually bake to, regardless of whether we
    # archive later on.
    output_directory, extension = path_greedy_split(opts.output)
    if extension:
        try:
            archiver = ARCHIVERS[extension]
        except KeyError:
            raise SystemExit("ERROR: Unknown archiver type '{}'".format(extension))
    else:
        archiver = None

    # Check pre-conditions on input/output filenames.
    if not path.exists(opts.filename):
        raise SystemExit("ERROR: Missing input file '{}'".format(opts.filename))
    if path.exists(opts.output):
        raise SystemExit("ERROR: Output path already exists '{}'".format(opts.output))
    if path.exists(output_directory):
        raise SystemExit(
            "ERROR: Output directory already exists '{}'".format(output_directory))

    baked = bake_to_directory(opts, output_directory, not opts.verbose, opts.quiet)
    if not baked:
        raise SystemExit("ERROR: Error baking into directory '{}'".format(
            output_directory))

    # Archive if requested.
    if archiver:
        archiver(output_directory, opts.output, not opts.verbose)
        shutil.rmtree(output_directory)

    print("Output in '{}'".format(opts.output))


if __name__ == '__main__':
    main()

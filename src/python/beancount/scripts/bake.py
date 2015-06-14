"""Bake a Beancount input file's web files to a directory hierarchy.

You provide a Beancount filename, an output directory, and this script
runs a server and a scraper that puts all the files in the directory,
and if your output name has an archive suffix, we automatically the
fetched directory contents to the archive and delete them.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import argparse
import functools
import logging
import os
import subprocess
import shutil
import shlex
from os import path

import lxml.html

from beancount.web import scrape
from beancount.web import web
from beancount.web import web_test
from beancount.scripts import checkdeps
from beancount.utils import file_utils


def normalize_filename(url):
    """Convert URL paths to filenames. Add .html extension if needed.

    Args:
      url: A string, the url to convert.
    Returns:
      A string, possibly with an extension appended.
    """
    if url.endswith('/'):
        return path.join(url, 'index.html')
    else:
        return url if path.splitext(url)[1] else (url + '.html')


def relativize_links(html, current_url):
    """Make all the links in the contents string relative to an URL.

    Args:
      html: An lxml document node.
      current_url: A string, the URL of the current page, a path to.
        a file or a directory. If the path represents a directory, the
        path ends with a /.
    Returns:
      A bytes object, the re-rendered HTML string.
    """
    current_dir = path.dirname(current_url)
    for element, attribute, link, pos in lxml.html.iterlinks(html):
        if path.isabs(link):
            relative_link = path.relpath(normalize_filename(link), current_dir)
            element.set(attribute, relative_link)
    return lxml.html.tostring(html, method="xml")


def save_scraped_document(output_dir, response, html_root):
    """Callback function to process a document being scraped.

    This converts the document to have relative links and writes out the file to
    the output directory.

    Args:
      output_dir: A string, the output directory to write.
      response: An http response as per urlopen.

      url: A string, the URL of the page being scraped.
      status: An integer, the status code from the response.
      contents: A bytes object, the contents of the response.
      html_root: An lxml root node for the document, optionally. If this is provided,
        this avoid you having to reprocess it (for performance reasons).
    """
    if response.status != 200:
        logging.error("Invalid status: %s", response.status)

    # Ignore directories.
    if response.url.endswith('/'):
        return

    # Convert all the links to be relative ones.
    if response.info().get_content_type() == 'text/html':
        if html_root is None:
            html_root = lxml.html.document_fromstring(response.read())
        contents = relativize_links(html_root, response.url)
    else:
        contents = response.read()

    # Compute output filename and write out the relativized contents.
    output_filename = path.join(output_dir,
                                normalize_filename(response.url).lstrip('/'))
    os.makedirs(path.dirname(output_filename), exist_ok=True)
    with open(output_filename, 'wb') as outfile:
        outfile.write(contents)


def bake_to_directory(webargs, output_dir, quiet=False):
    """Serve and bake a Beancount's web to a directory.

    Args:
      webargs: An argparse parsed options object with the web app arguments.
      output_dir: A directory name. We don't check here whether it exists or not.
      quiet: A boolean, True to suppress web server fetch log.
    Returns:
      True on success, False otherwise.
    """
    callback = functools.partial(save_scraped_document, output_dir)
    # Skip the context pages, too slow.
    # Skip the component pages... too many.
    # Skip served documents.
    scrape.scrape(webargs.filename, callback, webargs.port,
                  '/(context|view/component|.*/doc)/', quiet)


def archive(command_template, directory, archive, quiet=False):
    """Archive the directory to the given tar/gz archive filename.

    Args:
      command_template: A string, the command template to format with in order
        to compute the command to run.
      directory: A string, the name of the directory to archive.
      archive: A string, the name of the file to output.
      quiet: A boolean, True to suppress output.
    Raises:
      IOError: if the directory does not exist or if the archive name already
      exists.

    """
    directory = path.abspath(directory)
    archive = path.abspath(archive)
    if not path.exists(directory):
        raise IOError("Directory to archive '{}' does not exist".format(
            directory))
    if path.exists(archive):
        raise IOError("Output archive name '{}' already exists".format(
            archive))

    command = command_template.format(directory=directory,
                                      dirname=path.dirname(directory),
                                      basename=path.basename(directory),
                                      archive=archive)

    pipe = subprocess.Popen(shlex.split(command),
                            shell=False,
                            cwd=path.dirname(directory),
                            stdout=subprocess.PIPE if quiet else None,
                            stderr=subprocess.PIPE if quiet else None)
    _, _ = pipe.communicate()
    if pipe.returncode != 0:
        raise OSError("Archive failure")


ARCHIVERS = {
    '.tar.gz'  : 'tar -C {dirname} -zcvf {archive} {basename}',
    '.tgz'     : 'tar -C {dirname} -zcvf {archive} {basename}',
    '.tar.bz2' : 'tar -C {dirname} -jcvf {archive} {basename}',
    '.zip'     : 'zip -r {archive} {basename}',
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)

    web_group = web.add_web_arguments(parser)
    web_group.set_defaults(port=9475)

    group = parser.add_argument_group("Bake process arguments")

    group.add_argument('output',
                       help=('The output directory or archive name. If you '
                             'specify a filename with a well-known extension,'
                             'we automatically archive the fetched directory '
                             'contents to this archive name and delete them.'))

    group.add_argument('-q', '--quiet', action='store_true',
                       help="Don't even print out web server log")

    opts = parser.parse_args()

    # Figure out the archival method.
    output_directory, extension = file_utils.path_greedy_split(opts.output)
    if extension:
        try:
            archival_command = ARCHIVERS[extension]
        except KeyError:
            raise SystemExit("ERROR: Unknown archiver type '{}'".format(extension))
    else:
        archival_command = None

    # Check pre-conditions on input/output filenames.
    if not path.exists(opts.filename):
        raise SystemExit("ERROR: Missing input file '{}'".format(opts.filename))
    if path.exists(opts.output):
        raise SystemExit("ERROR: Output path already exists '{}'".format(opts.output))
    if path.exists(output_directory):
        raise SystemExit(
            "ERROR: Output directory already exists '{}'".format(output_directory))

    # Make sure that wget is installed.
    package, version, sufficient = checkdeps.check_wget()
    if not sufficient:
        parser.error("Package {} is not installed or insufficient (version: {})".format(
            package, version or 'N/A'))

    try:
        bake_to_directory(opts, output_directory, opts.quiet)
    except Exception as exc:
        raise SystemExit("ERROR: Error baking into directory '{}': {}".format(
            output_directory, exc))

    # Archive if requested.
    if archival_command:
        archive(archival_command, output_directory, opts.output, True)
        shutil.rmtree(output_directory)

    print("Output in '{}'".format(opts.output))


if __name__ == '__main__':
    main()

"""Code to write output to a pager.

This module contains an object accumulates lines up to a minimum and then
decides whether to flush them to the original output directly if under the
threshold (no pager) or creates a pager and flushes the lines to it if above the
threshold and then forwards all future lines to it. The purpose of this object
is to pipe output to a pager only if the number of lines to be printed exceeds a
minimum number of lines.

The contextmanager is intended to be used to pipe output to a pager and wait on
the pager to complete before continuing. Simply write to the file object and
upon exit we close the file object. This also silences broken pipe errors
triggered by the user exiting the sub-process, and recovers from a failing pager
command by just using stdout.
"""
__copyright__ = "Copyright (C) 2014-2017  Martin Blais"
__license__ = "GNU GPLv2"

import codecs
import contextlib
import os
import sys
import subprocess
import io
import logging


# The default command to run for a pager, if the PAGER environment variable is not set.
DEFAULT_PAGER = 'more'


def create_pager(command, file):
    """Try to create and return a pager subprocess.

    Args:
      command: A string, the shell command to run as a pager.
      file: The file object for the pager write to. This is also used as a
        default if we failed to create the pager subprocess.
    Returns:
      A pair of (file, pipe), a file object and an optional subprocess.Popen instance
      to wait on. The pipe instance may be set to None if we failed to create a subprocess.
    """

    if command is None:
        command = os.environ.get('PAGER', DEFAULT_PAGER)
    if not command:
        command = DEFAULT_PAGER

    pipe = None

    # In case of using 'less', make sure the charset is set properly. In theory
    # you could override this by setting PAGER to "LESSCHARSET=utf-8 less" but
    # this shouldn't affect other programs and is unlikely to cause problems, so
    # we set it here to make default behavior work for most people (we always
    # write UTF-8).
    env = os.environ.copy()
    env['LESSCHARSET'] = "utf-8"

    try:
        pipe = subprocess.Popen(command, shell=True,
                                stdin=subprocess.PIPE,
                                stdout=file,
                                env=env)
    except OSError as exc:
        logging.error("Invalid pager: {}".format(exc))
    else:
        stdin_wrapper = io.TextIOWrapper(pipe.stdin, 'utf-8')
        file = stdin_wrapper
    return file, pipe


class ConditionalPager:
    """A proxy file for a pager that only creates a pager after a minimum number of
    lines has been printed to it.
    """
    def __init__(self, command, minlines=None):
        """Create a conditional pager.

        Args:
          command: A string, the shell command to run as a pager.
          minlines: If set, the number of lines under which you should not bother starting
            a pager. This avoids kicking off a pager if the screen is high enough to
            render the contents. If the value is unset, always starts a pager (which is
            fine behavior too).
        """
        self.command = command
        self.minlines = minlines
        self.default_file = (codecs.getwriter("utf-8")(sys.stdout.buffer)
                             if hasattr(sys.stdout, 'buffer') else
                             sys.stdout)

    def __enter__(self):
        """Initialize the context manager and return this instance as it."""

        # The file and pipe object we're writing to. This gets set after the
        # number of accumulated lines reaches the threshold.
        if self.minlines:
            self.file = None
            self.pipe = None
        else:
            self.file, self.pipe = create_pager(self.command, self.default_file)

        # Lines accumulated before the threshold.
        self.accumulated_data = []
        self.accumulated_lines = 0

        # Return this object to be used as the context manager itself.
        return self

    def flush_accumulated(self, file):
        """Flush the existing lines to the newly created pager.
        This also disabled the accumulator.

        Args:
          file: A file object to flush the accumulated data to.
        """
        if self.accumulated_data:
            write = file.write
            for data in self.accumulated_data:
                write(data)
        self.accumulated_data = None
        self.accumulated_lines = None

    def write(self, data):
        """Write the data out. Overridden from the file object interface.

        Args:
          data: A string, data to write to the output.
        """
        if self.file is None:
            # Accumulate the new lines.
            self.accumulated_lines += data.count('\n')
            self.accumulated_data.append(data)

            # If we've reached the threshold, create a file.
            if self.accumulated_lines > self.minlines:
                self.file, self.pipe = create_pager(self.command, self.default_file)
                self.flush_accumulated(self.file)
        else:
            # We've already created a pager subprocess... flush the lines to it.
            self.file.write(data)
            # try:
            # except BrokenPipeError:
            #     # Make sure we don't barf on __exit__().
            #     self.file = self.pipe = None
            #     raise

    def __exit__(self, type, value, unused_traceback):
        """Context manager exit. This flushes the output to our output file.

        Args:
          type: Optional exception type, as per context managers.
          value: Optional exception value, as per context managers.
          unused_traceback: Optional trace.
        """
        try:
            if self.file:
                # Flush the output file and close it.
                self.file.flush()
            else:
                # Oops... we never reached the threshold. Flush the accumulated
                # output to the file.
                self.flush_accumulated(self.default_file)

            # Wait for the subprocess (if we have one).
            if self.pipe:
                self.file.close()
                self.pipe.wait()

        # Absorb broken pipes that may occur on flush or close above.
        except BrokenPipeError:
            return True

        # Absorb broken pipes.
        if isinstance(value, BrokenPipeError):
            return True
        elif value:
            raise


@contextlib.contextmanager
def flush_only(fileobj):
    """A contextmanager around a file object that does not close the file.

    This is used to return a context manager on a file object but not close it.
    We flush it instead. This is useful in order to provide an alternative to a
    pager class as above.

    Args:
      fileobj: A file object, to remain open after running the context manager.
    Yields:
      A context manager that yields this object.
    """
    try:
        yield fileobj
    finally:
        fileobj.flush()

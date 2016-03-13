"""Importer protocol.

All importers must comply with this interface and implement at least some of its
methods. A configuration consists in a simple list of such importer instances.
The importer processes run through the importers, calling some of its methods in
order to identify, extract and file the downloaded files.

Each of the methods accept a cache.FileMemo object which has a 'name' attribute
with the filename to process, but which also provides a place to cache
conversions. Use its convert() method whenever possible to avoid carrying out
the same conversion multiple times. See beancount.ingest.cache for more details.

Synopsis:

 name(): Return a unique identifier for the importer instance.
 identify(): Return true if the identifier is able to process the file.
 extract(): Extract directives from a file's contents and return of list of entries.
 file_account(): Return an account name associated with the given file for this importer.
 file_date(): Return a date associated with the downloaded file (e.g., the statement date).
 file_name(): Return a cleaned up filename for storage (optional).

Just to be clear: Although this importer will not raise NotImplementedError
exceptions (it returns default values for each method), you NEED to derive from
it in order to do anything meaningful. Simply instantiating this importer will
not match not provide any useful information. It just defines the protocol for
all importers.
"""
__author__ = "Martin Blais <blais@furius.ca>"

from beancount.core import flags


class ImporterProtocol:
    "Interface that all source importers need to comply with."

    # A flag to use on new transaction. Override this flag in derived classes if
    # you prefer to create your imported transactions with a different flag.
    FLAG = flags.FLAG_OKAY

    def name(self):
        """Return a unique id/name for this importer.

        Returns:
          A string which uniquely identifies this importer.
        """
        cls = self.__class__
        return '{}.{}'.format(cls.__module__, cls.__name__)

    __str__ = name

    def identify(self, file):
        """Return true if this importer matches the given file.

        Args:
          file: A cache.FileMemo instance.
        Returns:
          A boolean, true if this importer can handle this file.
        """

    def extract(self, file):
        """Extract transactions from a file.

        Args:
          file: A cache.FileMemo instance.
        Returns:
          A list of new, imported directives (usually mostly Transactions)
          extracted from the file.
        """

    def file_account(self, file):
        """Return an account associated with the given file.

        Note: If you don't implement this method you won't be able to move the
        files into its preservation hierarchy; the bean-file command won't work.

        Also, normally the returned account is not a function of the input
        file--just of the importer--but it is provided anyhow.

        Args:
          file: A cache.FileMemo instance.
        Returns:
          The name of the account that corresponds to this importer.
        """

    def file_name(self, file):
        """A filter that optionally renames a file before filing.

        This is used to make tidy filenames for filed/stored document files. The
        default implementation just returns the same filename. Note that a
        simple RELATIVE filename must be returned, not an absolute filename.

        Args:
          file: A cache.FileMemo instance.
        Returns:
          The tidied up, new filename to store it as.
        """

    def file_date(self, file):
        """Attempt to obtain a date that corresponds to the given file.

        Args:
          file: A cache.FileMemo instance.
        Returns:
          A date object, if successful, or None if a date could not be extracted.
          (If no date is returned, the file creation time is used. This is the
          default.)
        """

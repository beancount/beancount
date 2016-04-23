"""A mixin class which allows identification on a text representation.

This mixin implements the identify() method, and uses a list of regular
expressions against a single text representation of the file's characteristics:
its full name, its MIME type, and its contents, which looks like this:

The point is to allow an implementor to specify identification using just a few
regular expressions.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import re


class RegexpImporterMixin:
    """A mixin class which allows identification on a text representation.

    Mix this into the implementation of a importer.ImporterProtocol.
    """

    def __init__(self, regexps=None):
        """Provide regular expressions for identifying a file.

        Args:
          regexps: A list of regular expression strings.
        Raises:
          re.error: If some of the regular expressions are invalid.
        """
        super().__init__()
        assert isinstance(regexps, (list, type(None)))

        self.regexps = [(regexp, re.compile(regexp, re.DOTALL|re.IGNORECASE|re.MULTILINE))
                        for regexp in regexps]

    def get_text(self, file):
        """Return the text contents of the file.

        This can be the file itself, or if it is a binary file, some conversion
        of the contents to a format that can be matched against. Override this
        file in order to provide a suitable conversion.

        Args:
          file: A cache.FileMemo instance.
        Returns:
          A string, the contents of the file.
        """
        # The default implementation does not convert its contents.
        return file.contents()

    def identify(self, file):
        """See base class."""
        match_text = '\n'.join([
            "Filename: {}".format(file.name),
            "MimeType: {}".format(file.mimetype()),
            "Contents: {}".format(self.get_text(file)),
        ])
        return all(compre.search(match_text)
                   for regexp, compre in self.regexps)

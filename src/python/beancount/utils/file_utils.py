"""File utilities.
"""
from os import path


def guess_file_format(filename, default='txt'):
    """Guess the file format from the filename.

    Args:
      filenmae: A string, the name of the file. This can be None.
    Returns:
      A string, the extension of the format, without a leading period.
    """
    if filename:
        if filename.endswith('.txt'):
            format = 'txt'
        elif filename.endswith('.csv'):
            format = 'csv'
        elif filename.endswith('.html') or filename.endswith('.xhtml'):
            format = 'html'
        else:
            format = default
    else:
        format = default
    return format


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

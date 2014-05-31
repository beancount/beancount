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

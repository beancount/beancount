"""Check the installation dependencies and report the version numbers of each.

This is meant to be used as an error diagnostic tool.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import sys


def check_dependencies():
    """Check the runtime dependencies and report their version numbers.

    Returns:
      A list of pairs of (package-name, version-number, sufficient) whereby if a
      package has not been installed, its 'version-number' will be set to None.
      Otherwise, it will be a string with the version number in it. 'sufficient'
      will be True if the version if sufficient for this installation of
      Beancount.
    """
    return [
        # Check for a complete installation of Python itself.
        check_python(),

        # Modules we really do need installed.
        check_import('dateutil'),
        check_import('bottle'),
        check_import('ply', module_name='ply.yacc', min_version='3.4'),
        check_import('lxml', module_name='lxml.etree', min_version='3'),

        # Test are only required because of google-api-python-client.
        check_import('apiclient'),
        check_import('oauth2client'),
        ]


def check_python():
    """Check that Python 3.3 or above is installed.

    Returns:
      A triple of (package-name, version-number, sufficient) as per
      check_dependencies().
    """
    return ('python3',
            '.'.join(map(str, sys.version_info[:3])),
            sys.version_info[:2] >= (3, 3))


def check_import(package_name, min_version=None, module_name=None):
    """Check that a particular module name is installed.

    Args:
      package_name: A string, the name of the package and module to be
        imported to verify this works. This should have a __version__
        attribute on it.
      min_version: If not None, a string, the minimum version number
        we require.
      module_name: The name of the module to import if it differs from the
        package name.
    Returns:
      A triple of (package-name, version-number, sufficient) as per
      check_dependencies().
    """
    if module_name is None:
        module_name = package_name
    try:
        __import__(module_name)
        module = sys.modules[module_name]
        version = module.__version__
        assert isinstance(version, str)
        is_sufficient = version >= min_version if min_version else True
    except ImportError:
        version, is_sufficient = None, False
    return (package_name, version, is_sufficient)

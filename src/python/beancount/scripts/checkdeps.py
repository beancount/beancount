"""Check the installation dependencies and report the version numbers of each.

This is meant to be used as an error diagnostic tool.
"""
import re
import sys
import subprocess


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
        check_python(),
        check_cdecimal(),
        check_dateutil(),
        check_bottle(),
        check_wget(),
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


def check_cdecimal():
    """Check that cdecimal 2.3 or above is installed.

    Returns:
      A triple of (package-name, version-number, sufficient) as per
      check_dependencies().
    """
    try:
        import cdecimal
        # Note: There is no method to obtain the version number.
        version, sufficient = 'INSTALLED', True
    except ImportError:
        version, sufficient = None, False
    return ('cdecimal', version, sufficient)


def check_dateutil():
    """Check that dateutil is installed.

    Returns:
      A triple of (package-name, version-number, sufficient) as per
      check_dependencies().
    """
    try:
        import dateutil
        # Note: There is no method to obtain the version number.
        version, sufficient = dateutil.__version__, True
    except ImportError:
        version, sufficient = None, False
    return ('dateutil', version, sufficient)


def check_bottle():
    """Check that bottle.py is installed.

    Returns:
      A triple of (package-name, version-number, sufficient) as per
      check_dependencies().
    """
    try:
        import bottle
        version, sufficient = bottle.__version__, True
    except ImportError:
        version, sufficient = None, False
    return ('bottle', version, sufficient)


def check_wget():
    """Check that wget is installed.

    Returns:
      A triple of (package-name, version-number, sufficient) as per
      check_dependencies().
    """
    version, sufficient = None, False
    try:
        pipe = subprocess.Popen(('wget', '--version'),
                                shell=False,
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE)
        stdout, stderr = pipe.communicate()
        mo = re.search(r'\b(\d+\.\d[\d\.]*)', (stdout + stderr).decode())
        if mo:
            version, sufficient = mo.group(1), True
    except FileNotFoundError as e:
        pass
    return ('wget', version, sufficient)

"""Support for encrypted tests."""

from __future__ import annotations

__copyright__ = "Copyright (C) 2015-2018, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import contextlib
import re
import subprocess
from os import path
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from pathlib import Path


def is_gpg_installed() -> bool:
    """Return true if GPG 1.4.x or 2.x are installed, which is what we use and support."""
    try:
        pipe = subprocess.Popen(
            ["gpg", "--version"],
            shell=False,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        out, err = pipe.communicate()
        version_text = out.decode("utf8")
        return pipe.returncode == 0 and bool(
            re.match(r"gpg \(GnuPG\) (1\.4|2)\.", version_text)
        )
    except OSError:
        return False


def is_encrypted_file(filename: str | Path) -> bool:
    """Return true if the given filename contains an encrypted file.

    Args:
      filename: A path string.
    Returns:
      A boolean, true if the file contains an encrypted file.
    """
    _, ext = path.splitext(filename)
    if ext == ".gpg":
        return True
    if ext == ".asc":
        # python will still raise UnicodeDecodeError if file content is not in ascii encoding
        with contextlib.suppress(UnicodeDecodeError):
            with open(filename, encoding="ascii") as encfile:
                head = encfile.read(1024)
                if re.search("--BEGIN PGP MESSAGE--", head):
                    return True
    return False


def read_encrypted_file(filename: str | Path) -> str:
    """Decrypt and read an encrypted file without temporary storage.

    Args:
      filename: A string, the path to the encrypted file.
    Returns:
      A string, the contents of the file.
    Raises:
      OSError: If we could not properly decrypt the file.
    """
    command = ["gpg", "--batch", "--decrypt", path.realpath(filename)]
    pipe = subprocess.Popen(
        command, shell=False, stdout=subprocess.PIPE, stderr=subprocess.PIPE
    )
    contents, errors = pipe.communicate()
    if pipe.returncode != 0:
        raise OSError(
            "Could not decrypt file ({}): {}".format(pipe.returncode, errors.decode("utf8"))
        )
    return contents.decode("utf-8")

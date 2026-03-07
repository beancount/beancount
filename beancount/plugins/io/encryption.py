import io

from beancount.utils import encryption

__plugins__ = ["read_encrypted_file"]


def read_encrypted_file(file_io, filename):
    """If the file is encrypted, decrypt it and return the new IO object."""
    if filename and encryption.is_encrypted_file(filename):
        contents = encryption.read_encrypted_file(filename)
        yield (io.BytesIO(contents.encode("utf-8")), filename, [])
    else:
        yield (file_io, filename, [])

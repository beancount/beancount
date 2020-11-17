__copyright__ = "Copyright (C) 2015-2016  Martin Blais"
__license__ = "GNU GPLv2"

import os
import unittest
import subprocess
import tempfile
from os import path

from beancount.utils import encryption
from beancount.utils import test_utils


TEST_PUBLIC_KEY = """
-----BEGIN PGP PUBLIC KEY BLOCK-----
Version: GnuPG v1

mI0EVnD1IwEEAKYBJGvvR+l+ZqQQVMSign2ZGakvaxmXH5bEJOhSgfUvp9ybhD/e
590Y2nECKLNlECsn3QtPAQKrmox2ub5Y35Xr3yPL7PBieBlaPfINvBzHmQq27NXq
Wj1MoUY6qrXGdCXKK7GTL6ALZ2Dt7OZOQ65W4tNezr7Dv4NEmlKv+2nLABEBAAG0
KUJlYW5jb3VudCBUZXN0IDxiZWFuY291bnQtdGVzdEBmdXJpdXMuY2E+iLgEEwEC
ACIFAlZw9SMCGwMGCwkIBwMCBhUIAgkKCwQWAgMBAh4BAheAAAoJEEQJo5QLow+k
4mUD/1yAc/vkrfuBCokYueFI6DSDxZhZcgfJig+facE1HIGnwdO0rmNfwY/AUj4p
qcxwSS96Of9MM+acxl2ucYAYqJfKRf9CMhAuFIlmAB1YhQC5LugRFZYCvL/4Trh8
gHrpw1yGSEl7HRKIyk8sOwovKD7JNn0TN+fwHpyoKfbywwu2uI0EVnD1IwEEANt1
QrSw5Y4GMgr3vP96mWD2mjb+NEwiFBv2hl8Kj5bm2JeEHWd32efm1ba8wPl9t7iw
8vFk5G4LUor2G9V3JwxVTi82ktarogaqxR7hGs8f7e2Wb+D22RAYBDn69bUUS/94
552eda0FIzy72CDfw5R8rBHtZPZ51A19dabwL5KNABEBAAGInwQYAQIACQUCVnD1
IwIbDAAKCRBECaOUC6MPpBgRA/0W71unsNiJtBXk1gbPZh5IZNG/2Q1rnmvRjkYE
ru4YvPNN9RzmDjaqSBK4qkDQkqddkWGPaB0yM7frMn1l2YE2xWaP93WwMxcQk2tz
0lSHfCkGNdOVbFgSkc/RsZ79h6INTNiQlkvLjUObcj/jyZ2kY9DfrFDNFdAmty/Z
xMFw1g==
=B5/O
-----END PGP PUBLIC KEY BLOCK-----
"""

TEST_SECRET_KEY = """
-----BEGIN PGP PRIVATE KEY BLOCK-----
Version: GnuPG v1

lQHYBFZw9SMBBACmASRr70fpfmakEFTEooJ9mRmpL2sZlx+WxCToUoH1L6fcm4Q/
3ufdGNpxAiizZRArJ90LTwECq5qMdrm+WN+V698jy+zwYngZWj3yDbwcx5kKtuzV
6lo9TKFGOqq1xnQlyiuxky+gC2dg7ezmTkOuVuLTXs6+w7+DRJpSr/tpywARAQAB
AAP+OnzrirXfRea5njN80l0ZkcwH7QKefwEuaY3JHSu4yxzTzVfj1Cci1VUE3c0v
bul8NWBRssLOrdStWrEYB3dDqvr9XVVuVsk1KdhSvSm5FCcm5IFUcRcVneUXVFlC
3p/VHKJBdmyYCDNFK1yaURSPZk6ldN9aL+BXrhOVlfyZc2ECAMellQgQcTOjagC0
b4qStWGViEj7VF0qhTETCwB8H+9EbFZmot7ahkimn4t9DBoscFAGNyjQooQFGVr4
H1+j+asCANTckxXt9MTlmlRNoU5vi8WSB1jGEZ5oT+DThkuJNGKgz22BMAIyEY8H
ZMBOlZgYE0UfUyrmTIPZt93aIIfdcGEB/0hRMuuNmQ8JHT+RLABRXcBvrMO1zrlZ
5UQ5uGywzHNP+PWIWS/4YsOrMV7kVGr9VzYVxFjnxss5wbcGzrPos9uaprQpQmVh
bmNvdW50IFRlc3QgPGJlYW5jb3VudC10ZXN0QGZ1cml1cy5jYT6IuAQTAQIAIgUC
VnD1IwIbAwYLCQgHAwIGFQgCCQoLBBYCAwECHgECF4AACgkQRAmjlAujD6TiZQP/
XIBz++St+4EKiRi54UjoNIPFmFlyB8mKD59pwTUcgafB07SuY1/Bj8BSPimpzHBJ
L3o5/0wz5pzGXa5xgBiol8pF/0IyEC4UiWYAHViFALku6BEVlgK8v/hOuHyAeunD
XIZISXsdEojKTyw7Ci8oPsk2fRM35/AenKgp9vLDC7adAdgEVnD1IwEEANt1QrSw
5Y4GMgr3vP96mWD2mjb+NEwiFBv2hl8Kj5bm2JeEHWd32efm1ba8wPl9t7iw8vFk
5G4LUor2G9V3JwxVTi82ktarogaqxR7hGs8f7e2Wb+D22RAYBDn69bUUS/94552e
da0FIzy72CDfw5R8rBHtZPZ51A19dabwL5KNABEBAAEAA/4uO8QvmnPmCZaZ2mrR
/NNayuEe1EuSvXw0julslOY+wYfgVcg3D9gTo70i6B2UgiCGM5se8WUxbMe5OFOR
U26hZeb+zgRiPuKW2koLhorhg3yvYP53pce7ub4QLL5tFwqThnXtODVqhUg3Ps6K
FqRSGYJbCrJWNh092HaVLRbwKQIA6bv2vzG7K8RxY5vLYwWSHUZU3r0egBBaibzw
t9VUxZvgPVclm5dBb2Cdexcqq/tkfnFguNXc040pHizuU161FQIA8F0lWFK8xkHj
maVYbqdFd8KU77cpH+LZIz3MjMuUOrAOTa9f2CMW89wBTjDw/Rvk2M7zSWusZfrf
PdwDcrg1mQH+KAqko8y82Acg6fyWLds1o9mHmktOG3205vAnPL3ANF1l8+fzKLIl
qQ2aNq/hAVhAfhKFOaR07e823wXwO8bnz6ShiJ8EGAECAAkFAlZw9SMCGwwACgkQ
RAmjlAujD6QYEQP9Fu9bp7DYibQV5NYGz2YeSGTRv9kNa55r0Y5GBK7uGLzzTfUc
5g42qkgSuKpA0JKnXZFhj2gdMjO36zJ9ZdmBNsVmj/d1sDMXEJNrc9JUh3wpBjXT
lWxYEpHP0bGe/YeiDUzYkJZLy41Dm3I/48mdpGPQ36xQzRXQJrcv2cTBcNY=
=XVNf
-----END PGP PRIVATE KEY BLOCK-----
"""


INPUT = """\
;; -*- mode: beancount -*-

2015-01-01 open Assets:Checking
2015-01-01 open Income:Salary

2015-12-15 * "Something"
  Assets:Checking              100.00 USD
  Income:Salary

"""


class TestEncryptedBase(unittest.TestCase):

    def setUp(self):
        self.tmpdir = tempfile.TemporaryDirectory(prefix='beancount.')
        self.ringdir = path.join(self.tmpdir.name, 'keyring')
        os.makedirs(self.ringdir)
        os.chmod(self.ringdir, 0o700)

        # Import secret and public keys.
        self.run_gpg('--import', stdin=TEST_PUBLIC_KEY.encode('ascii'))
        self.run_gpg('--import', stdin=TEST_SECRET_KEY.encode('ascii'))

    def tearDown(self):
        try:
            self.tmpdir.cleanup()
        except FileNotFoundError:
            pass  # Ignore those, GPG agent sometimes causes this problem.

    def run_gpg(self, *args, **kw):
        command = ('gpg',
                   '--batch',
                   '--armor',
                   '--homedir', self.ringdir,
                   '--trust-model', 'always') + args
        stdin = kw.pop('stdin', None)
        pipe = subprocess.Popen(command, shell=False,
                                stdin=subprocess.PIPE if stdin else None,
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE)
        out, err = pipe.communicate(stdin)
        if pipe.returncode != 0:
            raise OSError('Error running command "{}":\n{}\n'.format(' '.join(command),
                                                                     err.decode('utf8')))
        return out.decode('utf8'), err.decode('utf8')

    def encrypt_as_file(self, string, encrypted_filename):
        # Encrypt the Beancount plaintext file with it.
        out, err = self.run_gpg('--recipient', 'beancount-test', '--encrypt', '--output=-',
                                stdin=string.encode('utf8'))
        with open(encrypted_filename, 'w') as encfile:
            encfile.write(out)


class TestEncryptedFiles(TestEncryptedBase):

    @unittest.skipIf(not encryption.is_gpg_installed(), "gpg is not installed")
    def test_read_encrypted_file(self):
        encrypted_file = path.join(self.tmpdir.name, 'test.beancount.asc')
        self.encrypt_as_file(INPUT, encrypted_file)

        with test_utils.environ('GNUPGHOME', self.ringdir):
            plaintext = encryption.read_encrypted_file(encrypted_file)
            self.assertEqual(INPUT, plaintext)



class TestEncryptedFilesCheck(unittest.TestCase):

    def test_is_encrypted_file(self):
        with tempfile.NamedTemporaryFile(suffix='.txt') as file:
            file.write(b'\n')
            file.flush()
            self.assertFalse(encryption.is_encrypted_file(file.name))

        with tempfile.NamedTemporaryFile(suffix='.gpg') as file:
            file.flush()
            self.assertTrue(encryption.is_encrypted_file(file.name))

        with tempfile.NamedTemporaryFile(suffix='.asc') as file:
            file.write(b'Anything else\n')
            file.flush()
            self.assertFalse(encryption.is_encrypted_file(file.name))

        with tempfile.NamedTemporaryFile(suffix='.asc') as file:
            file.write(b'\n\n\n')
            file.write(b'-----BEGIN PGP MESSAGE-----\n')
            file.write(b'\n\n\n')
            file.flush()
            self.assertTrue(encryption.is_encrypted_file(file.name))


if __name__ == '__main__':
    unittest.main()

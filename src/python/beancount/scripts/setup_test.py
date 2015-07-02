"""A test for setup.py.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import os
import shutil
import sys
import tempfile
import subprocess
from os import path

from beancount.utils import test_utils


class TestSetup(test_utils.TestCase):

## You need to test this with setuptools and with distutils.
## Test with setuptools like this (skip the test if setuptools is not installed):
    """
    rm -rf /tmp/install && mkdir -p /tmp/install/lib/python3.4/site-packages &&  PYTHONPATH=/tmp/install/lib/python3.4/site-packages   python3 setup.py install  --prefix=/tmp/install
    """

    def test_setup(self):
        rootdir = test_utils.find_repository_root(__file__)
        installdir = tempfile.mkdtemp(prefix='beancount-setup-test.')
        try:
            # Install in a temporary directory.
            command = [sys.executable,
                       path.join(rootdir, 'setup.py'),
                       'install',
                       '--prefix={}'.format(installdir)]
            pipe = subprocess.Popen(command, shell=False,
                                    stdout=subprocess.PIPE,
                                    stderr=subprocess.PIPE,
                                    cwd=rootdir)
            stdout, stderr = pipe.communicate()
            self.assertEqual(0, pipe.returncode, stderr)

            self.assertTrue(path.exists(path.join(installdir, 'bin')))
            self.assertTrue(path.exists(path.join(installdir, 'lib')))
            self.assertGreater(len(list(os.walk(installdir))), 20)

            # Find bin and library dirs.
            bindir = path.join(installdir, 'bin')
            libdir = path.join(installdir, 'lib')
            while path.basename(libdir) != 'site-packages':
                libdir = path.join(libdir, os.listdir(libdir)[0])

            # Run some basic commands using the newly installed version.
            example_filename = path.join(rootdir, 'examples/example.beancount')

            # Run bean-check.
            command = [path.join(bindir, 'bean-check'), '-v', example_filename]
            pipe = subprocess.Popen(command, shell=False,
                                    stdout=subprocess.PIPE,
                                    stderr=subprocess.PIPE,
                                    env={'PYTHONPATH': path.join(libdir)},
                                    cwd=rootdir)
            stdout, stderr = pipe.communicate()
            self.assertEqual(0, pipe.returncode, stderr)

            # Run bean-report.
            command = [path.join(bindir, 'bean-report'), example_filename, 'balsheet']
            pipe = subprocess.Popen(command, shell=False,
                                    stdout=subprocess.PIPE,
                                    stderr=subprocess.PIPE,
                                    env={'PYTHONPATH': path.join(libdir)},
                                    cwd=rootdir)
            stdout, stderr = pipe.communicate()
            self.assertEqual(0, pipe.returncode, stderr)

            # Run bean-query.
            command = [path.join(bindir, 'bean-query'), example_filename, 'balances;']
            pipe = subprocess.Popen(command, shell=False,
                                    stdout=subprocess.PIPE,
                                    stderr=subprocess.PIPE,
                                    env={'PYTHONPATH': path.join(libdir)},
                                    cwd=rootdir)
            stdout, stderr = pipe.communicate()
            self.assertEqual(0, pipe.returncode, stderr)
        finally:
            shutil.rmtree(installdir)

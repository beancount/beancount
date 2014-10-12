"""A test for setup.py.
"""
import os
import shutil
import sys
import tempfile
import subprocess
from os import path

from beancount.utils import test_utils
from beancount.utils import file_utils
from beancount.scripts import bake


class TestSetup(test_utils.TestCase):

    def test_setup(self):
        rootdir = test_utils.find_repository_root(__file__)
        tmpdir = tempfile.mkdtemp()
        try:
            print(tmpdir)

            command = [sys.executable,
                       path.join(rootdir, 'setup.py'),
                       'install',
                       '--prefix={}'.format(tmpdir)]
            print(' '.join(command))
            pipe = subprocess.Popen(command, shell=False,
                                    stdout=subprocess.PIPE,
                                    stderr=subprocess.PIPE,
                                    cwd=rootdir)
            stdout, stderr = pipe.communicate()
            self.assertEqual(0, pipe.returncode, stderr)

            self.assertTrue(path.exists(path.join(tmpdir, 'bin')))
            self.assertTrue(path.exists(path.join(tmpdir, 'lib')))
            self.assertGreater(len(list(os.walk(tmpdir))), 20)
        finally:
            shutil.rmtree(tmpdir)

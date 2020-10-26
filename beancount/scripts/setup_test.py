"""A test for setup.py.
"""
__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

from os import path
import os
import re
import shutil
import subprocess
import sys
import tarfile
import tempfile
import unittest

from beancount.utils import test_utils


def is_bazel_build():
    "Return true if this is invoked from Bazel."
    return "RUNFILES_DIR" in os.environ


class TestSetup(test_utils.TestCase):

    def setUp(self):
        self.installdir = tempfile.mkdtemp(prefix='beancount-setup-test.')

    def tearDown(self):
        if path.exists(self.installdir):
            shutil.rmtree(self.installdir)

    @unittest.skipIf(is_bazel_build(), "Cannot setup within Bazel.")
    def test_setup(self):
        # We need to create the installation target directory and have our
        # PYTHONPATH set on it in order for setuptools to work properly in a
        # temporary installation directory. Otherwise it fails and spits out a
        # large error message with instructions on how to work with setuptools.
        site_packages_path = path.join(
            self.installdir,
            'lib/python{vi.major:d}.{vi.minor:d}/site-packages'.format(
                vi=sys.version_info))
        os.makedirs(site_packages_path)
        self.run_setup(self.installdir, {'PYTHONPATH': site_packages_path})

        # Setuptools will leave some crud in the installation source. Clean this
        # up so as not to be annoying.
        rootdir = test_utils.find_repository_root(__file__)
        egg_info = path.join(rootdir, 'beancount.egg-info')
        if path.exists(egg_info):
            shutil.rmtree(egg_info)

    def run_setup(self, installdir, extra_env=None):
        """Run setup.py with the given extra environment variables.

        Args:
          installdir: A string, the name of the temporary directory to install into.
          extra_env: A dict, extra environment variables to set in the subprocess.
        """
        rootdir = test_utils.find_repository_root(__file__)

        # Clean previously built "build" output.
        command = [sys.executable, 'setup.py', 'clean', '--all']
        subprocess_env = os.environ.copy()
        if extra_env:
            subprocess_env.update(extra_env)
        pipe = subprocess.Popen(command, shell=False,
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE,
                                cwd=rootdir,
                                env=subprocess_env)
        stdout, stderr = pipe.communicate()
        self.assertEqual(0, pipe.returncode, stderr)

        # Install in a temporary directory.
        command = [sys.executable, 'setup.py', 'install', '--prefix={}'.format(installdir)]
        subprocess_env = os.environ.copy()
        if extra_env:
            subprocess_env.update(extra_env)
        pipe = subprocess.Popen(command, shell=False,
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE,
                                cwd=rootdir,
                                env=subprocess_env)
        stdout, stderr = pipe.communicate()
        self.assertEqual(0, pipe.returncode, stderr)

        self.assertTrue(path.exists(path.join(installdir, 'bin')))
        self.assertTrue(path.exists(path.join(installdir, 'lib')))
        self.assertGreater(len(list(os.walk(installdir))), 20)

        # Note: We used to run the commands with --help in the past, but after
        # changes in the installation made it difficult to programmatically set
        # PYTHONPATH explicitly, the tests were removed. The Right Thing would
        # be to create a test with pytest-virtual end here.

    @unittest.skipIf(is_bazel_build(), "Cannot setup within Bazel.")
    def test_sdist_includes_c_files(self):
        # Clean previously built "build" output.
        rootdir = test_utils.find_repository_root(__file__)
        subprocess.check_call(
            [sys.executable, 'setup.py', 'sdist', '--dist-dir', self.installdir],
            cwd=rootdir, shell=False,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        files = os.listdir(self.installdir)
        self.assertEqual(1, len(files))
        targz = path.join(self.installdir, files[0])

        # Find the set of expected header & C files.
        exp_filenames = set()
        for root, dirs, files in os.walk(path.join(rootdir, 'beancount')):
            for filename in files:
                if re.match(r'.*\.[hc]$', filename):
                    exp_filenames.add(path.join(root[len(rootdir)+1:], filename))

        # Find the set of packaged files in the source distribution.
        tar = tarfile.open(targz)
        tar_filenames = set(re.sub('^.*?{}'.format(os.sep), '', info.name)
                            for info in tar
                            if re.match(r'.*\.[hc]$', info.name))

        # Check that all the expected files are present.
        self.assertLessEqual(exp_filenames, tar_filenames)


if __name__ == '__main__':
    unittest.main()

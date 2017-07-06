__copyright__ = "Copyright (C) 2014, 2016  Martin Blais"
__license__ = "GNU GPLv2"

import os
import shutil
import tempfile
from os import path

from beancount.utils import test_utils
from beancount.scripts import tutorial


class TestTutorial(test_utils.TestCase):

    def test_generate_tutorial(self):
        rootdir = test_utils.find_repository_root(__file__)
        example_beancount = path.join(rootdir, 'examples', 'example.beancount')
        temp_output_dir = tempfile.mkdtemp()
        try:
            with test_utils.capture('stdout', 'stderr'):
                result = test_utils.run_with_args(tutorial.main,
                                                  [example_beancount, temp_output_dir])
            self.assertEqual(0, result)
            output_files = os.listdir(temp_output_dir)
            self.assertLess(20, len(output_files))
        finally:
            shutil.rmtree(temp_output_dir)

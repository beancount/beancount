import unittest
import re
from os import path
import shelve
import os
import io

from beancount.utils import sample_stack


class TestSampleStack(unittest.TestCase):

    # def test_sample(self):
    #   sample_stack.sample('/tmp/samples.db')
    #   sample_stack.sample('/tmp/samples.db')
    #   sample_stack.sample('/tmp/samples.db')

    filename = '/tmp/samples.db'

    def deleteSamples(self):
        if path.exists(self.filename):
            os.remove(self.filename)
    #setUp = tearDown = deleteSamples
    setUp = deleteSamples

    def test_sample(self):
        N = 10
        for i in range(N):
            sample_stack.sample(self.filename)

        with shelve.open(self.filename, 'r') as db:
            items = list(db.items())
        self.assertEqual(1, len(items))
        self.assertEqual(N, items[0][1])

    def test_print_samples(self):
        sample_stack.sample(self.filename)
        oss = io.StringIO()
        sample_stack.print_samples(self.filename, file=oss)
        printed = oss.getvalue()
        self.assertTrue(printed)
        self.assertTrue(re.search('Seen', printed))

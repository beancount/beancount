import functools
import sys
import os
import pprint
import tempfile
import unittest

from experiments.v3.protos import expose_protos as ep


class ExportProtosTests(unittest.TestCase):

  def test_simple(self):
    pr = functools.partial(print, file=sys.stderr)
    pr(ep.__doc__)
    #pprint.pprint(dir(ep))
    pr(ep.Balance)
    #pr(ep.Open)
    pr(ep)
    balance = ep.Balance()
    #pr(balance)
    balance.account = "Assets:Blablabla"
    #print(balance)


if __name__ == '__main__':
  unittest.main()

__copyright__ = "Copyright (C) 2015-2017, 2019, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount.utils import invariants


class Dummy:
    """Just a dummy class as a target to instrument."""

    counter = 0

    def do_something(self):
        pass


class TestInvariants(unittest.TestCase):
    def setUp(self):
        incr = lambda obj: setattr(obj, "counter", obj.counter + 1)
        invariants.instrument_invariants(Dummy, incr, incr)

    def tearDown(self):
        invariants.uninstrument_invariants(Dummy)

    def test_invariants_on_dummy(self):
        dummy = Dummy()
        dummy.do_something()
        self.assertEqual(2, dummy.counter)


if __name__ == "__main__":
    unittest.main()

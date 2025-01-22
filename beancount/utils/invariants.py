"""Functions to register auxiliary functions on a class' methods to check for invariants.

This is intended to be used in a test, whereby your test will setup a class to
automatically run invariant verification functions before and after each
function call, to ensure some extra sanity checks that wouldn't be used in
non-tests.

Example: Instrument the Inventory class with the check_inventory_invariants()
function.

  def setUp(module):
      instrument_invariants(Inventory,
                            check_inventory_invariants,
                            check_inventory_invariants)

  def tearDown(module):
      uninstrument_invariants(Inventory)

"""

__copyright__ = "Copyright (C) 2015-2017, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import types


def invariant_check(method, prefun, postfun):
    """Decorate a method with the pre/post invariant checkers.

    Args:
      method: An unbound method to instrument.
      prefun: A function that checks invariants pre-call.
      postfun: A function that checks invariants post-call.
    Returns:
      An unbound method, decorated.
    """
    reentrant = []

    def new_method(self, *args, **kw):
        reentrant.append(None)
        if len(reentrant) == 1:
            prefun(self)
        result = method(self, *args, **kw)
        if len(reentrant) == 1:
            postfun(self)
        reentrant.pop()
        return result

    return new_method


def instrument_invariants(klass, prefun, postfun):
    """Instrument the class 'klass' with pre/post invariant
    checker functions.

    Args:
      klass: A class object, whose methods to be instrumented.
      prefun: A function that checks invariants pre-call.
      postfun: A function that checks invariants pre-call.
    """
    instrumented = {}
    for attrname, object_ in klass.__dict__.items():
        if attrname.startswith("_"):
            continue
        if not isinstance(object_, types.FunctionType):
            continue
        instrumented[attrname] = object_
        setattr(klass, attrname, invariant_check(object_, prefun, postfun))
    klass.__instrumented = instrumented


def uninstrument_invariants(klass):
    """Undo the instrumentation for invariants.

    Args:
      klass: A class object, whose methods to be uninstrumented.
    """
    instrumented = getattr(klass, "__instrumented", None)
    if instrumented:
        for attrname, object_ in instrumented.items():
            setattr(klass, attrname, object_)
    del klass.__instrumented

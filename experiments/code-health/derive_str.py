#!/usr/bin/env python3
"""
Can you specialize a str type?
"""
__author__ = "Martin Blais <blais@furius.ca>"

class tag(str):
    pass

class payee(str):
    pass

ptown_tag = tag("trip-provincetown")
tmobile_payee = payee("TMobile")

print(ptown_tag)
print(isinstance(ptown_tag, str))
print(isinstance(ptown_tag, tag))
print(isinstance(ptown_tag, payee))
print()

# Checking whether the short-strings optimization still applies...
a = "ptown"
b = "ptown"
print(a is b)

a = tag("ptown")
b = tag("ptown")
print(a is b)

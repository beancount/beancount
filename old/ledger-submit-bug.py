#!/usr/bin/env python

Username = username
Password = password

import re
import sys

print "Please enter a description for this bug:"
desc = sys.stdin.read()

print "Thank you; your bug is now being submitted."

from mechanize import Browser

br = Browser()
br.add_password("http://trac.newartisans.com/ledger/login",
                Username, Password)

print "Logging in to the Trac ..."
br.open("http://trac.newartisans.com/ledger/login")
assert br.viewing_html()

# follow second link with element text matching regular expression
print "Opening the New Ticket page ..."
resp1 = br.open("http://trac.newartisans.com/ledger/newticket")

newticket = None
index = 0
for form in br.forms():
    if index == 1:
        newticket = form
        break
    index += 1
br.form = newticket

br["summary"]     = sys.argv[1]
br["description"] = desc
br["owner"]       = ["johnw"]

print "Submitting the ticket ..."
br.submit(nr=1)                 # submit the bug!

print "Done!  Your bug is entered."

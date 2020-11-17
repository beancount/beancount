========================================================
   beancount: Double-Entry Accounting from Text Files
========================================================

.. contents::
..
    1  Description
    2  Documentation
    3  Download & Installation
    4  Filing Bugs
    5  Copyright and License
    6  Author


Description
===========

A double-entry bookkeeping computer language that lets you define financial
transaction records in a text file, read them in memory, generate a variety of
reports from them, and provides a web interface.


Documentation
=============

Documentation can be read at:

  https://beancount.github.io/docs/

Documentation authoring happens on Google Docs, where you can contribute by
requesting access or commenting on individual documents. An index of all source
documents is available here:

  http://furius.ca/beancount/doc/index

There's a `mailing-list dedicated to Beancount
<https://groups.google.com/forum/#!forum/beancount>`_, please post questions
there, so others can share in the responses. More general discussions about
command-line accounting also occur on the `Ledger mailing-list
<https://groups.google.com/forum/#!forum/ledger-cli>`_ so you might be
interested in that group as well.


Download & Installation
=======================

You can obtain the source code from the official Git repository on Github:

  | https://github.com/beancount/beancount/

See the `Installing Beancount`__ document for more details.

__ http://furius.ca/beancount/doc/install


Versions
========

There are three versions

- **Version 3** (`branch master
  <http://github.com/beancount/beancount/tree/master>`_): The in-development
  next version of Beancount since June 2020. This is unstable and you want to
  use version 2 below. The scope of changes is described in `this document
  <https://docs.google.com/document/d/1qPdNXaz5zuDQ8M9uoZFyyFis7hA0G55BEfhWhrVBsfc/>`_.

- **Version 2** (`branch v2 <http://github.com/beancount/beancount/tree/v2>`_):
  The current stable version of Beancount, in maintenance mode as of July 2020.
  This was a complete rewrite of the first version, which introduced a number of
  constraints and a new grammar and much more. Use this now.

- **Version 1** (`branch v1 <http://github.com/beancount/beancount/tree/v1>`_):
  The original version of Beancount. Development on this version halted in 2013.
  This initial version was intended to be similar to and partially compatible
  with Ledger. Do not use this.


Filing Bugs
===========

Tickets can be filed at on the Github project page:

  https://github.com/beancount/beancount/issues


Copyright and License
=====================

Copyright (C) 2007-2020  Martin Blais.  All Rights Reserved.

This code is distributed under the terms of the "GNU GPLv2 only".
See COPYING file for details.


Author
======

Martin Blais <blais@furius.ca>

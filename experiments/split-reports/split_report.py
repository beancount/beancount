#!/usr/bin/env python3
"""Generate final reports for a shared expenses on a trip or project.

For each of many participants, generate a detailed list of expenses,
contributions, a categorized summary of expenses, and a final balance. Also
produce a global list of final balances so that participants can reconcile
between each other.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

from beancount.plugins import split_expenses
split_expenses.main()

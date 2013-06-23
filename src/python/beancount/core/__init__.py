"""Core basic objects and data structures to represent a list of entries.
"""

# Lookup for ordering a list of currencies: we want the majors first, then the
# cross-currencies, and then all the rest of the stuff a user might define
# (shorter strings first).
CURRENCY_ORDER = {
    # Majors
    'USD': 0,
    'EUR': 1,
    'JPY': 2,
    # Commonwealth
    'CAD': 3,
    'GBP': 4,
    'AUD': 5,
    'NZD': 6,
    'CHF': 7,
    # All the rest...
}

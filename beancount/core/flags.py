"""Flag constants."""

__copyright__ = "Copyright (C) 2013-2014, 2016-2017, 2020-2021, 2024  Martin Blais"
__license__ = "GNU GPLv2"


# Special flags.
FLAG_OKAY = "*"  # Transactions that have been checked.
FLAG_WARNING = "!"  # Mark by the user as something to be looked at later on.
FLAG_PADDING = "P"  # Transactions created from padding directives.
FLAG_SUMMARIZE = "S"  # Transactions created due to summarization.
FLAG_TRANSFER = "T"  # Transactions created due to balance transfers.
FLAG_CONVERSIONS = "C"  # Transactions created to account for price conversions.
FLAG_MERGING = "M"  # A flag to mark postings merging together legs for average cost.

"""Flag constants.
"""

# Special flags
FLAG_OKAY        = '*' # Transactions that have been checked.
FLAG_WARNING     = '!' # Mark by the user as something to be looked at later on.
FLAG_PADDING     = 'P' # Transactions created from padding directives.
FLAG_SUMMARIZE   = 'S' # Transactions created due to summarization.
FLAG_TRANSFER    = 'T' # Transactions created due to balance transfers.
FLAG_CONVERSIONS = 'C' # Transactions created to account for price conversions.
FLAG_UNREALIZED  = 'U' # Transactions created due to unrealized gains.

# Default flag for imported transactions.
FLAG_IMPORT = FLAG_OKAY

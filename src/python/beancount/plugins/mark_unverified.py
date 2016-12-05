"""Add metadata to Postings which occur after their last Balance directives.

Some people use Balance directives as a way to indicate that all postings before
them are verified. They want to compute balances in each account as of the date
of that last Balance directives. One way to do that is to use this plugin to
mark the postings which occur after and to then filter them out using a WHERE
clause on that metadata:

  SELECT account, sum(position) WHERE NOT meta("unverified")

Note that doing such a filtering may result in a list of balances which may not
add to zero.

Also, postings for accounts without a single Balance directive on them will not
be marked as unverified as all (otherwise all the postings would be marked, this
would make no sense).
"""
__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

from beancount.core import data


__plugins__ = ('mark_unverified',)


def mark_unverified(entries, options_map):
    """Add metadata to postings after the last Balance entry. See module doc.

    Args:
      entries: A list of data directives.
      options_map: A dict of options, that confirms to beancount.parser.options.
    Returns:
      A list of entries, which includes the new unrealized capital gains entries
      at the end, and a list of errors. The new list of entries is still sorted.
    """
    # The last Balance directive seen for each account.
    last_balances = {}
    for entry in entries:
        if isinstance(entry, data.Balance):
            last_balances[entry.account] = entry

    new_entries = []
    for entry in entries:
        if isinstance(entry, data.Transaction):
            postings = entry.postings
            new_postings = postings
            for index, posting in enumerate(postings):
                balance = last_balances.get(posting.account, None)
                if balance and balance.date <= entry.date:
                    if new_postings is postings:
                        new_postings = postings.copy()
                    new_meta = posting.meta.copy()
                    new_meta['unverified'] = True
                    new_postings[index] = posting._replace(meta=new_meta)
            if new_postings is not postings:
                entry = entry._replace(postings=new_postings)
        new_entries.append(entry)

    return new_entries, []

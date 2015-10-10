"""An example of adding a forecasting feature to Beancount via a plugin.

This entry filter plugin uses existing syntax to define and automatically
inserted transactions in the future based on a convention. It serves mostly as
an example of how you can experiment by creating and installing a local filter,
and not so much as a serious forecasting feature (though the experiment is a
good way to get something more general kickstarted eventually, I think the
concept would generalize nicely and should eventually be added as a comon
feature of Beancount).

A user can create a create a transaction like this:

  2014-03-08 # "Electricity bill [MONTHLY]""
    Expenses:Electricity 			50.10 USD
    Assets:Checking			       -50.10 USD

and new transactions will be created monthly for the following year.
Note the use of the '#' flag and the word 'MONTHLY' which defines the
periodicity. This needs more expansion, but as an example, it works.
"""

__author__ = 'Martin Blais <blais@furius.ca>'

import re

from beancount.core import data


__plugins__ = ('forecast_plugin',)


def forecast_plugin(entries, options_map):
    """An example filter that piggybacks on top of the Beancount input syntax to
    insert forecast entries automatically. This functions accepts the return
    value of beancount.loader.load_file() and must return the same type of output.

    Args:
      entries: a list of entry instances
      options_map: a dict of options parsed from the file
    Returns:
      A tuple of entries and errors.
    """

    # Find the last entry's date.
    date_today = entries[-1].date

    # Filter out forecast entries from the list of valid entries.
    forecast_entries = []
    filtered_entries = []
    for entry in entries:
        outlist = (forecast_entries
                   if (isinstance(entry, data.Transaction) and entry.flag == '#')
                   else filtered_entries)
        outlist.append(entry)

    # Generate forecast entries up to the end of the current year.
    new_entries = []
    for entry in forecast_entries:
        # Parse the periodicity.
        match = re.search(r'(.*)\[MONTHLY\]', entry.narration)
        if match:
            forecast_narration = match.group(1).strip()
            for month in range(date_today.month + 1, 13):
                # Create a new entry at the given datė
                forecast_date = date_today.replace(month=month)
                forecast_entry = entry._replace(date=forecast_date,
                                                narration=forecast_narration)
                new_entries.append(forecast_entry)

    # Make sure the new entries inserted are sorted.
    new_entries.sort(key=data.entry_sortkey)

    return (filtered_entries + new_entries, [])

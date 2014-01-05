#!/usr/bin/env python3
from datetime import date
import re

from beancount.web import web
from beancount.core import data

def forecast_filter(entries, errors, options):
    """An example filter that piggybacks on top of the Beancount input syntax to
    insert forecast entries automatically. This functions accepts the return
    value of beancount.loader.load() and must return the same type of output.

    Args:
      entries: a list of entry instances
      errors: a list of errors generated during parsing
      options: a dict of options parsed from the file
    Returns:
      A triple of the same, possibly modifeid.
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
        mo = re.search(r'(.*)\[MONTHLY\]', entry.narration)
        if mo:
            forecast_narration = mo.group(1).strip()
            for month in range(date_today.month + 1, 13):
                forecast_date = date_today.replace(month=month)
                forecast_entry = data.entry_replace(entry,
                                                    date=forecast_date,
                                                    narration=forecast_narration)
                new_entries.append(forecast_entry)

                print('Created forecast entry at {}: "{}"'.format(
                    forecast_date, forecast_entry.narration))

    # Make sure the new entries inserted are sorted.
    new_entries.sort(key=data.entry_sortkey)

    return (filtered_entries + new_entries, errors, options)


web.install_load_filter(forecast_filter)
web.main()

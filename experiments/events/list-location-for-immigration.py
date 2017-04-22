#!/usr/bin/env python3
"""Report on location for a US N400 Naturalization application.
"""
__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import datetime
import itertools
import logging
import re
import csv
import sys

from beancount import loader
from beancount.core import data


def is_usa(event):
    return event is None or re.match('.* USA$', event.description)


def extract_trips_outside_us(events):
    it1 = iter(events)
    it2 = iter(events); next(it2)
    trips = []
    current_trip = []
    for e1, e2 in itertools.zip_longest(it1, it2):
        if not is_usa(e1):
            current_trip.append(e1)
        if is_usa(e2) and current_trip:
            current_trip.append(e2)
            trips.append(current_trip)
            current_trip = []
    if current_trip:
        trips.append(current_trip)
    return trips


def main():
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('filename', help="Beancount input filename")
    parser.add_argument('detail', help="Filename to output details to")
    args = parser.parse_args()

    entries, _, options_map = loader.load_file(args.filename)

    today = datetime.date.today()
    date_start = today - datetime.timedelta(365 * 6)

    events = [entry
              for entry in entries
              if (isinstance(entry, data.Event) and
                  entry.type == 'location' and
                  entry.date >= date_start)]

    trips = extract_trips_outside_us(events)
    for trip in trips:
        e1 = trip[0]
        e2 = trip[-1]
        print("  {} / {:24} -> {} / {:24}".format(e1.date, e1.description,
                                                  e2.date, e2.description))

    print("How many total days (24 hours or longer) did you spend outside the "
          "United States during the last 5 years?")
    total_days = sum([(trip[-1].date - trip[0].date).days
                      for trip in trips])
    print("{} days ({:.1%})".format(total_days, float(total_days)/(today - date_start).days))
    print()

    print("How many trips of 24 hours of longer have you taken outside the "
          "United States during the last 5 years?")
    print("{} trips".format(len(trips)))
    print()

    print("List below all the trips of 24 hours or longer that you have taken outside the "
          "United States during the last 5 years. Begin with your most recent trip and "
          "work backwards.")
    with open(args.detail, 'w') as file:
        wr = csv.writer(file)
        total_days_ = 0
        for trip in reversed(trips):
            date_left = trip[0].date
            date_returned = trip[-1].date
            days_outside = (date_returned - date_left).days
            six_months = 'YES' if days_outside > (6 * 30) else 'NO'

            countries_raw = set(ev.description.split(',')[-1].strip() for ev in trip)
            if 'Jordan' in countries_raw:
                countries_raw.remove('Jordan')
                countries_raw.add('Jordan(Petra)')
            countries_raw.discard('USA')
            countries = ', '.join(sorted(countries_raw))

            wr.writerow((date_left, date_returned, six_months, countries, days_outside))
            total_days_ += days_outside
        assert(total_days == total_days_)
    print(open(args.detail).read())


if __name__ == '__main__':
    main()

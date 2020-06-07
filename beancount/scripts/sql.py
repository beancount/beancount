"""Convert a Beancount ledger into an SQL database.
"""
__copyright__ = "Copyright (C) 2014-2017  Martin Blais"
__license__ = "GNU GPLv2"

import sqlite3 as dbapi
import logging
import sys
import os
import itertools
from os import path
from decimal import Decimal

from beancount import loader
from beancount.core import data
from beancount.utils import misc_utils
from beancount.parser import version


def output_common(connection, unused_entries):
    """Create a table of common data for all entries.

    Args:
      connection: A DBAPI-2.0 Connection object.
      entries: A list of directives.
    """
    with connection:
        connection.execute("""
          CREATE TABLE entry (
            id 			INTEGER PRIMARY KEY,
            date 		DATE,
            type                CHARACTER(8),
            source_filename	STRING,
            source_lineno	INTEGER
          );
        """)


def output_transactions(connection, entries):
    """Create a table for transactions and fill in the data.

    Args:
      connection: A DBAPI-2.0 Connection object.
      entries: A list of directives.
    """
    with connection:
        connection.execute("""
          CREATE TABLE transactions_detail (
            id 			INTEGER PRIMARY KEY,
            flag 		CHARACTER(1),
            payee 		VARCHAR,
            narration 		VARCHAR,
            tags                VARCHAR, -- Comma-separated
            links               VARCHAR  -- Comma-separated
          );
        """)

        connection.execute("""
          CREATE VIEW transactions AS
            SELECT * FROM entry JOIN transactions_detail USING (id);
        """)

        connection.execute("""
          CREATE TABLE postings (
            posting_id		INTEGER PRIMARY KEY,
            id 			INTEGER,
            flag                CHARACTER(1),
            account             VARCHAR,
            number              DECIMAL(16, 6),
            currency            CHARACTER(10),
            cost_number         DECIMAL(16, 6),
            cost_currency       CHARACTER(10),
            cost_date           DATE,
            cost_label          VARCHAR,
            price_number        DECIMAL(16, 6),
            price_currency      CHARACTER(10),
            FOREIGN KEY(id) REFERENCES entries(id)
          );
        """)

    postings_count = iter(itertools.count())
    with connection:
        for eid, entry in enumerate(entries):
            if not isinstance(entry, data.Transaction):
                continue
            connection.execute("""
              insert into entry values (?, ?, ?, ?, ?);
            """, (eid, entry.date, 'txn', entry.meta["filename"], entry.meta["lineno"]))

            connection.execute("""
              insert into transactions_detail values (?, ?, ?, ?, ?, ?);
            """, (eid, entry.flag, entry.payee, entry.narration,
                  ','.join(entry.tags or ()), ','.join(entry.links or ())))

            for posting in entry.postings:
                pid = next(postings_count)
                units = posting.units
                cost = posting.cost
                price = posting.price
                connection.execute("""
                  INSERT INTO postings VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);
                """, (pid, eid,
                      posting.flag,
                      posting.account,
                      units.number,
                      units.currency,
                      cost.number if cost else None,
                      cost.currency if cost else None,
                      cost.date if cost else None,
                      cost.label if cost else None,
                      price.number if price else None,
                      price.currency if price else None))


class DirectiveWriter:
    """A base class for writers of directives.
    This is used to factor out code for all the simple directives types
    (all types except Transaction).
    """
    # The name of the type of the directive. Override this.
    type = None

    # A string, the columns to create as a single multiline declaration.
    columns = None

    def __init__(self):
        self.name = self.type.__name__.lower()

    def __call__(self, connection, entries):
        """Create a table for a directives.

        Args:
          connection: A DBAPI-2.0 Connection object.
          entries: A list of directives.
        """
        with connection:
            columns_text = ','.join(self.columns.strip().splitlines())
            connection.execute("""
              CREATE TABLE {name}_detail (
                id 			INTEGER PRIMARY KEY,
                {columns}
              );
            """.format(name=self.name,
                       columns=columns_text))

            connection.execute("""
              CREATE VIEW {name} AS
                SELECT * FROM entry JOIN {name}_detail USING (id);
            """.format(name=self.name))

        with connection:
            for eid, entry in enumerate(entries):
                if not isinstance(entry, self.type):
                    continue

                # Store common data.
                connection.execute("""
                  INSERT INTO entry VALUES (?, ?, ?, ?, ?);
                """, (eid, entry.date, self.name,
                      entry.meta["filename"], entry.meta["lineno"]))

                # Store detail data.
                detail_data = self.get_detail(entry)
                row_data = (eid,) + detail_data
                query = """
                  INSERT INTO {name}_detail VALUES ({placeholder});
                """.format(name=self.name,
                           placeholder=','.join(['?'] * (1 + len(detail_data))))
                connection.execute(query, row_data)

    def get_detail(self, entry):
        """Provide data to store for details table.

        Args:
          entry: An instance of the desired directive.
        Returns:
          A tuple of the values corresponding to the columns declared in the
          'columns' attribute.
        """
        raise NotImplementedError


class OpenWriter(DirectiveWriter):
    type = data.Open

    columns = """
      account             VARCHAR
      currencies          VARCHAR
    """

    def get_detail(self, entry):
        return (entry.account,
                ','.join(entry.currencies or []))


class CloseWriter(DirectiveWriter):
    type = data.Close

    columns = """
      account             VARCHAR
    """

    def get_detail(self, entry):
        return (entry.account,)


class PadWriter(DirectiveWriter):
    type = data.Pad

    columns = """
      account             VARCHAR
      source_account      VARCHAR
    """

    def get_detail(self, entry):
        return (entry.account, entry.source_account)


class BalanceWriter(DirectiveWriter):
    type = data.Balance

    columns = """
      account             VARCHAR
      amount_number       DECIMAL(16,6)
      amount_currency     CHARACTER(10)
      diff_number         DECIMAL(16,6)
      diff_currency       CHARACTER(10)
    """

    def get_detail(self, entry):
        return (entry.account,
                entry.amount.number,
                entry.amount.currency,
                entry.diff_amount.currency if entry.diff_amount else None,
                entry.diff_amount.currency if entry.diff_amount else None)


class NoteWriter(DirectiveWriter):
    type = data.Note

    columns = """
      account             VARCHAR
      comment             VARCHAR
    """

    def get_detail(self, entry):
        return (entry.account,
                entry.comment)


class EventWriter(DirectiveWriter):
    type = data.Event

    columns = """
      type                VARCHAR
      description         VARCHAR
    """

    def get_detail(self, entry):
        return (entry.type,
                entry.description)


class QueryWriter(DirectiveWriter):
    type = data.Query

    columns = """
      name                VARCHAR
      query_string        VARCHAR
    """

    def get_detail(self, entry):
        return (entry.name,
                entry.query_string)


class PriceWriter(DirectiveWriter):
    type = data.Price

    columns = """
      currency            CHARACTER(10)
      amount_number       DECIMAL(16,6)
      amount_currency     CHARACTER(10)
    """

    def get_detail(self, entry):
        return (entry.currency,
                entry.amount.number,
                entry.amount.currency)


class DocumentWriter(DirectiveWriter):
    type = data.Document

    columns = """
      account             VARCHAR
      filenam             VARCHAR
    """

    def get_detail(self, entry):
        return (entry.account,
                entry.filename)



def adapt_decimal(number):
    """Adapt a Decimal instance to a string for creating queries.

    Args:
      number: An instance of Decimal.
    Returns:
      A string.
    """
    return str(number)


def convert_decimal(string):
    """Convert a Decimal string to a Decimal instance.

    Args:
      string: A decimal number in a string.
    Returns:
      An instance of Decimal.
    """
    return Decimal(string)


def setup_decimal_support():
    """Setup sqlite3 to support conversions to/from Decimal numbers.
    """
    dbapi.register_adapter(Decimal, adapt_decimal)
    dbapi.register_converter("decimal", convert_decimal)


def main():
    parser = version.ArgumentParser(description=__doc__)
    parser.add_argument('filename',
                        help='Beancount input filename')
    parser.add_argument('database',
                        help='Filename of database file to create')
    args = parser.parse_args()
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')

    entries, errors, options_map = loader.load_file(args.filename,
                                                    log_timings=logging.info,
                                                    log_errors=sys.stderr)

    # Delete previous database if it already exists.
    if path.exists(args.database):
        os.remove(args.database)

    # The only supported DBAPI-2.0 backend for now is SQLite3.
    connection = dbapi.connect(args.database)

    setup_decimal_support()
    for function in [
            output_common,
            output_transactions,
            OpenWriter(),
            CloseWriter(),
            PadWriter(),
            BalanceWriter(),
            NoteWriter(),
            PriceWriter(),
            DocumentWriter(),
    ]:
        step_name = getattr(function, '__name__', function.__class__.__name__)
        with misc_utils.log_time(step_name, logging.info):
            function(connection, entries)

    return 0

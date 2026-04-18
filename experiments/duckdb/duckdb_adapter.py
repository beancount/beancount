__copyright__ = "Copyright (C) 2026  Martin Blais"
__license__ = "GNU GPLv2"

import json
import logging
import os
from typing import Optional

import click
import duckdb

from beancount import loader
from beancount.core import amount
from beancount.core import convert
from beancount.core import data
from beancount.parser.version import VERSION


def setup_database(conn: duckdb.DuckDBPyConnection):
    """Define custom types and create the tables."""

    # Define custom types
    conn.execute("""
        CREATE TYPE Amount AS STRUCT(
            number DECIMAL(38,18),
            currency VARCHAR
        );

        CREATE TYPE Cost AS STRUCT(
            number DECIMAL(38,18),
            currency VARCHAR,
            date DATE,
            label VARCHAR
        );

        CREATE TYPE BeancountPosition AS STRUCT(
            units Amount,
            cost Cost
        );
    """)

    # Create postings table
    conn.execute("""
        CREATE TABLE postings (
            -- Transaction Info
            id VARCHAR,
            date DATE,
            tx_flag VARCHAR,
            payee VARCHAR,
            narration VARCHAR,
            description VARCHAR,
            tags VARCHAR[],
            links VARCHAR[],
            tx_meta JSON,
            
            -- Posting Info
            account VARCHAR,
            number DECIMAL(38,18),
            currency VARCHAR,
            posting_flag VARCHAR,
            posting_meta JSON,
            
            -- Rich Objects
            units Amount,
            cost Cost,
            pos BeancountPosition,
            price Amount,
            weight Amount,
            
            -- Context
            filename VARCHAR,
            lineno INTEGER,
            year INTEGER,
            month INTEGER,
            day INTEGER
        );
    """)

    # Create prices table
    conn.execute("""
        CREATE TABLE prices (
            date DATE,
            currency VARCHAR,
            amount Amount
        );
    """)

    # Create accounts table
    conn.execute("""
        CREATE TABLE accounts (
            name VARCHAR,
            open_date DATE,
            close_date DATE,
            currencies VARCHAR[],
            meta JSON
        );
    """)

    # Create commodities table
    conn.execute("""
        CREATE TABLE commodities (
            currency VARCHAR,
            meta JSON
        );
    """)


def to_duck_amount(amt: Optional[amount.Amount]):
    if amt is None:
        return None
    return {"number": amt.number, "currency": amt.currency}


def to_duck_cost(cst: Optional[data.Cost]):
    if cst is None:
        return None
    return {
        "number": cst.number,
        "currency": cst.currency,
        "date": cst.date,
        "label": cst.label,
    }


import polars as pl


def to_duck_json(meta: Optional[data.Meta]):
    if not meta:
        return None
    # Filter out internal metadata
    filtered = {
        k: v
        for k, v in meta.items()
        if k not in ("filename", "lineno") and not k.startswith("__")
    }
    if not filtered:
        return None
    return json.dumps(filtered, default=str)


def insert_postings(conn: duckdb.DuckDBPyConnection, entries: data.Entries):
    """Iterate through entries and insert postings into the database using Polars for speed."""

    rows = []
    # Using a simple counter instead of hash_entry for significant speedup
    tx_counter = 0

    for entry in entries:
        if not isinstance(entry, data.Transaction):
            continue

        tx_counter += 1
        tx_id = f"tx_{tx_counter}"
        tx_meta_json = to_duck_json(entry.meta)
        tags = list(entry.tags) if entry.tags else []
        links = list(entry.links) if entry.links else []
        description = " | ".join(filter(None, [entry.payee, entry.narration]))

        for posting in entry.postings:
            units = posting.units
            cost = posting.cost
            price = posting.price
            weight = convert.get_weight(posting)

            # Shorthands
            number = units.number if units else None
            currency = units.currency if units else None

            # Rich Objects
            duck_units = to_duck_amount(units)
            duck_cost = to_duck_cost(cost)
            duck_position = {"units": duck_units, "cost": duck_cost}
            duck_price = to_duck_amount(price)
            duck_weight = to_duck_amount(weight)

            rows.append(
                {
                    "id": tx_id,
                    "date": entry.date,
                    "tx_flag": entry.flag,
                    "payee": entry.payee,
                    "narration": entry.narration,
                    "description": description,
                    "tags": tags,
                    "links": links,
                    "tx_meta": tx_meta_json,
                    "account": posting.account,
                    "number": number,
                    "currency": currency,
                    "posting_flag": posting.flag,
                    "posting_meta": to_duck_json(posting.meta),
                    "units": duck_units,
                    "cost": duck_cost,
                    "pos": duck_position,
                    "price": duck_price,
                    "weight": duck_weight,
                    "filename": entry.meta.get("filename"),
                    "lineno": entry.meta.get("lineno"),
                    "year": entry.date.year,
                    "month": entry.date.month,
                    "day": entry.date.day,
                }
            )

    if rows:
        df = pl.from_dicts(rows, infer_schema_length=None)
        # Use DuckDB's native insertion from DataFrame
        conn.execute("INSERT INTO postings SELECT * FROM df")


def insert_other_tables(conn: duckdb.DuckDBPyConnection, entries: data.Entries):
    """Insert data into prices, accounts, and commodities tables using Polars."""

    price_rows = []
    account_rows = {}  # Use dict to store open/close info
    commodity_rows = []

    for entry in entries:
        if isinstance(entry, data.Price):
            price_rows.append(
                {
                    "date": entry.date,
                    "currency": entry.currency,
                    "amount": to_duck_amount(entry.amount),
                }
            )
        elif isinstance(entry, data.Open):
            account_rows[entry.account] = {
                "name": entry.account,
                "open_date": entry.date,
                "close_date": None,
                "currencies": entry.currencies,
                "meta": to_duck_json(entry.meta),
            }
        elif isinstance(entry, data.Close):
            if entry.account in account_rows:
                account_rows[entry.account]["close_date"] = entry.date
            else:
                account_rows[entry.account] = {
                    "name": entry.account,
                    "open_date": None,
                    "close_date": entry.date,
                    "currencies": None,
                    "meta": to_duck_json(entry.meta),
                }
        elif isinstance(entry, data.Commodity):
            commodity_rows.append(
                {"currency": entry.currency, "meta": to_duck_json(entry.meta)}
            )

    if price_rows:
        df_prices = pl.from_dicts(price_rows, infer_schema_length=None)
        conn.execute("INSERT INTO prices SELECT * FROM df_prices")

    if account_rows:
        df_accounts = pl.from_dicts(list(account_rows.values()), infer_schema_length=None)
        conn.execute("INSERT INTO accounts SELECT * FROM df_accounts")

    if commodity_rows:
        df_commodities = pl.from_dicts(commodity_rows, infer_schema_length=None)
        conn.execute("INSERT INTO commodities SELECT * FROM df_commodities")


@click.command()
@click.argument("filename", type=click.Path(exists=True))
@click.argument("database", type=click.Path())
@click.version_option(version=VERSION)
def main(filename, database):
    """Convert a Beancount ledger into a DuckDB database.

    Write ledger FILENAME contents into DuckDB database DATABASE.
    """
    logging.basicConfig(level=logging.INFO, format="%(levelname)-8s: %(message)s")

    logging.info("Loading beancount file: %s", filename)
    entries, errors, options_map = loader.load_file(filename)
    if errors:
        for error in errors:
            logging.error(
                "%s:%s: %s", error.source["filename"], error.source["lineno"], error.message
            )
        # We continue anyway, as some errors might be acceptable

    # Delete previous database if it already exists and is a file
    if os.path.exists(database) and database != ":memory:":
        os.remove(database)

    logging.info("Connecting to DuckDB: %s", database)
    conn = duckdb.connect(database)

    logging.info("Setting up database schema")
    setup_database(conn)

    logging.info("Inserting postings")
    insert_postings(conn, entries)

    logging.info("Inserting other tables (prices, accounts, commodities)")
    insert_other_tables(conn, entries)

    logging.info("Done. Database saved to %s", database)
    conn.close()


if __name__ == "__main__":
    main()

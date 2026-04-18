from __future__ import annotations

import datetime

import pytest

duckdb = pytest.importorskip("duckdb")

from beancount.core.amount import Amount
from experiments.duckdb.duckdb_core import inventory_from_dict
from experiments.duckdb.duckdb_core import position_from_dict
from experiments.duckdb.duckdb_core import register_duckdb
from beancount.core.inventory import Inventory
from beancount.core.number import D
from beancount.core.position import Cost
from beancount.core.position import Position


def test_duckdb_position_round_trip():
    connection = register_duckdb(duckdb.connect())

    position_value, rendered = connection.execute(
        """
        select
            bnpos(4.5, 'HOOL', 510.23, 'USD', date '2024-02-03', 'lot-1'),
            bnstr(
                bnpos(4.5, 'HOOL', 510.23, 'USD', date '2024-02-03', 'lot-1')
            )
        """
    ).fetchone()

    assert position_from_dict(position_value) == Position(
        Amount(D("4.5"), "HOOL"),
        Cost(D("510.23"), "USD", datetime.date(2024, 2, 3), "lot-1"),
    )
    assert rendered == '4.5 HOOL {510.23 USD, 2024-02-03, "lot-1"}'


def test_duckdb_inventory_aggregate_merges_positions():
    connection = register_duckdb(duckdb.connect())

    inventory_value, rendered = connection.execute(
        """
        with positions(pos) as (
            values
                (bnpos(2, 'HOOL', 100, 'USD', date '2024-01-01', 'lot-a')),
                (bnpos(3, 'HOOL', 100, 'USD', date '2024-01-01', 'lot-a')),
                (bnpos(-1, 'HOOL', 100, 'USD', date '2024-01-01', 'lot-a')),
                (bnpos(5, 'USD', null, null, null, null))
        )
        select
            bnsum(pos),
            bnstr(bnsum(pos))
        from positions
        """
    ).fetchone()

    expected = Inventory(
        [
            Position(
                Amount(D("4"), "HOOL"),
                Cost(D("100"), "USD", datetime.date(2024, 1, 1), "lot-a"),
            ),
            Position(Amount(D("5"), "USD"), None),
        ]
    )

    assert inventory_from_dict(inventory_value) == expected
    assert rendered == '(5 USD, 4 HOOL {100 USD, 2024-01-01, "lot-a"})'


def test_duckdb_inventory_aggregate_groups():
    connection = register_duckdb(duckdb.connect())

    rows = connection.execute(
        """
        with positions(bucket, pos) as (
            values
                ('assets', bnpos(2, 'USD', null, null, null, null)),
                ('assets', bnpos(3, 'USD', null, null, null, null)),
                ('lots', bnpos(1, 'BTC', 60000, 'USD', date '2024-03-01', 'buy'))
        )
        select
            bucket,
            bnstr(bnsum(pos))
        from positions
        group by bucket
        order by bucket
        """
    ).fetchall()

    assert rows == [
        ("assets", "(5 USD)"),
        ("lots", '(1 BTC {60000 USD, 2024-03-01, "buy"})'),
    ]

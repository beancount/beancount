"""DuckDB adapters for Beancount core types.

This module keeps DuckDB integration optional. Call ``register_duckdb()`` with a
DuckDB connection to register Python UDFs and SQL macros for Beancount
``Position`` and ``Inventory`` values.
"""

from __future__ import annotations

import datetime
import json
from decimal import Decimal
from typing import Any

from beancount.core.amount import Amount
from beancount.core.inventory import Inventory
from beancount.core.number import D
from beancount.core.position import Cost
from beancount.core.position import Position

POSITION_SQL_TYPE = (
    "STRUCT("
    "units STRUCT(number DECIMAL(38,18), currency VARCHAR), "
    "cost STRUCT(number DECIMAL(38,18), currency VARCHAR, date DATE, label VARCHAR)"
    ")"
)
INVENTORY_SQL_TYPE = f"STRUCT(positions {POSITION_SQL_TYPE}[])"

# Internal UDF names
_UDF_POS = "_bn_pos"
_UDF_STR = "_bn_str"
_UDF_INV = "_bn_inv"

_REGISTERED_FUNCTIONS = (
    _UDF_POS,
    _UDF_STR,
    _UDF_INV,
)


def _decimal_to_sql(number: Decimal | None) -> Decimal | None:
    return number


def _decimal_from_sql(number: Any | None) -> Decimal | None:
    if number is None:
        return None
    if isinstance(number, Decimal):
        return number
    return Decimal(str(number))


def position_to_dict(position: Position) -> dict[str, Any]:
    """Convert a Position into a DuckDB-compatible nested struct."""
    cost = None
    if position.cost is not None:
        cost = {
            "number": _decimal_to_sql(position.cost.number),
            "currency": position.cost.currency,
            "date": position.cost.date,
            "label": position.cost.label,
        }
    return {
        "units": {
            "number": _decimal_to_sql(position.units.number),
            "currency": position.units.currency,
        },
        "cost": cost,
    }


def position_from_dict(value: dict[str, Any] | None) -> Position | None:
    """Convert a DuckDB struct value back into a Position."""
    if value is None:
        return None

    units = value["units"]
    cost_value = value["cost"]
    cost = None
    if cost_value is not None:
        cost = Cost(
            _decimal_from_sql(cost_value["number"]),
            cost_value["currency"],
            cost_value.get("date"),
            cost_value.get("label"),
        )

    return Position(
        Amount(_decimal_from_sql(units["number"]), units["currency"]),
        cost,
    )


def inventory_to_dict(inventory: Inventory) -> dict[str, list[dict[str, Any]]]:
    """Convert an Inventory into a DuckDB-compatible struct."""
    return {"positions": [position_to_dict(position) for position in sorted(inventory)]}


def inventory_from_dict(value: dict[str, Any] | None) -> Inventory:
    """Convert a DuckDB struct value back into an Inventory."""
    inventory = Inventory()
    if value is None:
        return inventory

    for position_value in value.get("positions") or []:
        position = position_from_dict(position_value)
        if position is not None:
            inventory.add_position(position)
    return inventory


def _bn_pos_udf(
    units_number: Decimal,
    units_currency: str,
    cost_number: Decimal | None,
    cost_currency: str | None,
    cost_date: datetime.date | None,
    cost_label: str | None,
) -> dict[str, Any]:
    cost = None
    if cost_number is not None or cost_currency is not None or cost_date is not None:
        if cost_number is None or cost_currency is None or cost_date is None:
            raise ValueError(
                "Cost positions require cost_number, cost_currency, and cost_date."
            )
        cost = Cost(cost_number, cost_currency, cost_date, cost_label)

    position = Position(Amount(units_number, units_currency), cost)
    return position_to_dict(position)


def _bn_str_udf(value: str | None) -> str | None:
    """Polymorphic string renderer for Amount, Position, and Inventory JSON strings."""
    if value is None:
        return None

    try:
        data = json.loads(value)
    except json.JSONDecodeError:
        return str(value)

    if not isinstance(data, dict):
        return str(value)

    # Dispatch based on the JSON keys
    if "positions" in data:
        # It's an Inventory
        for pos in data["positions"]:
            if pos.get("cost") and pos["cost"].get("date"):
                pos["cost"]["date"] = datetime.date.fromisoformat(pos["cost"]["date"])
            if pos.get("units") and pos["units"].get("number") is not None:
                pos["units"]["number"] = Decimal(str(pos["units"]["number"])).normalize()
            if pos.get("cost") and pos["cost"].get("number") is not None:
                pos["cost"]["number"] = Decimal(str(pos["cost"]["number"])).normalize()
        return inventory_from_dict(data).to_string()
    elif "units" in data:
        # It's a Position
        if data.get("cost") and data["cost"].get("date"):
            data["cost"]["date"] = datetime.date.fromisoformat(data["cost"]["date"])
        if data.get("units") and data["units"].get("number") is not None:
            data["units"]["number"] = Decimal(str(data["units"]["number"])).normalize()
        if data.get("cost") and data["cost"].get("number") is not None:
            data["cost"]["number"] = Decimal(str(data["cost"]["number"])).normalize()
        pos = position_from_dict(data)
        return pos.to_string() if pos else None
    elif "number" in data and "currency" in data:
        # It's an Amount
        amt = Amount(Decimal(str(data["number"])).normalize(), data["currency"])
        return amt.to_string()
    else:
        return str(value)


def _bn_inv_udf(
    values: list[dict[str, Any]] | None,
) -> dict[str, list[dict[str, Any]]]:
    inventory = Inventory()
    for value in values or []:
        position = position_from_dict(value)
        if position is not None:
            inventory.add_position(position)
    return inventory_to_dict(inventory)


def register_duckdb(connection):
    """Register DuckDB Python UDFs and macros for Beancount core types."""
    import _duckdb
    import duckdb

    null_handling = _duckdb._func.FunctionNullHandling.SPECIAL
    varchar = duckdb.sqltypes.VARCHAR
    date = duckdb.sqltypes.DATE
    decimal = duckdb.decimal_type(38, 18)
    json_type = duckdb.sqltype("JSON")

    for name in _REGISTERED_FUNCTIONS:
        try:
            connection.remove_function(name)
        except Exception:
            pass

    # 1. Position Constructor UDF
    connection.create_function(
        _UDF_POS,
        _bn_pos_udf,
        [decimal, varchar, decimal, varchar, date, varchar],
        duckdb.sqltype(POSITION_SQL_TYPE),
        null_handling=null_handling,
    )

    # 2. String Renderer UDF (Uses JSON for polymorphism)
    connection.create_function(
        _UDF_STR,
        _bn_str_udf,
        [json_type],
        varchar,
        null_handling=null_handling,
    )

    # 3. Inventory Constructor UDF
    connection.create_function(
        _UDF_INV,
        _bn_inv_udf,
        [duckdb.sqltype(f"{POSITION_SQL_TYPE}[]")],
        duckdb.sqltype(INVENTORY_SQL_TYPE),
        null_handling=null_handling,
    )

    # Exposed Macros
    connection.execute(
        f"""
        create or replace macro bnpos(
            units_number,
            units_currency,
            cost_number,
            cost_currency,
            cost_date,
            cost_label
        ) as {_UDF_POS}(
            cast(units_number as decimal(38,18)),
            units_currency,
            cast(cost_number as decimal(38,18)),
            cost_currency,
            cost_date,
            cost_label
        )
        """
    )

    # bnstr dispatching macro
    connection.execute(
        f"""
        create or replace macro bnstr(x) as
        {_UDF_STR}(cast(x as json))
        """
    )

    connection.execute(
        f"""
        create or replace macro bninv(position_values) as
        {_UDF_INV}(position_values)
        """
    )

    connection.execute(
        f"""
        create or replace macro bnsum(position_value) as
        {_UDF_INV}(list(position_value) filter (where position_value is not null))
        """
    )

    # Standard Table Macros (Column Sets)
    try:
        connection.execute(
            """
            CREATE OR REPLACE MACRO bnj() AS TABLE 
            SELECT date, tx_flag AS flag, payee, narration, account, bnstr(units) AS units, bnstr(pos) AS pos 
            FROM postings;
            """
        )

        connection.execute(
            """
            CREATE OR REPLACE MACRO bnb() AS TABLE 
            SELECT account, bnstr(bnsum(pos)) AS balance 
            FROM postings 
            GROUP BY account 
            ORDER BY account;
            """
        )
    except Exception:
        # Table 'postings' might not exist yet (e.g. during tests or fresh DB)
        pass

    return connection

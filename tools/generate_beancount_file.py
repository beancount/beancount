#!/usr/bin/env python3
"""Generate a Beancount file of roughly the requested size.

Example:
    python tools/generate_beancount_file.py 50m /tmp/bench.beancount
"""

from __future__ import annotations

import argparse
import datetime as dt
import os
import random
import re
from typing import List

SIZE_UNITS = {
    "": 1,
    "b": 1,
    "k": 1_000,
    "kb": 1_000,
    "ki": 1_024,
    "kib": 1_024,
    "m": 1_000_000,
    "mb": 1_000_000,
    "mi": 1_048_576,
    "mib": 1_048_576,
    "g": 1_000_000_000,
    "gb": 1_000_000_000,
    "gi": 1_073_741_824,
    "gib": 1_073_741_824,
}


def parse_size(value: str) -> int:
    match = re.fullmatch(r"(?i)\s*([0-9]+(?:\.[0-9]+)?)\s*([kmgb]?i?b?)?\s*", value)
    if not match:
        raise argparse.ArgumentTypeError(f"invalid size: {value!r}")

    number, suffix = match.groups()
    suffix = (suffix or "").lower()
    multiplier = SIZE_UNITS.get(suffix)
    if multiplier is None:
        raise argparse.ArgumentTypeError(f"unknown size suffix: {suffix!r}")

    bytes_requested = int(float(number) * multiplier)
    if bytes_requested <= 0:
        raise argparse.ArgumentTypeError("size must be greater than zero")

    return bytes_requested


def header_lines(commodity: str) -> List[str]:
    return [
        "; Auto-generated for parser benchmarks",
        f'option "title" "Benchmark Book {commodity}"',
        f'option "operating_currency" "{commodity}"',
        f"commodity {commodity}",
        f"2000-01-01 open Assets:Bank:Checking {commodity}",
        f"2000-01-01 open Assets:Cash:Wallet {commodity}",
        f"2000-01-01 open Expenses:Food {commodity}",
        f"2000-01-01 open Expenses:Travel {commodity}",
        f"2000-01-01 open Expenses:Housing {commodity}",
        f"2000-01-01 open Income:Salary {commodity}",
        f"2000-01-01 open Income:Other {commodity}",
        f"2000-01-01 open Equity:Opening-Balances {commodity}",
        "",
    ]


def format_transaction(
    idx: int,
    date: dt.date,
    commodity: str,
    categories: List[str],
) -> str:
    payee = f"Payee {idx % 500}"
    narration = f"Transaction {idx}"
    amount = round(random.uniform(5, 500), 2)

    if idx % 5 == 0:
        lines = [
            f'{date.isoformat()} * "{payee}" "{narration}"',
            f"  Assets:Bank:Checking  {amount:.2f} {commodity}",
            f"  Income:Salary        -{amount:.2f} {commodity}",
        ]
    else:
        expense = categories[idx % len(categories)]
        lines = [
            f'{date.isoformat()} * "{payee}" "{narration}"',
            f"  Assets:Bank:Checking  -{amount:.2f} {commodity}",
            f"  Expenses:{expense:<12} {amount:.2f} {commodity}",
        ]

    return "\n".join(lines) + "\n"


def write_until_size(
    out_path: str,
    target_bytes: int,
    commodity: str,
    start_date: dt.date,
    seed: int,
) -> int:
    random.seed(seed)
    categories = ["Food", "Travel", "Housing", "Utilities", "Health", "Misc"]
    total_bytes = 0

    directory = os.path.dirname(out_path)
    if directory:
        os.makedirs(directory, exist_ok=True)

    with open(out_path, "w", encoding="utf-8") as handle:
        for line in header_lines(commodity):
            handle.write(line + "\n" if not line.endswith("\n") else line)
            total_bytes += len(line.encode("utf-8")) + (0 if line.endswith("\n") else 1)

        idx = 0
        while total_bytes < target_bytes:
            date = start_date + dt.timedelta(days=idx % 3650)
            block = format_transaction(idx, date, commodity, categories)
            handle.write(block)
            total_bytes += len(block.encode("utf-8"))
            idx += 1

    return total_bytes


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Generate a Beancount file of a target size."
    )
    parser.add_argument("size", type=parse_size, help="target size (e.g. 10m, 512k, 1g)")
    parser.add_argument("output", help="path to write the file")
    parser.add_argument("--commodity", default="USD", help="commodity symbol to use")
    parser.add_argument(
        "--start-date", default="2010-01-01", help="starting transaction date (YYYY-MM-DD)"
    )
    parser.add_argument(
        "--seed", type=int, default=1, help="random seed for reproducibility"
    )

    args = parser.parse_args()

    try:
        start_date = dt.date.fromisoformat(args.start_date)
    except ValueError as exc:
        raise SystemExit(f"invalid start date: {args.start_date!r}") from exc

    written = write_until_size(
        out_path=args.output,
        target_bytes=args.size,
        commodity=args.commodity,
        start_date=start_date,
        seed=args.seed,
    )

    print(f"Wrote {written} bytes to {args.output}")


if __name__ == "__main__":
    main()

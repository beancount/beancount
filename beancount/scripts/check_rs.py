"""Rust-only check command (no transforms/plugins/validation).

This is intended only for performance comparisons.

Unlike `bean-check`, this does not run the Python transformations pipeline nor
`beancount.ops.validate`. It only runs Rust recursive load + booking.
"""

from __future__ import annotations

import argparse

from beancount.parser import _rust


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(prog="bean-check-rs", add_help=True)
    parser.add_argument(
        "--no-cache",
        action="store_true",
        help="Accepted for compatibility; ignored (no Python loader cache used).",
    )
    parser.add_argument("filename")

    args = parser.parse_args(argv)

    entries, load_errors, parse_errors, booking_errors = _rust.check_file_rust(
        args.filename
    )
    total_errors = load_errors + parse_errors + booking_errors

    print(
        f"entries={entries} errors={total_errors} (load={load_errors} parse={parse_errors} booking={booking_errors})"
    )

    return 0 if total_errors == 0 else 1


if __name__ == "__main__":
    raise SystemExit(main())

#!/usr/bin/env python3
"""Benchmark parsing, booking, and validation for C and Rust backends.

This script runs the beancount load pipeline with either the legacy C parser
or the Rust parser and reports timing statistics for each major phase. It is
intended for apples-to-apples comparisons while keeping the rest of the loader
stack identical.
"""

from __future__ import annotations

import argparse
import glob
import io
import os
import statistics
import sys
import time
from pathlib import Path
from typing import Callable
from typing import Iterable
from typing import NamedTuple

from beancount import loader
from beancount.core import data
from beancount.ops import validation
from beancount.parser import _rust
from beancount.parser import booking
from beancount.parser import grammar
from beancount.parser import options
from beancount.parser import parser as rust_parser

LoadTriplet = tuple[data.Directives, list[data.BeancountError], loader.OptionsMap]
ParseFileFn = Callable[[str, str | None], LoadTriplet]
ParseStringFn = Callable[[str | bytes, str | None], LoadTriplet]


class Backend(NamedTuple):
    """A parser backend with compatible parse helpers."""

    name: str
    parse_file: ParseFileFn
    parse_string: ParseStringFn


class Stats(NamedTuple):
    """Aggregated timing stats for a metric."""

    mean: float
    stdev: float
    minimum: float
    maximum: float


class Timings(NamedTuple):
    """Per-run timings in seconds."""

    parse: float
    booking: float
    validation: float

    @property
    def total(self) -> float:
        return self.parse + self.booking + self.validation


class RunResult(NamedTuple):
    """Outcome of a single run."""

    timings: Timings
    errors: list[data.BeancountError]


class BackendResult(NamedTuple):
    """Aggregated results for one backend."""

    backend: str
    runs: list[RunResult]

    @property
    def errors(self) -> list[data.BeancountError]:
        errs: list[data.BeancountError] = []
        for run in self.runs:
            errs.extend(run.errors)
        return errs


def stats(values: Iterable[float]) -> Stats:
    values = list(values)
    return Stats(
        mean=statistics.fmean(values),
        stdev=statistics.pstdev(values) if len(values) > 1 else 0.0,
        minimum=min(values),
        maximum=max(values),
    )


def make_c_backend() -> Backend | None:
    try:
        from beancount.parser import _parser
    except ImportError:
        return None

    def parse_file(filename: str, encoding: str | None = None) -> LoadTriplet:
        if encoding is not None and encoding.lower() != "utf-8":
            raise ValueError("Only UTF-8 encoded files are supported.")
        builder = grammar.Builder()
        parser_obj = _parser.Parser(builder)
        with open(filename, "rb") as handle:
            parser_obj.parse(handle, filename=filename, lineno=1)
        return builder.finalize()

    def parse_string(text: str | bytes, report_filename: str | None = None) -> LoadTriplet:
        filename = report_filename or "<string>"
        buffer = text if isinstance(text, (bytes, bytearray)) else text.encode("utf8")
        builder = grammar.Builder()
        parser_obj = _parser.Parser(builder)
        parser_obj.parse(io.BytesIO(buffer), filename=filename, lineno=1)
        return builder.finalize()

    return Backend("c", parse_file=parse_file, parse_string=parse_string)


def make_rust_backend() -> Backend:
    return Backend(
        "rust",
        parse_file=rust_parser.parse_file,
        parse_string=_rust.parse_string,
    )


def _parse_recursive(
    backend: Backend,
    sources: list[tuple[str, bool]],
    encoding: str | None,
) -> tuple[data.Directives, list[data.BeancountError], loader.OptionsMap]:
    entries: data.Directives = []
    parse_errors: list[data.BeancountError] = []
    options_map: loader.OptionsMap | None = None
    other_options_map: list[loader.OptionsMap] = []

    source_stack = list(sources)
    filenames_seen = set()

    while source_stack:
        source, is_file = source_stack.pop(0)
        is_top_level = options_map is None

        if is_file:
            cwd = os.path.dirname(source)
            source_filename = source
        else:
            cwd = os.getcwd()
            source_filename = None

        if is_file:
            filename = os.path.normpath(source)
            if filename in filenames_seen:
                parse_errors.append(
                    loader.LoadError(
                        data.new_metadata("<load>", 0),
                        f'Duplicate filename parsed: "{filename}"',
                    )
                )
                continue
            if not os.path.exists(filename):
                parse_errors.append(
                    loader.LoadError(
                        data.new_metadata("<load>", 0), f'File "{filename}" does not exist'
                    )
                )
                continue
            filenames_seen.add(filename)
            src_entries, src_errors, src_options_map = backend.parse_file(
                filename, encoding=encoding
            )
            cwd = os.path.dirname(filename)
        else:
            if encoding:
                source = (
                    source.encode("ascii", "replace") if isinstance(source, str) else source
                )
            src_entries, src_errors, src_options_map = backend.parse_string(
                source, source_filename
            )

        entries.extend(src_entries)
        parse_errors.extend(src_errors)

        if is_top_level:
            options_map = src_options_map
        else:
            other_options_map.append(src_options_map)

        include_expanded: list[str] = []
        for include_filename in src_options_map["include"]:
            search_path = include_filename
            if not os.path.isabs(include_filename):
                search_path = os.path.join(cwd, include_filename)
            matched = glob.glob(search_path, recursive=True)
            if matched:
                include_expanded.extend(matched)
            else:
                parse_errors.append(
                    loader.LoadError(
                        data.new_metadata("<load>", 0),
                        f'File glob "{include_filename}" does not match any files',
                    )
                )
        for include_filename in include_expanded:
            if not os.path.isabs(include_filename):
                include_filename = os.path.join(cwd, include_filename)
            include_filename = os.path.normpath(include_filename)
            source_stack.append((include_filename, True))

    if options_map is None:
        options_map = options.OPTIONS_DEFAULTS.copy()

    options_map["include"] = sorted(filenames_seen)
    options_map = loader.aggregate_options_map(options_map, other_options_map)
    return entries, parse_errors, options_map


def load_with_backend(
    backend: Backend,
    filename: str,
    extra_validations: list | None,
    encoding: str | None,
) -> tuple[list[data.BeancountError], Timings]:
    sources = [(os.path.abspath(filename), True)]

    parse_start = time.perf_counter()
    entries, parse_errors, options_map = _parse_recursive(backend, sources, encoding)
    entries.sort(key=data.entry_sortkey)
    parse_elapsed = time.perf_counter() - parse_start

    book_start = time.perf_counter()
    entries, balance_errors = booking.book(entries, options_map)
    parse_errors.extend(balance_errors)
    entries, errors = loader.run_transformations(
        entries, parse_errors, options_map, log_timings=None
    )
    book_elapsed = time.perf_counter() - book_start

    validation_start = time.perf_counter()
    valid_errors = validation.validate(
        entries, options_map, log_timings=None, extra_validations=extra_validations
    )
    errors.extend(valid_errors)
    validation_elapsed = time.perf_counter() - validation_start

    options_map["input_hash"] = loader.compute_input_hash(options_map["include"])

    return errors, Timings(
        parse=parse_elapsed, booking=book_elapsed, validation=validation_elapsed
    )


def run_backend(
    backend: Backend, ledger: str, iterations: int, warmup: int
) -> BackendResult:
    runs: list[RunResult] = []

    for _ in range(warmup):
        load_with_backend(backend, ledger, extra_validations=None, encoding=None)

    for _ in range(iterations):
        errors, timing = load_with_backend(
            backend, ledger, extra_validations=None, encoding=None
        )
        runs.append(RunResult(timings=timing, errors=errors))

    return BackendResult(backend=backend.name, runs=runs)


def render_backend_result(result: BackendResult) -> str:
    parse_stats = stats(run.timings.parse for run in result.runs)
    book_stats = stats(run.timings.booking for run in result.runs)
    validation_stats = stats(run.timings.validation for run in result.runs)
    total_stats = stats(run.timings.total for run in result.runs)

    lines = [f"backend={result.backend} runs={len(result.runs)}"]
    lines.append(
        "  parse:     mean={mean:.4f}s stdev={stdev:.4f}s min={minimum:.4f}s max={maximum:.4f}s".format(
            **parse_stats._asdict()
        )
    )
    lines.append(
        "  booking*:  mean={mean:.4f}s stdev={stdev:.4f}s min={minimum:.4f}s max={maximum:.4f}s".format(
            **book_stats._asdict()
        )
    )
    lines.append(
        "  validation: mean={mean:.4f}s stdev={stdev:.4f}s min={minimum:.4f}s max={maximum:.4f}s".format(
            **validation_stats._asdict()
        )
    )
    lines.append(
        "  total:     mean={mean:.4f}s stdev={stdev:.4f}s min={minimum:.4f}s max={maximum:.4f}s".format(
            **total_stats._asdict()
        )
    )

    error_count = sum(len(run.errors) for run in result.runs)
    if error_count:
        lines.append(
            f"  errors: {error_count} (first run shows {len(result.runs[0].errors)})"
        )
    else:
        lines.append("  errors: 0")

    lines.append("  booking* includes plugin transformations")
    return "\n".join(lines)


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("ledger", type=Path, help="Path to a Beancount ledger file")
    parser.add_argument(
        "--backend",
        dest="backends",
        action="append",
        choices=["rust", "c"],
        help="Backends to benchmark (default: both if available)",
    )
    parser.add_argument("--iterations", type=int, default=5, help="Number of measured runs")
    parser.add_argument("--warmup", type=int, default=1, help="Warmup runs to ignore")
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv or sys.argv[1:])
    backends = args.backends or ["rust", "c"]

    available: list[Backend] = []
    for name in backends:
        if name == "rust":
            available.append(make_rust_backend())
        elif name == "c":
            backend = make_c_backend()
            if backend is None:
                print("[warn] C parser backend unavailable; skipping", file=sys.stderr)
                continue
            available.append(backend)

    if not available:
        print("No backends available to benchmark.", file=sys.stderr)
        return 1

    ledger_path = args.ledger.expanduser().resolve()
    if not ledger_path.exists():
        print(f"Ledger file not found: {ledger_path}", file=sys.stderr)
        return 1

    results = [
        run_backend(backend, str(ledger_path), args.iterations, args.warmup)
        for backend in available
    ]

    for result in results:
        print(render_backend_result(result))

    return 0


if __name__ == "__main__":
    raise SystemExit(main())

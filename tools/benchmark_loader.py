#!/usr/bin/env python3
"""Benchmark Rust loader vs Python loader on the same revision.

This compares the default Python pipeline against the Rust "load + book" path
(enabled via `BEANCOUNT_RUST_LOAD_AND_BOOK=1`).

It uses `hyperfine` and runs `bean-check --no-cache` so the benchmark reflects
real-world loader usage.

Example:
  tools/benchmark_loader.py examples/example.beancount
"""

from __future__ import annotations

import argparse
import json
import os
import shutil
import statistics
import subprocess
import tempfile
import time
from pathlib import Path


def find_bean_check() -> str:
    return shutil.which("bean-check") or "./bin/bean-check"


def find_bean_check_rs() -> str:
    return shutil.which("bean-check-rs") or "./bin/bean-check-rs"


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument("beancount_file", help="Beancount input file to process.")
    parser.add_argument("--warmup", type=int, default=2)
    parser.add_argument("--min-runs", type=int, default=30)
    parser.add_argument(
        "--print-samples",
        action="store_true",
        help="Print the duration of every measured run for each mode.",
    )

    args = parser.parse_args()

    beancount_file = str(Path(args.beancount_file).resolve())
    bean_check = find_bean_check()
    bean_check_rs = find_bean_check_rs()

    env = os.environ.copy()
    env["L"] = beancount_file

    if shutil.which("hyperfine") is not None:
        # We keep the command strings for hyperfine so $L is expanded.
        cmd_python = f"env -u BEANCOUNT_RUST_LOAD_AND_BOOK {bean_check} --no-cache $L"
        cmd_rust = f"env BEANCOUNT_RUST_LOAD_AND_BOOK=1 {bean_check} --no-cache $L"
        cmd_rust_only = f"{bean_check_rs} --no-cache $L"

        export_path = None
        if args.print_samples:
            export_path = Path(tempfile.mkstemp(prefix="bench-", suffix=".json")[1])

        hyperfine_cmd = [
            "hyperfine",
            f"--warmup={args.warmup}",
            f"--min-runs={args.min_runs}",
            "-n",
            "python-loader",
            cmd_python,
            "-n",
            "rust-loader",
            cmd_rust,
            "-n",
            "rust-only",
            cmd_rust_only,
        ]

        if export_path is not None:
            hyperfine_cmd.extend(["--export-json", str(export_path)])

        subprocess.check_call(hyperfine_cmd, shell=False, env=env)

        if export_path is not None:
            data = json.loads(export_path.read_text("utf-8"))
            for result in data.get("results", []):
                name = result.get("command", "<unknown>")
                times = result.get("times", [])
                print(f"{name} samples ({len(times)}):")
                for i, t in enumerate(times, 1):
                    print(f"  {i:02d}: {t:.6f}s")
        return

    def run_one(mode_env: dict[str, str]) -> float:
        start = time.perf_counter()
        subprocess.run(
            [bean_check, "--no-cache", beancount_file],
            check=True,
            env=mode_env,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        end = time.perf_counter()
        return end - start

    def run_one_rust_only() -> float:
        start = time.perf_counter()
        subprocess.run(
            [bean_check_rs, "--no-cache", beancount_file],
            check=True,
            env=env,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        end = time.perf_counter()
        return end - start

    def bench(name: str, runner) -> list[float]:
        for _ in range(args.warmup):
            runner()

        samples = [runner() for _ in range(args.min_runs)]
        mean = statistics.mean(samples)
        stdev = statistics.pstdev(samples)
        median = statistics.median(samples)
        print(f"{name}: mean={mean:.4f}s  median={median:.4f}s  stdev={stdev:.4f}s")
        return samples

    env_python = env.copy()
    env_python.pop("BEANCOUNT_RUST_LOAD_AND_BOOK", None)
    env_rust = env.copy()
    env_rust["BEANCOUNT_RUST_LOAD_AND_BOOK"] = "1"

    python_samples = bench("python-loader", lambda: run_one(env_python))
    rust_samples = bench("rust-loader", lambda: run_one(env_rust))
    rust_only_samples = bench("rust-only", run_one_rust_only)

    if args.print_samples:

        def dump(name: str, samples: list[float]) -> None:
            print(f"{name} samples ({len(samples)}):")
            for i, t in enumerate(samples, 1):
                print(f"  {i:02d}: {t:.6f}s")

        dump("python-loader", python_samples)
        dump("rust-loader", rust_samples)
        dump("rust-only", rust_only_samples)
    ratio = statistics.mean(rust_samples) / statistics.mean(python_samples)
    print(f"rust/python mean ratio: {ratio:.3f}x")

    ratio_rust_only = statistics.mean(rust_only_samples) / statistics.mean(python_samples)
    print(f"rust-only/python mean ratio: {ratio_rust_only:.3f}x")


if __name__ == "__main__":
    main()

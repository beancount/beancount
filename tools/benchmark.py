#!/usr/bin/env python3
"""Estimate and compare the performance of a list of revisions.
"""
__copyright__ = "Copyright (C) 2020  Martin Blais"
__license__ = "GNU GPLv2"


import argparse
import os
import subprocess


def benchmark_revision(beancount_file: str, revision: str):
    """Run the benchmark on a particular revision."""
    args = dict(shell=False, stdout=subprocess.PIPE)

    # Checkout the desired revision.
    checkout_command = ["git", "checkout", revision]
    subprocess.check_call(checkout_command, **args)

    # Build from scratch.
    subprocess.check_call(["make", "clean", "build"], **args)

    # Run a number of iterations.
    meta_command = ["hyperfine", "--warmup=2", "--min-runs=30"]
    run_command = ["bean-check --no-cache $L"]
    env = os.environ.copy()
    env["L"] = beancount_file
    subprocess.check_call(meta_command + run_command,
                          shell=False, env=env)


def main():
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('beancount_file', help="Beancount input file to process.")
    parser.add_argument('revisions', nargs='+', help="Revisions to compare.")

    args = parser.parse_args()

    for revision in args.revisions:
        benchmark_revision(args.beancount_file, revision)


if __name__ == '__main__':
    main()

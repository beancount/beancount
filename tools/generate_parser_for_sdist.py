#!/usr/bin/env python3
"""Generate parser C sources into the Meson dist directory."""

from __future__ import annotations

import os
import pathlib
import subprocess
import sys


def run(command: list[str], cwd: pathlib.Path) -> None:
    subprocess.run(command, cwd=cwd, check=True)


def main() -> int:
    if len(sys.argv) != 3:
        print(
            "usage: generate_parser_for_sdist.py <bison> <flex>",
            file=sys.stderr,
        )
        return 2

    dist_root_env = os.environ.get("MESON_DIST_ROOT")
    if not dist_root_env:
        print("MESON_DIST_ROOT is not set", file=sys.stderr)
        return 2

    dist_root = pathlib.Path(dist_root_env)
    parser_dir = dist_root / "beancount" / "parser"

    bison = pathlib.Path(sys.argv[1])
    flex = pathlib.Path(sys.argv[2])

    run(
        [
            str(flex),
            "--outfile=lexer.c",
            "--header-file=lexer.h",
            "lexer.l",
        ],
        cwd=parser_dir,
    )

    run(
        [
            str(bison),
            "--report=itemset",
            "--verbose",
            "-Wall",
            "-Werror",
            "-o",
            "grammar.c",
            "grammar.y",
        ],
        cwd=parser_dir,
    )

    return 0


if __name__ == "__main__":
    raise SystemExit(main())

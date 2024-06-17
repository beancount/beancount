#!/usr/bin/env python3
"""Generate a Bazel rule from source code, mainly to automatically (mostly) compute deps."""

from os import path
import argparse
import re
import textwrap


def get_deps(filename):
    deps = set()
    for line in open(filename):
        if "beancount" not in line:
            continue
        line = re.sub(r" as (.*)$", "", line)
        dep = None
        match = re.match(r"from (.*) import (.*)$", line)
        if match:
            if re.match(r"[a-z]", match.group(2)):
                dep = ".".join(match.group(1, 2))
            else:
                dep = match.group(1)
        else:
            match = re.match(r"import (.*)$", line)
            if match:
                dep = match.group(1)
        if dep is not None:
            deps.add(dep)
    return deps


def main():
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument("filename", help="Filename")
    args = parser.parse_args()

    deps = get_deps(args.filename)
    render(args.filename, deps)


def render(filename, deps):
    basename = path.basename(filename)
    name = basename.replace(".py", "")
    target = "py_test" if re.search(r"_test.py$", basename) else "py_library"
    print(
        textwrap.dedent(f"""\
        {target}(
            name = "{name}",
            srcs = ["{basename}"],
            deps = [""")
    )

    for dep in sorted(deps):
        libdep = dep.replace(".", "/")
        sdep = list(libdep)
        sdep[libdep.rindex("/")] = ":"
        libdep = "".join(sdep)
        print(r'        "//{}",'.format(libdep))

    print(
        textwrap.dedent("""\
            ],
        )
    """)
    )


if __name__ == "__main__":
    main()

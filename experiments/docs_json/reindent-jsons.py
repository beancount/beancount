#!/usr/bin/env python3
""" """

__copyright__ = "Copyright (C) 2013-2018, 2020, 2024  Martin Blais"
__license__ = "GNU GPLv2"


import json
import os
import re


def main():
    for infilename in os.listdir("."):
        if not re.search(r"\.json$", infilename):
            continue
        outfilename = infilename.replace(".json", ".json2")
        obj = json.load(open(infilename, "r"))
        json.dump(obj, open(outfilename, "w"), indent=2)
        os.rename(outfilename, infilename)


if __name__ == "__main__":
    main()

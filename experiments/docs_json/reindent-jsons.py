#!/usr/bin/env python3
""" """

import os
import json
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

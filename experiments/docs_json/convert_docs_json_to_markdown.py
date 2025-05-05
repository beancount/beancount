#!/usr/bin/env python3
"""Convert Google Docs V1 API's JSON to Markdown."""

__copyright__ = "Copyright (C) 2019-2020, 2023-2024  Martin Blais"
__license__ = "GNU GPLv2"


import argparse
import collections
import json
import logging
import os
import re
import textwrap
from os import path


def _get(obj, path):
    for comp in path.split("/"):
        if comp not in obj:
            return
        obj = obj[comp]
    return obj


def _dispatch(table, elem):
    celem = elem.copy()
    celem.pop("startIndex", None)
    celem.pop("endIndex", None)
    assert len(celem) == 1
    etype, econtents = celem.popitem()
    return table[etype](econtents)


TextRun = collections.namedtuple("TextRun", "text family")


def parse_TextRun(contents):
    family = _get(contents, "textStyle/weightedFontFamily/fontFamily")
    return TextRun(contents["content"], family)


def parse_AutoText(contents):
    raise NotImplementedError


def parse_PageBreak(contents):
    pass


def parse_ColumnBreak(contents):
    raise NotImplementedError


def parse_FootnoteReference(contents):
    pass
    # raise NotImplementedError(pprint.pformat(contents))


def parse_HorizontalRule(contents):
    pass


def parse_Equation(contents):
    raise NotImplementedError


def parse_InlineObjectElement(contents):
    pass
    # raise NotImplementedError


_dispatch_Element = {
    "textRun": parse_TextRun,
    "autoText": parse_AutoText,
    "pageBreak": parse_PageBreak,
    "columnBreak": parse_ColumnBreak,
    "footnoteReference": parse_FootnoteReference,
    "horizontalRule": parse_HorizontalRule,
    "equation": parse_Equation,
    "inlineObjectElement": parse_InlineObjectElement,
}


def parse_Element(elem):
    return _dispatch(_dispatch_Element, elem)


def parse_SectionBreak(econtents):
    assert econtents.keys() == {"sectionStyle"}, econtents


def parse_Table(econtents):
    pass
    # raise NotImplementedError


def parse_Paragraph(paragraph):
    style = paragraph["paragraphStyle"]["namedStyleType"]
    # Compress runs of text together.
    parelems = []
    for element in paragraph["elements"]:
        pelem = parse_Element(element)
        if isinstance(pelem, TextRun):
            last = parelems[-1] if parelems else None
            if last and isinstance(last, TextRun) and last.family == pelem.family:
                parelems.pop(-1)
                pelem = TextRun(last.text + pelem.text, last.family)
            parelems.append(pelem)
        else:
            assert pelem is None

    # Convert all the hard newlines to soft ones.
    parelems = [
        elem._replace(text=elem.text.replace("\x0b", "\n"))
        if isinstance(elem, TextRun)
        else elem
        for elem in parelems
    ]

    return (style, parelems)


def parse_TableOfContents(econtents):
    assert econtents.keys() == {"content"}, econtents.keys()


_dispatch_StructuralElement = {
    "sectionBreak": parse_SectionBreak,
    "paragraph": parse_Paragraph,
    "table": parse_Table,
    "tableOfContents": parse_TableOfContents,
}


def parse_StructuralElement(selem):
    return _dispatch(_dispatch_StructuralElement, selem)


def parse_Body(body):
    assert set(body.keys()) == {"content"}
    return list(filter(None, [parse_StructuralElement(selem) for selem in body["content"]]))


def parse_Document(document):
    return (document["title"], parse_Body(document["body"]))


def remove_default_fonts(body, default_font="Cambria"):
    """Remove text runs with the default font."""
    new_body = []
    for etype, runs in body:
        new_runs = []
        for run in runs:
            if run.family == default_font:
                run = run._replace(family=None)
            new_runs.append(run)
        new_body.append((etype, new_runs))
    return new_body


def merge_runs(body):
    """Merge consecutive text runs with the same font."""
    new_body = []
    for etype, runs in body:
        new_runs = []
        last_run = None
        for run in runs:
            if last_run is None:
                last_run = run
            elif run.family == last_run.family:
                run = last_run = run._replace(text=(last_run.text + run.text))
                new_runs.pop(-1)
            new_runs.append(run)
        new_body.append((etype, new_runs))
    return new_body


class Renderer:
    def __init__(self, outfile):
        self.file = outfile

    def TITLE(self, item):
        print("= {} =\n".format(item.text.strip()), file=self.file)

    def HEADING_1(self, item):
        print("== {} ==\n".format(item.text.strip()), file=self.file)

    def HEADING_2(self, item):
        print("=== {} ===\n".format(item.text.strip()), file=self.file)

    def HEADING_3(self, item):
        print("==== {} ====\n".format(item.text.strip()), file=self.file)

    def HEADING_4(self, item):
        print("===== {} =====\n".format(item.text.strip()), file=self.file)

    def HEADING_5(self, item):
        print("====== {} ======\n".format(item.text.strip()), file=self.file)

    def NORMAL_TEXT(self, item):
        if item.family == "Consolas":
            lines = item.text.split("\n")
            print("\n".join("   {}".format(line) for line in lines), file=self.file)
        else:
            print(textwrap.fill(item.text.strip(), 80), file=self.file)
        print(file=self.file)


def main():
    logging.basicConfig(level=logging.INFO, format="%(levelname)-8s: %(message)s")
    parser = argparse.ArgumentParser(description=__doc__.strip())

    parser.add_argument(
        "--fileordir",
        action="store",
        default=os.getcwd(),
        help="The JSON file or directory to process",
    )

    args = parser.parse_args()

    if path.isfile(args.fileordir):
        filenames = [args.fileordir]
    else:
        filenames = [
            path.join(args.fileordir, x)
            for x in os.listdir(args.fileordir)
            if re.search("\.json$", x)
        ]
    for filename in filenames:
        with open(filename, "r") as infile:
            document = json.load(infile)
        title, body = parse_Document(document)

        for item in body:
            assert len(item) == 2
        body = remove_default_fonts(body)
        body = merge_runs(body)

        output_filename = filename.replace(".json", ".md")
        with open(output_filename, "w") as outfile:
            renderer = Renderer(outfile)
            for etype, runs in body:
                fun = getattr(renderer, etype, None)
                if fun is None:
                    print(etype)
                else:
                    for run in runs:
                        fun(run)

            # print(title, file=outfile)
            # print(pprint.pformat(body), file=outfile)


if __name__ == "__main__":
    main()

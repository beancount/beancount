#!/usr/bin/env python3
"""Re-insert indentation from blockquotes in RST converted from docx file."""

__copyright__ = "Copyright (C) 2018, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import io
import logging
import os
import re
import subprocess
import threading
import zipfile
from os import path

import bs4


class _Save(threading.local):
    """Funcall return saver for regexp matching."""

    def __call__(self, value):
        self.value = value
        return value


save = _Save()


def ConvertToMarkdown(text: str, isbold: bool, isitalic: bool):
    text = text.replace("*", r"\*")
    text = text.replace('"', r"\"")
    text = text.replace("_", r"\_")
    text = text.replace("<", r"\<")
    text = text.replace(">", r"\>")
    # text = text.replace('{...}', r'{\...}')
    # text = text.replace("…", "\\…")
    if isbold:
        text = "**{}**".format(text)
    elif isitalic:
        text = "*{}*".format(text)
    return text


def ConvertToRst(text: str, isbold: bool, isitalic: bool):
    text = text.replace("*", r"\*")
    if isbold:
        text = "**{}**".format(text)
    elif isitalic:
        text = "*{}*".format(text)
    return text


def GetDocxBlocks(filename: str, convert: callable = None) -> list[str]:
    """Get the list of blocks formatted with a fixed-width font
    from the docs file. Note that this will include short blocks
    which aren't necessarily blockquotes."""
    with zipfile.ZipFile(filename, "r") as myzip:
        with myzip.open("word/document.xml") as docfile:
            soup = bs4.BeautifulSoup(docfile, "lxml")
    blocks = []
    for wp in soup.find_all("w:p"):
        if not wp.find("w:rfonts", attrs={"w:cs": "Consolas"}):
            continue
        block = io.StringIO()
        for wr in wp.find_all("w:r"):
            # if wr.find_all('w:rfonts', attrs={'w:cs': "Consolas"}):
            # print(wr.prettify())
            # print('XXXNEW')
            for w in wr.find_all(re.compile("w:(t|br)")):
                bold = wr.find("w:b", attrs={"w:val": "1"}) is not None
                italic = wr.find("w:i", attrs={"w:val": "1"}) is not None
                if w.name == "w:t":
                    text = w.text
                    if convert is not None:
                        text = convert(text, bold, italic)
                    block.write(text)
                    # print('XXX', w.text)
                elif w.name == "w:br":
                    block.write("\n")
                    # print('XXX', w.text)
            # print(',' + '-'*80)
        value = block.getvalue().splitlines()
        if value:
            # Cull out extracted bits which aren't likely to be blockquotes.
            if len(value) == 1 and len(value[0]) > 80:
                continue
            blocks.append(value)
            # print(block.getvalue().replace(' ', '_'))
            # print('`' + '-'*80)

    return blocks


def GetMarkdownBlocks(lines: list[str]) -> list[tuple[int, int, str]]:
    """Get the list of blockquotes from the markdown file (and
    their positions)."""
    blocks = []
    block = []
    for index, line in enumerate(lines):
        if save(re.match(r"> (.*)\\?", line)):
            text = save.value.group(1).rstrip("\\").replace(r"\...", "...")
            block.append((index, text))
        elif block:
            blocks.append((block[0][0], block[-1][0], [line for _, line in block]))
            block = []
    if block:
        blocks.append((block[0][0], block[-1][0], [line for _, line in block]))
    return blocks


def GetRstBlocks(lines: list[str]) -> list[tuple[int, int, str]]:
    """Get the list of blockquotes from the markdown file (and
    their positions)."""

    # Preprocess the rst text so that one-line blocks are prefixed with pipes
    # uniformly.
    inlens = []
    for index, line in enumerate(lines):
        match = re.match(r"([ ]*)(.*)", line)
        assert match
        inlens.append((len(match.group(1)), len(match.group(2))))
        if len(inlens) < 3:
            continue
        if (
            inlens[-3] == (0, 0)
            and inlens[-2][0] == 4
            and inlens[-2][1] > 0
            and inlens[-1] == (0, 0)
        ):
            lines[index - 1] = "    | {}".format(lines[index - 1].lstrip())

    blocks = []
    block = []
    for index, line in enumerate(lines):
        if save(re.match(r"    \| (.*)", line)):
            text = save.value.group(1)
            block.append((index, text))
        elif block and save(re.match(r"      (.*)", line)):
            text = save.value.group(1)
            index, prev_text = block.pop(-1)
            block.append((index, "{} {}".format(prev_text, text)))
        elif block:
            blocks.append((block[0][0], block[-1][0], [line for _, line in block]))
            block = []
    if block:
        blocks.append((block[0][0], block[-1][0], [line for _, line in block]))
    return blocks


def ComputeKey(lines: list[str], remove_matching_stars: bool = True) -> str:
    """Reduce a snippet to a comparable key."""
    newlines = []
    for line in lines:
        # Remove ellipsis
        line = line.replace("…", "")
        line = line.replace("...", "")
        # Compress whitespace.
        line = re.sub(r"[ \t]+", " ", line)
        if remove_matching_stars:
            for pattern in [r"\*\*(.*?)\*\*", r"\*(.*?)\*"]:
                n = 1
                while n > 0:
                    line, n = re.subn(pattern, r"\1", line)
        line = line.strip()
        if line:
            newlines.append(line)
    return "".join(newlines)


def PandocDocxToRst(filename: str) -> str:
    rst_string = subprocess.check_output(
        ["pandoc", "-f", "docx", "-t", "rst", filename], shell=False, encoding="utf8"
    )
    return [s.rstrip() for s in rst_string.splitlines()]


def ConvertDocx(filename: str) -> str:
    blocks_docx = GetDocxBlocks(filename, ConvertToRst)
    lines_txt = PandocDocxToRst(filename)
    blocks_txt = GetRstBlocks(lines_txt)

    docx = [(ComputeKey(block, True), block) for block in blocks_docx]
    map_docx = dict(docx)
    map_txt = {
        ComputeKey(block, True): (minline, maxline, block)
        for minline, maxline, block in blocks_txt
    }

    # print('BLOCKS_DOCX')
    # for block in blocks_docx:
    #     print('*' * 80)
    #     for line in block:
    #         print(repr(line))
    # print()

    # print('BLOCKS_TXT')
    # for block in blocks_txt:
    #     print('*' * 80)
    #     for line in block:
    #         print(repr(line))
    # print()

    matches = []
    for key, block in docx:
        # print('-' * 80)
        # print('KEY', key)
        try:
            minline, maxline, block_txt = map_txt.pop(key)
            block_docx = map_docx.pop(key)
            matches.append((key, (minline, maxline), block_txt, block_docx))
        except KeyError:
            pass

    # Trace matching lines.
    if 0:
        print("MATCHING")
        for key, (minline, maxline), block_txt, block_docx in matches:
            print("-" * 120)
            print("DOCX")
            for line in block_docx:
                print(repr(line))
            print("TXT")
            for line in block_txt:
                print(repr(line))

        # print('=' * 120)
        if map_docx:
            print("NOTFOUND DOCX:")
            for key, block in map_docx.items():
                print("-" * 120)
                for line in block:
                    print(repr(line))

        if map_txt:
            print("NOTFOUND TXT:")
            for key, (minline, maxline, block) in map_txt.items():
                print("-" * 120)
                for line in block:
                    print(repr(line))

    # Replace blocks in the original md file.
    offset = 0
    for key, (minline, maxline), block_txt, block_docx in matches:
        minline += offset
        maxline += offset
        del lines_txt[minline : maxline + 1]
        new_lines = ["    | {}".format(line) for line in block_docx]
        lines_txt[minline:minline] = new_lines
        offset += len(new_lines) - (maxline + 1 - minline)

    return os.linesep.join(lines_txt)


def FindDocxFiles(directory: str):
    if path.isdir(directory):
        for root, _, files in os.walk(directory):
            for filename in files:
                if re.search(r".docx$", filename):
                    yield path.join(root, filename)
    elif re.search(r".docx$", directory):
        yield directory


def main():
    logging.basicConfig(level=logging.INFO, format="%(levelname)-8s: %(message)s")
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument("root", help="Root directory where to look for .docx files")
    args = parser.parse_args()

    for filename in FindDocxFiles(args.root):
        rst_contents = ConvertDocx(filename)
        outfilename = filename.replace(".docx", ".rst")
        logging.info("Writing %s", outfilename)
        with open(outfilename, "w") as outfile:
            outfile.write(rst_contents)


if __name__ == "__main__":
    main()

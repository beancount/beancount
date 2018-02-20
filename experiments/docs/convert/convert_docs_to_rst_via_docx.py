#!/usr/bin/env python3
"""Re-insert indentation from blockquotes in Markdown converted from docx file.
"""

import argparse
import logging
import zipfile
import io
import re
import threading
from typing import List
from typing import Tuple
from pprint import pprint

import bs4


class _Save(threading.local):
    def __call__(self, value):
        self.value = value
        return value
save = _Save()


def ConvertToMarkdown(text: str, isbold: bool, isitalic: bool):
    text = text.replace('*', r'\*')
    text = text.replace('"', r'\"')
    text = text.replace('_', r'\_')
    text = text.replace('<', r'\<')
    text = text.replace('>', r'\>')
    #text = text.replace('{...}', r'{\...}')
    #text = text.replace("…", "\\…")
    if isbold:
        text = '**{}**'.format(text)
    elif isitalic:
        text = '*{}*'.format(text)
    return text

    
def ConvertToRst(text: str, isbold: bool, isitalic: bool):
    text = text.replace('*', r'\*')
    if isbold:
        text = '**{}**'.format(text)
    elif isitalic:
        text = '*{}*'.format(text)
    return text


def GetDocxBlocks(filename: str, convert: callable=None) -> List[str]:
    """Get the list of blocks formatted with a fixed-width font
    from the docs file. Note that this will include short blocks
    which aren't necessarily blockquotes."""
    with zipfile.ZipFile(filename, 'r') as myzip:
        with myzip.open('word/document.xml') as docfile:
            soup = bs4.BeautifulSoup(docfile, 'lxml')
    blocks = []
    for wp in soup.find_all('w:p'):
        if not wp.find('w:rfonts', attrs={'w:cs': "Consolas"}):
            continue
        block = io.StringIO()
        for wr in wp.find_all('w:r'):
            #if wr.find_all('w:rfonts', attrs={'w:cs': "Consolas"}):
            #print(wr.prettify())
            #print('XXXNEW')
            for w in wr.find_all(re.compile('w:(t|br)')):
                bold = wr.find('w:b', attrs={'w:val': '1'}) is not None
                italic = wr.find('w:i', attrs={'w:val': '1'}) is not None
                if w.name == 'w:t':
                    text = w.text
                    if convert is not None:
                        text = convert(text, bold, italic)
                    block.write(text)
                    #print('XXX', w.text)
                elif w.name == 'w:br':
                    block.write('\n')
                    #print('XXX', w.text)
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


def GetMarkdownBlocks(lines: List[str]) -> List[Tuple[int, int, str]]:
    """Get the list of blockquotes from the markdown file (and 
    their positions)."""
    blocks = []
    block = []
    for index, line in enumerate(lines):
        if save(re.match(r'> (.*)\\?', line)):
            text = save.value.group(1).rstrip('\\').replace(r'\...', '...')
            block.append((index, text))
        elif block:
            blocks.append((block[0][0],
                           block[-1][0],
                           [line for _, line in block]))
            block = []
    if block:
        blocks.append((block[0][0],
                       block[-1][0],
                       [line for _, line in block]))
    return blocks


def GetRstBlocks(lines: List[str]) -> List[Tuple[int, int, str]]:
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
        if (inlens[-3] == (0,0) and
            inlens[-2][0] == 4 and inlens[-2][1] > 0 and
            inlens[-1] == (0,0)):
            lines[index-1] = "    | {}".format(lines[index-1].lstrip())

    blocks = []
    block = []
    for index, line in enumerate(lines):
        if save(re.match(r'    \| (.*)', line)):
            text = save.value.group(1)
            block.append((index, text))
        elif block and save(re.match(r'      (.*)', line)):
            text = save.value.group(1)
            index, prev_text = block.pop(-1)
            block.append((index, "{} {}".format(prev_text, text)))
        elif block:
            blocks.append((block[0][0],
                           block[-1][0],
                           [line for _, line in block]))
            block = []
    if block:
        blocks.append((block[0][0],
                       block[-1][0],
                       [line for _, line in block]))
    return blocks


def compute_key(lines: List[str], remove_matching_stars: bool=True) -> str:
    """Reduce a snippet to a comparable key."""
    newlines = []
    for line in lines:
        # Remove ellipsis
        line = line.replace("…", "")
        line = line.replace("...", "")
        # Compress whitespace.
        line = re.sub(r'[ \t]+', ' ', line)
        if remove_matching_stars:
            for pattern in [r'\*\*(.*?)\*\*', r'\*(.*?)\*']:
                n = 1
                while n > 0:
                    line, n = re.subn(pattern, r'\1', line)
        line = line.strip()
        if line:
            newlines.append(line)
    return ''.join(newlines)


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('docx', help='Docx filename')
    parser.add_argument('markdown', help='Markdown filename')
    parser.add_argument('output', help='Output Markdown filename with replaced blocks')
    args = parser.parse_args()

    blocks_docx = GetDocxBlocks(args.docx, ConvertToRst)
    lines_txt = open(args.markdown).readlines()
    blocks_txt = GetRstBlocks(lines_txt)

    docx = [(compute_key(block, True), block)
            for block in blocks_docx]
    map_docx = dict(docx)
    map_txt = {compute_key(block, True): (minline, maxline, block)
               for minline, maxline, block in blocks_txt}

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
        print('MATCHING')
        for key, (minline, maxline), block_txt, block_docx in matches:
            print('-' * 120)
            print('DOCX')
            for line in block_docx:
                print(repr(line))
            print('TXT')
            for line in block_txt:
                print(repr(line))
            
    #print('=' * 120)
    if map_docx:
        print('NOTFOUND DOCX:')
        for key, block in map_docx.items():
            print('-' * 120)
            for line in block:
                print(repr(line))
    if map_txt:
        print('NOTFOUND TXT:')
        for key, (minline, maxline, block) in map_txt.items():
            print('-' * 120)
            for line in block:
                print(repr(line))

    # Replace blocks in the original md file.
    offset = 0
    for key, (minline, maxline), block_txt, block_docx in matches:
        minline += offset
        maxline += offset
        del lines_txt[minline:maxline+1]
        new_lines = ['    | {}\n'.format(line) for line in block_docx]
        lines_txt[minline:minline] = new_lines
        offset += len(new_lines) - (maxline + 1 - minline)
        
    # Output the result.
    with open(args.output, 'w') as outfile:
        for line in lines_txt:
            outfile.write(line)


if __name__ == '__main__':
    main()

#!/usr/bin/env python3
"""Count and report the number of arguments used to call a particular function in a codebase.
"""
import ast
import os
import argparse
import logging
from os import path


def find_files(rootdir):
    if path.isfile(rootdir):
        yield rootdir
    for root, dirs, files in os.walk(rootdir):
        for filename in files:
            if filename.endswith('.py'):
                yield path.join(root, filename)


def get_name(node):
    if isinstance(node, ast.Name):
        return node.id
    elif isinstance(node, ast.Attribute):
        return node.attr


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('root', help='Root directory to traverse')
    parser.add_argument('funcname', action='store')
    parser.add_argument('numargs', action='store', type=int)

    args = parser.parse_args()

    stdout = open(1, 'w', encoding='utf-8', closefd=False) # fd 1 is stdout

    for filename in find_files(args.root):
        #logging.info("Processing %s", filename)
        code = ast.parse(open(filename, encoding='utf8').read(), filename)
        for node in ast.walk(code):
            if isinstance(node, ast.Call):
                # stdout.write(ast.dump(node))
                # stdout.write('\n')
                if get_name(node.func) == args.funcname:
                    if len(node.args) != args.numargs:
                        stdout.write('{}:{}:\n'.format(filename, node.lineno))
                        stdout.write(ast.dump(node.func))
                        stdout.write('\n')
                        stdout.write('{}\n'.format(len(node.args)))


if __name__ == '__main__':
    main()

#!/usr/bin/env python3
"""Row iteration on an arrow dataset, in Python (not fast).
"""

import argparse
import logging
import collections

import pyarrow
import pyarrow.csv
import pyarrow.parquet as parquet


def row_iter(table: pyarrow.Table):
    """Iterator row over row."""
    # pylint: disable=invalid-name
    Row = collections.namedtuple("Row", table.column_names)
    for index in range(table.num_rows):
        row = table.slice(index, 1)
        obj = Row(*(col[0].as_py() for col in row.itercolumns()))
        yield obj


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('filename', help='Input filename.')
    args = parser.parse_args()

    table = parquet.read_table(args.filename)
    print(table.schema)

    for row in row_iter(table.select(['CRIM', 'NOX'])):
        print(row.CRIM, row.NOX)



if __name__ == '__main__':
    main()

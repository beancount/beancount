#!/usr/bin/env python3
"""A simple converter from CSV to Arrow file format.
"""

from os import path
import argparse
import logging
import pyarrow.csv
import pyarrow.feather as feather
import pyarrow.parquet as parquet

def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('csv_filename', help='CSV filename.')
    parser.add_argument('arrow_filename', help='Arrow filename.')

    parser.add_argument('-f', '--format', action='store',
                        help="File format to write the file to.")
    parser.add_argument('--skip-rows', type=int, help='Optional num rows to skip.')
    parser.add_argument('--column-names', help='Optional columns names.')
    args = parser.parse_args()

    # Figure out the file format.
    valid_formats = {'feather', 'parquet'}
    default_format = 'parquet'
    if args.format in valid_formats:
        format = args.format
    else:
        # Try to infer from the filename.
        format = path.splitext(args.arrow_filename)[1][1:]
        if format not in valid_formats:
            format = default_format
    assert format in valid_formats

    column_names = args.column_names.split(",") if args.column_names else None
    readopts = pyarrow.csv.ReadOptions(skip_rows=args.skip_rows,
                                       column_names=column_names)
    table = pyarrow.csv.read_csv(args.csv_filename, read_options=readopts)
    print(table)

    writer = feather.write_feather if format == 'feather' else parquet.write_table
    writer(table, args.arrow_filename)



if __name__ == '__main__':
    main()

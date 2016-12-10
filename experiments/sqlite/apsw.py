#!/usr/bin/env python3
"""Testing out APSW for Beancount.
"""
__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

import os
import sys
import time
import apsw
import argparse
import logging
import contextlib

from beancount import loader
from beancount.core import data


def get_transactions(filename):
    entries, _, options_map = loader.load_file(filename)
    columns = ['rowid', 'date', 'narration']
    rows = [(index, txn.date.isoformat(), txn.narration)
            for index, txn in enumerate(data.filter_txns(entries))]
    return columns, rows

    # columns=None
    # data=[]
    # counter=1
    # for directory in directories:
    #     for f in os.listdir(directory):
    #         if not os.path.isfile(os.path.join(directory,f)):
    #             continue
    #         counter+=1
    #         st=os.stat(os.path.join(directory,f))
    #         if columns is None:
    #             columns=["rowid", "name", "directory"]+[x for x in dir(st) if x.startswith("st_")]
    #         data.append( [counter, f, directory] + [getattr(st,x) for x in columns[3:]] )
    # return columns, data

class Source:

    def Create(self, db, modulename, dbname, tablename, *args):
        columns, data = get_transactions(args[0].strip('"'))
        schema = "CREATE TABLE x("+','.join(["'%s'" % (x,) for x in columns[1:]])+")"
        return schema, Table(columns, data)

    Connect = Create


class Table:

    def __init__(self, columns, data):
        self.columns = columns
        self.data = data

    def BestIndex(self, *args):
        return None

    def Open(self):
        return Cursor(self)

    def Disconnect(self):
        pass

    Destroy = Disconnect


class Cursor:
    def __init__(self, table):
        self.table = table

    def Filter(self, *args):
        self.pos = 0

    def Eof(self):
        return self.pos >= len(self.table.data)

    def Rowid(self):
        return self.table.data[self.pos][0]

    def Column(self, col):
        return self.table.data[self.pos][1+col]

    def Next(self):
        self.pos += 1

    def Close(self):
        pass


def main():
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('filename', action='store')
    args = parser.parse_args()

    try:
        with contextlib.closing(apsw.Connection("dbfile")) as conn:
            cursor = conn.cursor()
            conn.createmodule("beancount", Source())

            cursor.execute("""
              CREATE VIRTUAL TABLE transactions USING beancount("{}");
            """.format(args.filename))

            for row in cursor.execute("""
              SELECT date, narration FROM transactions;
            """):
                print(row)


            conn.close(True)
    finally:
        os.remove("dbfile")


if __name__ == '__main__':
    main()

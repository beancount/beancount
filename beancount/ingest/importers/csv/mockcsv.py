"""
MockCSV.py
Class that given a config will mock matching csv files and entries.
This is not designed to be comprehensive
"""
__copyright__ = "Copyright (C) 2018  Michael Droogleever"
__license__ = "GNU GPLv2"

import datetime
import csv as python_csv
import string
import random
import io
from typing import Union, List, Tuple, Iterable, Optional

from beancount.core import data
from beancount.parser import printer


class MockCSV(object):
    """
    docstring for MockCSV.
    """

    BASE_DATE = datetime.date(2016, 7, 10)
    BASE_LENGTH = 10
    BASE_END_DATE = datetime.date(2016, 7, 19)

    def __init__(
            self, values: list, *,
            csv_dialect: python_csv.Dialect = None,
            write_excel_sep: bool = False,
            header: Optional[list] = None,
            dates: Union[int,
                         List[datetime.date],
                         Tuple[datetime.date, Iterable[int]]]=0,
            date_format: str = "%d-%b-%Y",
            txn_kwargs: dict = {},
            posting_kwargs: dict = {},
            seed=0,
    ):
        self.random = random.Random(seed)

        self.write_excel_sep = write_excel_sep
        self.header_fieldnames = header
        self.csv_dialect = csv_dialect
        self.txn_kwargs = txn_kwargs
        self.posting_kwargs = posting_kwargs

        if isinstance(dates, (int, tuple)):
            try:
                base_date, date_range = dates
            except TypeError:
                base_date = self.BASE_DATE
                date_range = range(0, dates if dates > 0 else self.BASE_LENGTH)
            self.date_list = [base_date + datetime.timedelta(days=x)
                              for x in date_range]
        else:
            # If already a list
            self.date_list = dates

        self.length = len(self.date_list)
        self.datestr_list = [_.strftime(date_format) for _ in self.date_list]

        self.csv_config = []
        self.bean_config = []
        for val in values:
            datestr, bean_data = self.replace_type(val)
            self.csv_config.append(datestr)
            self.bean_config.append((val, bean_data))

    def replace_type(self, type_str):
        dbl = lambda x: (x, x)
        if "date" in type_str:
            return self.datestr_list, self.date_list
        return dbl(self._replace_type(type_str))

    def _replace_type(self, type_str):
        if "text" in type_str:
            return [self.random.choice(string.ascii_uppercase) for _ in range(self.length)]
        if "currency" in type_str:
            return [self.random.choice(["USD", "EUR", "BTC"]) for _ in range(self.length)]
        if "num" in type_str:
            num = [int(1+random.expovariate(2e-2)) for _ in range(self.length)]
            if "neg" in type_str:
                num = [-x for x in num]
            return num
        raise Exception("Incorrect MockCSV config")

    def csv_rows(self):
        return zip(*self.csv_config)

    def mock_csv(self):
        with io.StringIO() as csvoutput:
            if self.write_excel_sep:
                csvoutput.write(f"sep={self.csv_dialect.delimiter}\n")
            csvwriter = python_csv.writer(csvoutput, dialect=self.csv_dialect)
            if self.header_fieldnames:
                csvwriter.writerow(self.header_fieldnames)
            csvwriter.writerows(self.csv_rows())
            return csvoutput.getvalue()

    def bean_txns(self):
        entries = []
        keys, data_lists = zip(*self.bean_config)
        for data_zip in zip(*data_lists):
            kwargs = {**{
                'meta': {},
                'flag': '*',
                'payee': None,
                'tags': set(),
                'links': set(),
                'postings': [],
            }, **self.txn_kwargs}
            posting_kwargs = None
            for key, data_point in zip(keys, data_zip):
                if "date" in key:
                    kwargs["date"] = data_point
                if "payee" in key:
                    kwargs["payee"] = data_point
                if "narration" in key:
                    if "+" in key:
                        kwargs["narration"] = "{}+{}".format(kwargs["narration"],
                                                             data_point)
                    else:
                        kwargs["narration"] = data_point
                if "tag" in key:
                    kwargs["tags"].add(data_point)
                if "link" in key:
                    kwargs["links"].add(data_point)
                if "posting" in key:
                    if posting_kwargs is None:
                        posting_kwargs = {**{
                            'cost': None,
                            'price': None,
                            'flag': None,
                            'meta': {},
                        }, **self.posting_kwargs}
                    if "account" in key:
                        posting_kwargs["account"] = data_point
                    if "amount" in key:
                        posting_kwargs["amount"] = data.D(data_point)
                    if "currency" in key:
                        posting_kwargs["currency"] = data_point
            if posting_kwargs is not None:
                posting_kwargs["units"] = data.Amount(posting_kwargs["amount"],
                                                      posting_kwargs["currency"])
                del posting_kwargs["amount"]
                del posting_kwargs["currency"]
                # pylint: disable=not-callable
                kwargs["postings"].append(data.Posting(**posting_kwargs))
            # pylint: disable=not-callable
            entries.append(data.Transaction(**kwargs))
        return entries

    def mock_bean(self):
        with io.StringIO() as beanoutput:
            entries = self.bean_txns()
            printer.print_entries(entries, file=beanoutput)
            return beanoutput.getvalue()

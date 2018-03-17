"""
CSV importer.
"""
__copyright__ = "Copyright (C) 2016 Martin Blais, 2018 Michael Droogleever"
__license__ = "GNU GPLv2"

import csv as python_csv
import datetime
import enum
import collections
import typing
from inspect import signature
from io import StringIO
from os import path

from beancount.utils.date_utils import parse_date_liberally
from beancount.core import data
from beancount.ingest import importer

# def create_enum(namedtuple):
#     """
#     Create Enum based on namedtuple
#     """
#     return enum.Enum(
#         namedtuple.__name__,
#         {key.upper(): f"[{key.upper()}]"
#          for key in namedtuple.__annotations__.keys()}
#     )

def create_enum(nt):
    """
    Create Enum based on namedtuple
    """
    return enum.Enum(
        nt.__name__,
        {key.upper(): f"[{key.upper()}]" for key in signature(nt).parameters}
    )


# The set of Transaction properties.
class Props:
    """
    keys for the CSVImporter config
    e.g.
    # The settlement date, the date we should create the posting at.
    Transaction.DATE = '[DATE]'
    # The payee field.
    Transaction.PAYEE = '[PAYEE]'
    # The narration field.
    Transaction.NARRATION = '[NARRATION]'
    """
    BALANCES = "[BALANCES]"
    TRANSACTIONS = "[TRANSACTIONS]"
    Transaction = create_enum(data.Transaction)
    Balance = create_enum(data.Balance)
    Posting = create_enum(data.Posting)
    CostSpec = create_enum(data.CostSpec)
    Cost = create_enum(data.Cost)
    Amount = create_enum(data.Amount)


class RowFunc:
    """
    Store a value in a CSVConfig that is returned by a given function
    """
    def __init__(self, val):
        self.func, self.fieldnames = val
    def get_val(self, field_map, row):
        args = []
        for fieldname in self.fieldnames:
            if isinstance(fieldname, int):
                args.append(row[fieldname])
            else:
                args.append(row[field_map[fieldname]])
        return self.func(args)


class RowIdx:
    """
    Store a value in a CSVConfig that needs to be looked up in a csv row
    """
    def __init__(self, val, parse_type=None):
        self.parse_type = parse_type
        self.val = val
        if val is None:
            raise Exception(f"RowIdx cannot be None: {val}")
    def get_val(self, field_map, row, dateutil_kwds=None):
        if isinstance(self.val, BeanConfig.Const):
            val = str(self.val)
        elif isinstance(self.val, str):
            if field_map is None:
                raise BeanConfig.BeanConfigError("CSVConfig config uses strings to index the header, but no header line was detected. Try setting csv_options['header'] = True")
            if self.val not in field_map:
                raise KeyError(f"{self.val} not in CSV header")
            val = row[field_map[self.val]]
        elif isinstance(self.val, int):
            val = row[self.val]
        if self.parse_type == data.Decimal:
            if isinstance(val, str):
                val = data.D(val)
        elif self.parse_type == datetime.date:
            if not isinstance(val, str):
                raise BeanConfig.BeanConfigError(f"Cannot parse {type(val)} as date")
            val = parse_date_liberally(val, dateutil_kwds)
        return val



class BeanConfig(object):
    """
    docstring for BeanConfig.
    """
    def __init__(self, bean_class, config):
        super().__init__()
        self.config = config
        self.bean_class = bean_class
        self.iconfig = self.rec_parse(bean_class, config)

    class Const(str):
        """
        Use to pass a constant str, instead of a csv fieldname or index
        """
        pass

    class BeanConfigError(ValueError):
        pass

    def items(self):
        return self.iconfig.items()

    @classmethod
    def rec_parse(cls, bean_type, config):
        """
        Parses the provided config into an iconfig
        rec_parse -> parse_keyval -> (parse_typing, parse_simple)
        Args:
            bean_type:
            config:
        Returns: iconfig dict
            dict with the same structure as the config argument,
            but containing only kwarg parameter names as keys and
            iconfig, RowIdx, or RowFunc objects as vals
        """
        iconfig = {}
        # Iterate through OrderedDict parameters of {name: type}
        for param_name, type_val in bean_type.__annotations__.items():
            # Get key to self.config, lookup in Enum
            prop_enum = getattr(Props, bean_type.__name__)
            config_key = getattr(prop_enum, param_name.upper())
            # Get value from config if key in config
            try:
                config_val = config[config_key]
            except KeyError:
                # Not in config, use default
                config_val = None
            iconfig[param_name] = cls.parse_keyval(bean_type, config_val, param_name, type_val)
        return iconfig

    def recursive_construct(self, field_map, dateutil_kwds, meta, row, config):
        """
        Constructs the data.X object
        recursive_construct -> get_kwargs -> get_arg
        Args:
            field_map
            meta
            dateutil_kwds
            row
            config
        """
        kwargs = self.get_kwargs(field_map, meta, dateutil_kwds, row, config)
        if self.bean_class == data.Posting:
            if kwargs['price'] is not None:
                if kwargs['price'].number == data.Decimal():
                    kwargs['price'] = None
        #pylint: disable=not-callable
        return self.bean_class(**kwargs)

    @classmethod
    def get_kwargs(cls, field_map, meta, dateutil_kwds, row, config):
        """
        Gets the kwargs to construct the data.X object.
        Args:
            field_map:
            meta:
            dateutil_kwds:
            row: csv row item.
            config: dict of object_kwarg istantiation pairs
                e.g. {'date': <RowIdx>, 'narration': "BEANCOUNT SUBSCRIPTION"}
        """
        kwargs = {}
        for key, val in config.items():
            kwargs[key] = cls.get_arg(field_map, meta, dateutil_kwds, row, key, val)
        return kwargs

    @classmethod
    def get_arg(cls, field_map, meta, dateutil_kwds, row, param_name, arg):
        """
        Gets the argument for the kwarg to construct the data.X object.
        Args:
            field_map: dict of field names to integers
            meta: meta dict
            dateutil_kwds:
            row: csv row item.
            param_name: the parameter name for kwarg instatiation
            param_name: the parameter value, argument, for kwarg instatiation}
        """
        if isinstance(arg, BeanConfig):
            return arg.recursive_construct(field_map, meta, dateutil_kwds, row, arg)
        elif isinstance(arg, RowFunc):
            return arg.get_val(field_map, row)
        elif isinstance(arg, RowIdx):
            return arg.get_val(field_map, row, dateutil_kwds)
        elif isinstance(arg, dict):
            ret = {}
            if param_name == 'meta':
                ret = meta
            ret = {
                **ret,
                **{_key: cls.get_arg(field_map, meta, dateutil_kwds, row, _key, _val)
                   for _key, _val in arg.items()},
            }
            return ret
        elif isinstance(arg, (list, tuple, set)):
            if arg == set():
                return data.EMPTY_SET
            seq = [cls.get_arg(field_map, meta, dateutil_kwds, row, param_name, item)
                   for item in arg]
            if isinstance(arg, set):
                return set(seq)
            return seq
        elif isinstance(arg, str):
            return arg
        elif not arg:
            return arg
        raise cls.BeanConfigError(f"Unknown {param_name}: {arg}\n")

    @classmethod
    def parse_keyval(cls, bean_type, config_val, param_name, type_val):
        """
        Args:
            bean_type: e.g. data.Posting
            config_val e.g. {...}, None, int(), str()
            param_name e.g. 'account', 'payee'
            type_val e.g. str, typing.typing.Union[str, None], set, data.Posting
        """
        ## Func type
        try:
            if callable(config_val[0]) and isinstance(config_val[1],
                                                      (list, tuple)):
                return RowFunc(config_val)
        except (KeyError, TypeError):
            pass

        ## Amount type
        if type_val is data.Amount:
            # Normal formatting
            if isinstance(config_val, dict):
                return BeanConfig(type_val, config_val)
            # Tuple simplification
            elif isinstance(config_val, (tuple, list, set)):
                return BeanConfig(type_val, {
                    Props.Amount.NUMBER: config_val[0],
                    Props.Amount.CURRENCY: config_val[1],
                })
            # String formatting e.g. "13.69 USD"
            #TODO use Amount.from_string
            elif isinstance(config_val, str):
                *num, cur = config_val.split(' ')
                return BeanConfig(type_val, {
                    Props.Amount.NUMBER: ' '.join(num),
                    Props.Amount.CURRENCY: cur,
                })

        ## Bean type
        try:
            enum_name = type_val.__name__
        except AttributeError:
            enum_name = None
        if enum_name and hasattr(Props, enum_name):
            if config_val is None:
                raise cls.BeanConfigError("{} requires a {}: {}".format(
                    bean_type.__name__,
                    param_name.upper(),
                    type_val.__name__))
            # Recursively return config from below
            return BeanConfig(type_val, config_val)

        simple_types = (dict, list, tuple, set, datetime.date, str, typing.Any, int, Const, data.Decimal)

        ## Typing type
        if type_val not in simple_types:
            return cls.parse_typing(bean_type, config_val, param_name, type_val)

        ## Simple type
        return cls.parse_simple(config_val, type_val, param_name)

    @classmethod
    def parse_typing(cls, bean_type, config_val, param_name, type_val):
        try:
            super_type = type_val.__extra__
        # dict
            if super_type == dict:
                if config_val is None:
                    return {}
                ret_dict = {}
                for key_content, val_content in config_val.items():
                    key_type, val_type = type_val.__args__
                    # key_obj = cls.parse_simple(key_content, key_type)
                    key_obj = key_content
                    new_param_name = f"{param_name}[{key_content}]"
                    val_obj = cls.parse_keyval(bean_type, val_content, new_param_name, val_type)
                    ret_dict[key_obj] = val_obj
                return ret_dict
        # list, set, tuple
            if super_type in (list, set, tuple):
                if config_val is None:
                    return super_type()
                try:
                    list_type = type_val.__args__[0]
                except TypeError: # If typing has not defined type __args__ is None
                    list_type = str
                seq = [cls.parse_keyval(bean_type, item, param_name, list_type)
                       for item in config_val]
                if super_type is set:
                    return set(seq)
                return seq
        # typing.Union
        except AttributeError:
            if config_val is None and type(None) in type_val.__args__:
                return None
            if {*type_val.__args__, type(None)} == {data.Cost, data.CostSpec, type(None)}:
                if Props.Cost.NUMBER in config_val:
                    return BeanConfig(data.Cost, config_val)
                else:
                    return BeanConfig(data.CostSpec, config_val)
            for sub_type in [_ for _ in type_val.__args__ if _ is not type(None)]:
                return cls.parse_keyval(bean_type, config_val, param_name, sub_type)

    @classmethod
    def parse_simple(cls, config_val, type_val, param_name=None):
        """
        Simple types and sequences of simple types
        """
        # decimal
        if type_val == data.Decimal:
            return RowIdx(config_val, parse_type=data.Decimal)
        # date
        elif type_val == datetime.date:
            if config_val is None:
                raise cls.BeanConfigError("Date required in config")
            return RowIdx(config_val, parse_type=datetime.date)
        # static string
        elif isinstance(config_val, cls.Const):
            return RowIdx(config_val)
        # sequence, we assume of simple items
        elif type_val in (list, tuple, set):
            if config_val is None:
                # return empty
                if type_val is set:
                    return data.EMPTY_SET
                return type_val()
            seq = [cls.parse_simple(item, type(item)) for item in config_val]
            if type_val is set:
                return set(seq)
            return seq
        # dict, we assume of simple items, we assume the key is a Const str
        elif type_val is dict:
            if config_val is None:
                return {}
            ret_dict = {}
            for key_content, val_content in config_val.items():
                _key_type, val_type = type_val.__args__
                key_obj = Const(key_content)
                val_obj = cls.parse_simple(val_content, val_type)
                ret_dict[key_obj] = val_obj
            return ret_dict
        # str,
        # typing.Any we assume to be a string if it has passed all other checks
        elif type_val in (str, typing.Any):
            if config_val is None:
                # FIXME, remove hard coded flag
                if param_name == "flag":
                    return "*"
                else:
                    raise cls.BeanConfigError(f"{param_name}:{type_val} not optional")
            return RowIdx(config_val)
        raise cls.BeanConfigError(f"Unknown type: {type_val}")


# Make Const a module level import
Const = BeanConfig.Const

class CSVConfig:
    """docstring for CSVConfig."""
    def __init__(self, config, dateutil_kwds=None):
        super().__init__()
        self.config = config
        self.transaction_config = BeanConfig(
            data.Transaction, config[Props.TRANSACTIONS])
        if Props.BALANCES not in self.config:
            self.balance_configs = []
        else:
            self.balance_configs = [BeanConfig(data.Balance, balance_config)
                                    for balance_config in config[Props.BALANCES]]
        self.dateutil_kwds = dateutil_kwds

    def get_date(self, field_map, row):
        tnx = self.get_transaction(field_map=field_map, meta={}, row=row)
        return tnx.date

    def get_account(self, field_map, row):
        tnx = self.get_transaction(field_map=field_map, meta={}, row=row)
        try:
            return tnx.postings[0].account
        except IndexError: # No posting
            return "No account name"

    def get_balances(self, field_map, meta, row):
        #pylint: disable=not-callable
        return [
            data.Balance(
                **balance_config.get_kwargs(
                    field_map, meta, self.dateutil_kwds, row, balance_config))
            for balance_config in self.balance_configs
        ]

    def get_transaction(self, field_map, meta, row):
        #pylint: disable=not-callable
        return data.Transaction(
            **self.transaction_config.get_kwargs(
                field_map, meta, self.dateutil_kwds, row,
                self.transaction_config.iconfig))


class CSVImporter(importer.ImporterProtocol):
    """Importer for CSV files."""

    def __init__(
            self,
            config: typing.Dict, *,
            dateutil_kwds: typing.Optional[typing.Dict] = None,
            csv_options: typing.Optional[typing.Dict] = None,
            debug: bool = False,
        ):
        """
        Constructor
        Args:
            config: dict containing config
            dateutil_kwds: An optional dict
                defining the dateutil parser kwargs.
            csv_options: A dict with the following optional keys:
                dialect: a csv.Dialect
                comment: string that comments out lines
                skip_lines: number of lines to skip at the start of the file
                header: the fieldnames of the csv, will not attempt to Sniff
                truncate_lines: number of lines to stop short of at the end
            debug: Whether or not to print debug information
        """
        self.dateutil_kwds = dateutil_kwds
        csv_options = {} if csv_options is None else csv_options
        self.csv_dialect = csv_options.get('dialect', None)
        if isinstance(self.csv_dialect, str):
            self.csv_dialect = getattr(python_csv, self.csv_dialect)
        if self.csv_dialect is not None and not issubclass(self.csv_dialect, python_csv.Dialect):
            raise ValueError(
                f"CSVImporter csv_options['dialect'] type must be subclass of csv.Dialect, not {self.csv_dialect}")
        self.comment = csv_options.get('comment', ('#',))
        self.skip_lines = csv_options.get('skip_lines', 0)
        self.header = csv_options.get('header', None)
        self.truncate_lines = csv_options.get('truncate_lines', 0)
        for name, item in zip(("skip_lines", "truncate_lines"),
                              (self.skip_lines, self.truncate_lines)):
            if not isinstance(item, int):
                raise ValueError(
                    f"CSVImporter csv_options['{name}'] type must be int, not {type(item)}")
        self.debug = debug

        self.csvconfig = CSVConfig(config, self.dateutil_kwds)

    def identify(self, file):
        if file.mimetype() != 'text/csv':
            return False
        super_identify = super().identify(file)
        return super_identify if super_identify is not None else True

    def file_account(self, file):
        return self.csvconfig.get_account(
            field_map=self.get_fieldmap(file),
            row=next(self.csv_reader(file)),
        )

    def file_name(self, file):
        filename = path.splitext(path.basename(file.name))[0]
        return '{}.csv'.format(filename)

    def file_date(self, file):
        """
        Get the maximum date from the file.
        """
        max_date = None
        for row in self.csv_reader(file):
            date = self.csvconfig.get_date(
                field_map=self.get_fieldmap(file),
                row=row,
            )
            if max_date is None or date > max_date:
                max_date = date
        return max_date

    def extract(self, file):
        entries = []
        Row = collections.namedtuple('Row', ("index", "row"))
        first_row = last_row = None
        field_map = self.get_fieldmap(file)
        for index, row in enumerate(self.csv_reader(file), 1):
            # If debugging, print out the rows.
            if self.debug:
                print(row)
            # Track first and last rows
            if first_row is None:
                first_row = Row(index, row)
            last_row = Row(index, row)

            meta = data.new_metadata(file.name, index) or {}
            txn = self.csvconfig.get_transaction(
                field_map=field_map,
                meta=meta,
                row=row,
            )

            # Add the transaction to the output list
            entries.append(txn)

        # Figure out if the file is in ascending or descending order.
        first_date = self.csvconfig.get_date(
            field_map=field_map,
            row=first_row.row,
        )
        last_date = self.csvconfig.get_date(
            field_map=field_map,
            row=last_row.row,
        )
        is_ascending = first_date < last_date

        # Reverse the list if the file is in descending order
        if not is_ascending:
            first_row, last_row = last_row, first_row
            first_date, last_date = last_date, first_date
            entries = list(reversed(entries))

        # Add a balance entry if possible
        if entries:
            idx = len(entries) if is_ascending else 0
            for balance in self.csvconfig.get_balances(
                    field_map=field_map,
                    meta=data.new_metadata(file.name, last_row.index),
                    row=last_row.row,
            ):
                entries.insert(idx, balance)

        return entries

    def sniff(self, ifile):
        """
        Parses file to look for header and dialect.
        Args:
            ifile: FileMemo
        Returns:
            csv_dialect: csv.Dialect subclass
            skip_lines
            header: False or list of fieldnames
        """
        csv_dialect = self.csv_dialect
        delimiter = None
        skip_lines = self.skip_lines
        header = self.header
        # Sniff delimeter
        with StringIO(ifile.contents(), newline='') as csvfile:
            # Skip garbage lines, pylint: disable=R1708,C0321
            for _ in range(skip_lines): csvfile.readline()
            sep = csvfile.readline()
            if sep.startswith("sep="):
                skip_lines += 1
                delimiter = sep[4]
        # Sniff dialect
        with StringIO(ifile.contents(), newline='') as csvfile:
            # Skip garbage lines, pylint: disable=R1708,C0321
            for _ in range(skip_lines): csvfile.readline()
            try:
                csv_dialect = python_csv.Sniffer().sniff(csvfile.read())
            except python_csv.Error:
                pass
        # Sniffs for header
        if header is None:
            with StringIO(ifile.contents(), newline='') as csvfile:
                # Skip garbage lines, pylint: disable=R1708,C0321
                for _ in range(skip_lines): csvfile.readline()
                reader = python_csv.reader(csvfile, dialect=csv_dialect)
                try:
                    header = python_csv.Sniffer().has_header(csvfile.read())
                except python_csv.Error:
                    # TODO check if config contains any fieldname strings
                    header = True
                    raise Exception("The existence of a CSV header line could not be determined, please set the 'header' key in the CSVImporter csv_options")
        # Get header
        if header:
            with StringIO(ifile.contents(), newline='') as csvfile:
                # Skip garbage lines, pylint: disable=R1708,C0321
                for _ in range(skip_lines): csvfile.readline()
                reader = python_csv.reader(csvfile, dialect=csv_dialect)
                header = next(reader)
        return csv_dialect, delimiter, skip_lines, header

    def get_fieldmap(self, ifile):
        """
        Returns fieldmap from file's header, map of fieldname to field index
        """
        _, _, _, header = self.sniff(ifile)
        if header:
            return {field_name.strip(): index
                    for index, field_name in enumerate(header)}
        # TODO: push this up into RowIdx and RowFunc, pass around references to file instead
        # so this can raise an Error imediately when getting fieldmap from file without a header
        # Maybe subclass FileMemo into a special CSVFile
        return None

    def csv_reader(self, ifile):
        """
        Returns a csv.reader having skipped to the first line of content
        """
        csv_dialect, delimiter, skip_lines, header = self.sniff(ifile)
        with StringIO(ifile.contents(), newline='') as csvfile:
            # Skip garbage lines, pylint: disable=R1708,C0321
            for _ in range(skip_lines): csvfile.readline()
            if delimiter is None:
                reader = python_csv.reader(csvfile, dialect=csv_dialect)
            else:
                reader = python_csv.reader(csvfile,
                                           dialect=csv_dialect,
                                           delimiter=delimiter)
            # Skip header, if one was detected.
            if header:
                next(reader) # pylint: disable=R1708
            # A deque is used to truncate lines:
            # when the end is reached,
            # the deque will have witheld the lines which need to be truncated
            row_deque = collections.deque(
                (next(reader) for _ in range(self.truncate_lines)))
            for row in reader:
                row_deque.append(row)
                _row = row_deque.popleft()
                if not _row:
                    continue
                # Skip comments
                if _row[0].startswith(self.comment):
                    continue
                yield _row

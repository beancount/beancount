# we have a wrapper there in-case we need to wrap a rust function with some python code

import copy

from beancount.core.data import Booking
from beancount.core.data import Open

from . import _parser_rust
from . import options as _options


def build_options_map(filename: str):
    opts = copy.deepcopy(_options.OPTIONS_DEFAULTS)
    opts["filename"] = filename
    opts["include"] = [filename]
    return opts


load_file = _parser_rust.load_file
parse_string = _parser_rust.parse_string

__all__ = ["Booking", "Open", "build_options_map", "load_file", "parse_string"]

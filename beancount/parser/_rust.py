# we have a wrapper there in-case we need to wrap a rust function with some python code

import copy

from . import _parser_rust
from . import options as _options


def build_options_map(filename: str):
    opts = copy.deepcopy(_options.OPTIONS_DEFAULTS)
    opts["filename"] = filename
    opts["include"] = [filename]
    return opts


__version__: str = _parser_rust.__version__
load_file = _parser_rust.load_file
parse_string = _parser_rust.parse_string

__all__ = ["__version__", "build_options_map", "load_file", "parse_string"]

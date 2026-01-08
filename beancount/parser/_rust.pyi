from typing import Any
from typing import Dict
from typing import List
from typing import Tuple

__all__ = ["build_options_map", "load_file", "parse_string"]

def load_file(filename: str) -> Tuple[List[Any], List[Any], Dict[str, Any]]: ...
def parse_string(
    content: str, filename: str | None
) -> Tuple[List[Any], List[Any], Dict[str, Any]]: ...
def build_options_map(filename: str | None = ...) -> Dict[str, Any]: ...

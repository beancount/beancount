import enum
from datetime import date
from typing import Any
from typing import Dict
from typing import List
from typing import NamedTuple
from typing import Tuple

__all__ = ["Booking", "Open", "build_options_map", "load_file", "parse_string"]

class Booking(enum.Enum):
    # Reject ambiguous matches with an error.
    STRICT = "STRICT"

    # Strict booking method, but disambiguate further with sizes. Reject
    # ambiguous matches with an error but if a lot matches the size exactly,
    # accept the oldest.
    STRICT_WITH_SIZE = "STRICT_WITH_SIZE"

    # Disable matching and accept the creation of mixed inventories.
    NONE = "NONE"

    # Average cost booking: merge all matching lots before and after.
    AVERAGE = "AVERAGE"

    # First-in first-out in the case of ambiguity.
    FIFO = "FIFO"

    # Last-in first-out in the case of ambiguity.
    LIFO = "LIFO"

    # Highest-in first-out in the case of ambiguity.
    HIFO = "HIFO"

class Open(NamedTuple):
    meta: Dict[str, Any]
    date: date
    account: str
    currencies: list[str]
    booking: Booking | None

def load_file(filename: str) -> Tuple[List[Any], List[Any], Dict[str, Any]]: ...
def parse_string(
    content: str, filename: str | None = ...
) -> Tuple[List[Any], List[Any], Dict[str, Any]]: ...
def build_options_map(filename: str | None = ...) -> Dict[str, Any]: ...

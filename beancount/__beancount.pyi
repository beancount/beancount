from decimal import Decimal
from typing import Optional


class Amount:
    number: Optional[Decimal]
    currency: str

    def __init__(self, number: Optional[str | int | float], currency: str) -> None:
        ...

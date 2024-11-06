import datetime
from decimal import Decimal
from typing import Optional, Dict, Any

Account = str
Currency = str
Meta = Dict[str, Any]


# A set of valid booking method names for positions on accounts.
# See http://furius.ca/beancount/doc/inventories for a full explanation.
class Booking:
    # Reject ambiguous matches with an error.
    STRICT = "STRICT"

    # Strict booking method, but disambiguate further with sizes. Reject
    # ambiguous matches with an error but if a lot matches the size exactly,
    # accept it the oldest.
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


class Amount:
    number: Optional[Decimal]
    currency: str

    def __init__(self, number: Optional[str | int | float], currency: str) -> None: ...


class Price:
    """
    A price declaration directive. This establishes the price of a currency in
    terms of another currency as of the directive's date. A history of the prices
    for each currency pairs is built and can be queried within the bookkeeping
    system. Note that because Beancount does not store any data at time-of-day
    resolution, it makes no sense to have multiple price directives at the same
    date. (Beancount will not attempt to solve this problem; this is beyond the
    general scope of double-entry bookkeeping and if you need to build a day
    trading system, you should probably use something else).

    Attributes:
      meta: See above.
      date: See above.
     currency: A string, the currency that is being priced, e.g. HOOL.
     amount: An instance of Amount, the number of units and currency that
       'currency' is worth, for instance 1200.12 USD.
    """

    meta: Meta
    date: datetime.date
    currency: Currency
    amount: Amount

    def __init__(
            self,
            meta: Meta,
            date: datetime.date,
            currency: Currency,
            amount: Amount,
    ): ...


class Cost:
    number: Decimal
    currency: str
    date: datetime.date
    label: Optional[str]

    def __init__(
            self,
            number: Decimal,
            currency: str,
            date: datetime.date,
            label: Optional[str],
    ): ...


class Pad:
    """
    A "pad this account with this other account" directive. This directive
    automatically inserts transactions that will make the next chronological
    balance directive succeeds. It can be used to fill in missing date ranges of
    transactions, as a convenience. You don't have to use this, it's sugar coating
    in case you need it, while you're entering past history into your Ledger.

    Attributes:
      meta: See above.
      date: See above.
      account: A string, the name of the account which needs to be filled.
      source_account: A string, the name of the account which is used to debit from
        in order to fill 'account'.
    """

    meta: Meta
    date: datetime.date
    account: Account
    source_account: Account

    def __init__(
            self,
            meta: Meta,
            date: datetime.date,
            account: Account,
            source_account: Account,
    ): ...


class PostingPrice:
    unit: Amount
    total: Amount


class Posting:
    flag: int
    account: str
    amount: Amount
    cost: Cost
    price: PostingPrice
    meta: Meta

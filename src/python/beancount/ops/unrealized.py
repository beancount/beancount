"""Compute unrealized gains.
"""
import collections

from beancount.core.amount import Decimal, ZERO
from beancount.core import data
from beancount.core import account
from beancount.core import getters
from beancount.core.account_types import is_valid_account_name
from beancount.core.data import Transaction, Posting, FileLocation
from beancount.core.position import Lot, Position
from beancount.core import flags
from beancount.ops import holdings
from beancount.ops import prices
from beancount.parser import options


def add_unrealized_gains(entries, options_map):
    """Insert entries for unrealized capital gains.

    This function inserts entries that represent unrealized gains, at the end of
    the available history. It returns a new list of entries, with the new gains
    inserted. It replaces the account type with an entry in an income account.
    Optionally, it can book the gain in a subaccount of the original and income
    accounts.

    Args:
      entries: A list of data directives.
      options_map: A dict of options, that confirms to beancount.parser.options.
    Returns:
      A list of entries, which includes the new unrealized capital gains entries
      at the end, and a list of errors. The new list of entries is still sorted.
    Raises:
      ValueError: If the subaccount name is not a valid account name component.
    """
    errors = []

    account_types = options.get_account_types(options_map)

    # An optional string, the name of a subaccount to create under an account to
    # book the unrealized gain. If this is left to its default value, the gain
    # is booked directly in the same account.
    #
    # FIXME: This should not be a global option. Find a way to pass options in.
    subaccount_name = options_map['account_unrealized']

    # Assert the subaccount name is in valid format.
    if subaccount_name:
        validation_account = account.join(account_types.assets, subaccount_name)
        if not is_valid_account_name(validation_account):
            raise ValueError("Invalid subaccount name: '{}'".format(subaccount_name))

    if not entries:
        return (entries, errors)

    # Group positions by (account, cost, cost_currency).
    # FIXME: Make this use groupby.
    account_holdings = collections.defaultdict(list)
    for holding in holdings.get_final_holdings(entries):
        if not holding.cost_currency:
            continue
        key = (holding.account,
               holding.currency,
               holding.cost_currency)
        account_holdings[key].append(holding)

    # Get the latest prices from the entries.
    price_map = prices.build_price_map(entries)

    # Create transactions to account for each position.
    new_entries = []
    latest_date = entries[-1].date
    for (account_name,
         currency, cost_currency), holdings_list in account_holdings.items():

        # Get the price of this currency/cost pair.
        price_date, price_number = prices.get_price(price_map,
                                                    (currency, cost_currency),
                                                    latest_date)

        # Note: since we're only considering positions held at cost, the
        # transaction that created the position *must* have created at least one
        # price point for that commodity, so we never expect for a price not to
        # be available, which is reasoable.
        assert price_number is not None, (currency, cost_currency, latest_date)

        # Compute the total number of units and book value for set of positions.
        total_units = Decimal()
        market_value = Decimal()
        book_value = Decimal()
        for holding in holdings_list:
            units = holding.number
            total_units += units
            market_value += units * price_number
            book_value += units * holding.cost_number

        # Compute the PnL; if there is no profit or loss, we create a
        # corresponding entry anyway.
        pnl = market_value - book_value
        average_cost = book_value / total_units

        # Compute the name of the accounts and add the requested subaccount name
        # if requested.
        asset_account = account_name
        income_account = account.join(account_types.income,
                                      account.account_name_sans_root(account_name))
        if subaccount_name:
            asset_account = account.join(asset_account, subaccount_name)
            income_account = account.join(income_account, subaccount_name)

        # Create a new transaction to account for this difference in gain.
        fileloc = FileLocation('<unrealized_gains>', 0)
        gain_loss_str = "gain" if pnl > ZERO else "loss"
        narration = ("Unrealized {} for {} units of {} "
                     "(price: {:.4f} {} as of {}, "
                     "average cost: {:.4f} {})").format(
                         gain_loss_str, total_units, currency,
                         price_number, cost_currency, price_date,
                         average_cost, cost_currency)
        entry = Transaction(fileloc, latest_date, flags.FLAG_UNREALIZED,
                            None, narration, None, None, [])

        # Book this as income, converting the account name to be the same, but as income.
        # Note: this is a rather convenient but arbitraty choice--maybe it would be best to
        # let the user decide to what account to book it, but I don't a nice way to let the
        # user specify this.
        #
        # Note: we never set a price because we don't want these to end up in Conversions.
        entry.postings.extend([
            Posting(entry, asset_account,
                    Position(Lot(cost_currency, None, None), pnl),
                    None,
                    None),
            Posting(entry, income_account,
                    Position(Lot(cost_currency, None, None), -pnl),
                    None,
                    None)
        ])

        new_entries.append(entry)

    # Ensure that the accounts we're going to use to book the postings exist, by
    # creating open entries for those that we generated that weren't already
    # existing accounts.
    new_accounts = {posting.account
                    for entry in new_entries
                    for posting in entry.postings}
    open_entries = getters.get_account_open_close(entries)
    new_open_entries = []
    fileloc = FileLocation('<unrealized_gains>', 0)
    for account_ in new_accounts:
        if account_ not in open_entries:
            open_entry = data.Open(fileloc, latest_date, account_, None)
            new_open_entries.append(open_entry)

    return (entries + new_open_entries + new_entries, errors)


def get_unrealized_entries(entries):
    """Return entries automatically created for unrealized gains.

    Args:
      entries: A list of directives.
    Returns:
      A list of directives, all of which are in the original list.
    """
    return [entry
            for entry in entries
            if (isinstance(entry, data.Transaction) and
                entry.flag == flags.FLAG_UNREALIZED)]

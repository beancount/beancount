"""
CSV utils importer.
"""
__copyright__ = "Copyright (C) 2016 Martin Blais, 2018 Michael Droogleever"
__license__ = "GNU GPLv2"

from beancount.core.number import D, ZERO
from beancount.core.amount import Amount

def func_amountnum_dbcr(*, allow_zero_amounts=False, debit_negative=True):
    """
    Get the below function
    Args:
      allow_zero_amounts: Is a transaction with amount D('0.00') okay?
        If not raise Exception.
      debit_negative: Is a debit value a negative amount.
    Returns:
        Get the number from debit and credit rows
        Args:
          iterable 2 strs: debit and credit.
        Returns:
          amount.Decimal, +ve for credit, -ve for debit
    """
    def func(rows):
        debit, credit = rows
        is_zero_amount = ((credit is not None and D(credit) == ZERO) and
                          (debit is not None and D(debit) == ZERO))
        if allow_zero_amounts and is_zero_amount:
            return D(0)
        if debit and debit != "0":
            return -D(debit) if debit_negative else D(debit)
        elif credit and credit != "0":
            return D(credit) if debit_negative else -D(debit)
        # TODO determine utility of allow_zero_amounts option
        raise Exception("csv.allow_zero_amounts is False")
    return func

def func_amount_dbcr(*, allow_zero_amounts=False, debit_negative=True):
    """
    Get the below function
    Args:
      allow_zero_amounts: Is a transaction with amount D('0.00') okay?
        If not raise Exception.
      debit_negative: Is a debit value a negative amount.
    Returns:
        Get the amount.number from debit and credit rows
        Args:
          iterable 3 strs: debit, credit, and currency.
        Returns:
          amount.Amount, -ve for debit, +ve for credit
    """
    num_func = func_amountnum_dbcr(allow_zero_amounts=allow_zero_amounts,
                                   debit_negative=debit_negative)
    def func(rows):
        debit, credit, currency = rows
        num = num_func((debit, credit))
        return Amount(num, currency)
    return func

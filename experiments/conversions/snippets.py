# Snippets from beancount.core.conversions.




    #
    # Convert
    #

    def test_convert__noop(self):
        pos = self._pos(A("514.00 USD"), None)
        self.assertEqual(A("514.00 USD"),
                         conversions.convert(pos, "USD", self.PRICE_MAP_EMPTY))

    def test_convert__direct(self):
        pos = self._pos(A("100 HOOL"), None)
        self.assertEqual(A("53000.00 USD"),
                         conversions.convert(pos, "USD", self.PRICE_MAP_HIT))

    def test_convert__value_noop(self):
        pos = self._pos(A("100 HOOL"), Cost(D("514.00"), "USD", None, None))
        self.assertEqual(A("63600.00 CAD"),
                         conversions.convert(pos, "CAD", self.PRICE_MAP_HIT))






    def test_convert__value_direct(self):
        pos = self._pos(A("100 HOOL"), None, A("520.00 CAD"))
        self.assertEqual(A("63600.00 CAD"),
                         conversions.convert(pos, "CAD", self.PRICE_MAP_HIT))



def convert(pos, target_currency, price_map, date=None):
    """Convert the given Position or Posting to a specific currency.

    Args:
      pos: An instance of Position or Posting, equivalently.
      target_currency: A string, the target currency.
      price_map: A dict of prices, as built by prices.build_price_map().
      date: A datetime.date instance to evaluate the value at, or None.
    Returns:
      An Amount.
    """
    units = pos.units
    cost = pos.cost

    # If there's no need to convert, skip conversion.
    if units.currency == target_currency:
        return units

    # Try the price database first for a direct conversion.
    base_quote = (units.currency, target_currency)
    _, price_number = prices.get_price(price_map, base_quote, date)
    if price_number:
        return Amount(units.number * price_number, target_currency)

    # Otherwise, try the same on its value instead; if there is no change, just
    # return the units.
    value = get_value(pos, price_map, date)
    if value == units:
        return units

    # If the converted value is already in the target currency, return that.
    if value.currency == target_currency:
        return value

    base_quote = (value.currency, target_currency)
    _, price_number = prices.get_price(price_map, base_quote, date)
    if price_number:
        return Amount(value.number * price_number, target_currency)

    # Otherwise... we couldn't do it. Return the units, unconverted.
    return units

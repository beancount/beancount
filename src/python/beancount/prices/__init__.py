"""Fetch prices from the internet and output them as Beancount price directives.

This script accepts a list of Beancount input filenames, and fetches prices
required to compute market values for current positions:

  bean-price /home/joe/finances/joe.beancount

The list of fetching jobs to carry out is derived automatically from the input
file (see section below for full details). It is also possible to provide a list
of specific price fetching jobs to run, e.g.,

  bean-price -e google/TSE:XUS yahoo/AAPL mysources.morningstar/RBF1005

The general format of each of these "source strings" is

  <module>/[^]<ticker>

The "module" is the name of a Python module that contains a Source class which
can be instantiated and connect to a data source to extract price data. These
modules are automatically imported by name and instantiated in order to pull the
price from a particular data source. This allows you to write your own
supplementary fetcher codes without having to modify this script.

Default implementations are provided to provide access to prices from Yahoo!
Finance or Google Finance, which cover a large universe of common public
investment types (e.g. stock tickers). As a convenience, the module name is
always first searched under the "beancount.prices.sources" package, where those
default source implementations live. This is how, for example, in order to use
the provided Google Finance data fetcher you don't have to write
"beancount.prices.sources.yahoo/AAPL" but simply "yahoo/AAPL".

Date
----

By default, this script will fetch prices at the latest available date & time.
You can use an option to fetch historical prices for a desired date instead:

  bean-price --date=2015-02-03

Inverse
-------

Sometimes, prices are available for the inverse of an instrument. This is often
the case for currencies. For example, the price of "CAD" in USD" is provided by
the USD/CAD market, which gives the price of a US dollar in Canadian dollars. In
order specify this, you can prepend "^" to the instrument to instruct the driver
to compute the inverse of the given price:

  bean-price -e USD:google/^CURRENCY:USDCAD

If a source price is to be inverted, like this, the precision could be different
than what is fetched. For instance, if the price of USD/CAD is 1.32759, it would
output be this from the above directive:

  2015-10-28 price CAD  0.753244601119 USD

By default, inverted rates will be rounded similarly to how other Price
directives were rounding those numbers.


Swap Inverted
-------------

If you prefer to have the output Price entries with swapped currencies instead
of inverting the rate itself, you can use the --swap-inverted option. In the
previous example for the price of CAD, it would output this:

  2015-10-28 price USD   1.32759 CAD

This works since the Beancount price database computes and interpolates the
reciprocals automatically for all pairs of commodities in its database.


Prices Needed for a Beancount File
----------------------------------

You can also provide a filename to extract the list of tickers to fetch from a
Beancount input file, e.g.:

  bean-price /home/joe/finances/joe.beancount

There are many ways to extract a list of commodities with needed prices from a
Baancount input file:

- Prices for all the holdings that were seen held-at-cost at a particular date.

- Prices for holdings held at a particular date which were price converted from
  some other commodity in the past (i.e., for currencies).

- The list of all Commodity directives present in the file. For each of those
  holdings, the corresponding Commodity directive is consulted and its "ticker"
  metadata field is used to specify where to attempt to fetch prices. You should
  have directives like this in your input file:

    2007-07-20 commodity VEA
      price: "google/NYSEARCA:VEA"

  The "price" metadata can be a comma-separated list of sources to try out, in
  which case each of the sources will be looked at :

    2007-07-20 commodity VEA
      price: "google/CURRENCY:USDCAD,yahoo/USDCAD"

- Existing price directives for the same data are excluded by default, since the
  price is already in the file.

By default, the list of tickers to be fetched includes only the intersection of
these lists. The general intent of the user of this script is to fetch missing
prices, and only needed ones, for a particular date.

* Use the --date option to change the applied date.
* Use the --all option to fetch the entire set of prices, regardless
  of holdings and date.
* Use --clobber to ignore existing price directives.

You can also print the list of prices to be fetched with the --dry-run option,
which stops short of actually fetching the missing prices (it just prints the
list of fetches it would otherwise attempt).

Caching
-------

Prices are automatically cached. You can disable the cache with an option:

  bean-price --no-cache

You can also instruct the script to clear the cache before fetching its prices:

  bean-price --clear-cache

About Sources and Data Availability
-----------------------------------

IMPORTANT: Note that each source may support a different routine for getting its
latest data and for fetching historical/dated data, and that each of these may
differ in their support. For example, Google Finance does not support fetching
historical data for its CURRENCY:* instruments.

"""
__copyright__ = "Copyright (C) 2015-2017  Martin Blais"
__license__ = "GNU GPLv2"

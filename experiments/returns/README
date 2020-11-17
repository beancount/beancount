# Compute Returns

This directory contains code which computes investment returns on a variety of
assets, as recorded by Beancount, fed directly from a Beancount file.

See this document for details:
https://docs.google.com/document/d/1nPsMIunLnDvdsg6TSsd0PZb7jngojNpFlqnaX36WRp8/

## Scripts

There are three related scripts:

- configure.py: This attempts to automatically infer and generate configuration
  to compute returns from an existing Beancount ledger.

- compute_returns.py: This extracts data for each of the investments defined in
  the configuration and computes the returns and generates output for each
  requested returns report.

- download_prices.py: The compute_returns.py script outputs a list of missing
  (or inadequately dated) price directives to properly do its job as a
  side-product. This script can read that file and fetch those missing dates,
  which you can insert in your ledger and then rerun compute_returns.py for a
  more precise calculation.

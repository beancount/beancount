#!/usr/bin/env python3
""" """

__copyright__ = "Copyright (C) 2018, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import shutil
from os import path

# Maintained by hand.
FILE_PAIRS = [
    (
        "1e4Vz3wZB_8-ZcAwIFde8X5CjzKshE4-OXtVVHm4RQ8s/Beancount-Motivation.rst",
        "users/cl_accounting.rst",
    ),
    (
        "1FqyrTPwiHVLyncWTf3v5TcooCu9z5JRX8Nm41lVZi0U/Beancount-Install.rst",
        "users/installation.rst",
    ),
    (
        "100tGcA4blh6KSXPRGCZpUlyxaRUwFHEvnz_k9DyZFn4/Beancount-The_Double-Entry_Counting_Method.rst",
        "users/double_entry_method.rst",
    ),
    (
        "1e44jtLyVRl2H2Pj4K3WUc66otAlTOFOc90-tsrFEQdo/Beancount-Running_Reports.rst",
        "users/running_and_reports.rst",
    ),
    (
        "1P5At-z1sP8rgwYLHso5sEy3u4rMnIUDDgob9Y_BYuWE/Beancount-Getting_Started.rst",
        "users/getting_started.rst",
    ),
    (
        "1wAMVrKIA2qtRGmoVDSUBJGmYZSygUaR0uOMW1GV3YE0/Beancount-Language_Syntax.rst",
        "users/language.rst",
    ),
    # .. options,                                                                                     "users/options.rst"
    (
        "1lgHxUUEY-UVEgoF6cupz2f_7v7vEF7fiJyiSlYYlhOo/Beancount-Precision_Tolerances.rst",
        "users/precision_tolerances.rst",
    ),
    (
        "1s0GOZMcrKKCLlP29MD7kHO4L88evrwWdIO0p4EwRBE0/Beancount-Query_Language.rst",
        "users/bql.rst",
    ),
    (
        "1M4GwF6BkcXyVVvj4yXBJMX7YFXpxlxo95W6CpU3uWVc/Beancount-Syntax_Cheat_Sheet.rst",
        "users/cheat_sheet.rst",
    ),
    (
        "11a9bIoNuxpSOth3fmfuIFzlZtpTJbvw-bPaQCnezQJs/Beancount-How_Inventories_Work.rst",
        "users/inventories.rst",
    ),
    (
        "1mNyE_ONuyEkF_I2l6V_AoAU5HJgI654AOBhHsnNPPqw/Beancount-Exporting_your_Portfolio_New_.rst",
        "users/exporting.rst",
    ),
    (
        "1G-gsmwK551lSyuHboVLW3xbLhh99JfoKIbNnZSJxteE/Beancount-Tutorial_Example.rst",
        "users/tutorial.rst",
    ),
    (
        "1thYRAMell_QT1Da1F_laprSs6BlROZjyK_h3V8qHW9c/Beancount-Price_in_Beancount.rst",
        "users/fetching_prices.rst",
    ),
    (
        "1Tss0IEzEyAPuKSGeNsfNgb0BfiW2ZHyP5nCFBW1uWlk/Beancount-Cookbook.rst",
        "cookbook/cl_cookbook.rst",
    ),
    (
        "1WjARst_cSxNE-Lq6JnJ5CC41T3WndEsiMw4d46r2694/Beancount-Trading_with_Beancount.rst",
        "cookbook/trading.rst",
    ),
    (
        "1mHNlNMTZsKPMjP_qQmedoizZFQy1-GzlR2lX5zy_0ok/Beancount-Cookbook-Vesting.rst",
        "cookbook/stock_vesting.rst",
    ),
    (
        "1FRcJqUfeAMQO6KjG94w6rF7VajMGJaFplmF1Wu0rCHY/Beancount-Cookbook-Sharing_Expenses.rst",
        "cookbook/sharing_expenses.rst",
    ),
    (
        "1QftxNvQPdH-MikMBHupftU6F4IsNZP5FlFh1LCbVgk8/Beancount-Scripting_Plugins.rst",
        "developers/scripting_plugins.rst",
    ),
    (
        "11u1sWv7H7Ykbc7ayS4M9V3yKqcuTY7LJ3n1tgnEN2Hk/LedgerHub-Design_Doc.rst",
        "developers/design.rst",
    ),
    # .. source code,                                                                                 "developers/source_code.rst"
    (
        "1Z37bQ45wDtjTPaMQ_x-f33p1trH9fNosEAUgbQXwp30/Beancount-Contributions.rst",
        "developers/external_contributions.rst",
    ),
]

"""
"11EwQdujzEo2cxqaF5PgxCEZXWfKKQCYSMfdJowp_1S8/Beancount-Importing_External_Data.rst"
"1dW2vIjaXVJAf9hr7GlZVe3fJOkM-MtlVjvCO1ZpNLmg/Beancount-Comparison_Differences.rst"
"1x0qqWGRHi02ef-FtUW172SHkdJ8quOZD-Xli7r4Nl_k/Beancount-Proposal-Settlement_Dates.rst"
"1MY2JMiiXUmcwsOT0CkiK-fCo0ZE7nbr8uTcTL50b6X4/Beancount-Proposal-Rounding_Precision.rst"
"1vyemZFox47IZjuBrT2RjhSHZyTgloYOUeJb73RxMRD0/Beancount-Proposal-Balance_Assertions.rst"
"17wTH7aKnN_7-6nCxsOad6zIQfHwQgJUdI02RjzQuNi8/Beancount-History_and_Credits.rst"
"1N7HDXuNWgLG2PqFS4Kkgv5LzAAtU6c97UVNT7tdTIjA/Beancount-Design_Doc.rst"
"1F8IJ_7fMHZ75XFPocMokLxVZczAhrBRBVN9uMhQFCZ4/Beancount-Proposal-Inventory_Booking_Improvements.rst"
"1nf_yCiLuewVCEjkXq9Kd9SqbZGWqcs0v0pT5xQnkyzs/Beancount-Proposal-Fund_Accounting.rst"
"""


def main():
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument("convert_docs", help="Root directory of conversion docs")
    parser.add_argument(
        "rst_docs", help="Root directory of Dominik Aumayr's static docs repo"
    )
    args = parser.parse_args()

    for ffrom, fto in FILE_PAIRS:
        afrom = path.join(args.convert_docs, ffrom)
        ato = path.join(args.rst_docs, fto)
        print("Copying {} -> {}".format(afrom, ato))
        shutil.copyfile(afrom, ato)


if __name__ == "__main__":
    main()

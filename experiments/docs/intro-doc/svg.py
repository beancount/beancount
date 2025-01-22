#!/usr/bin/env python3
"""Generate a visual graph of transactions."""

__copyright__ = "Copyright (C) 2016-2017, 2019, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import bisect
import collections
import csv
import random
import re
import textwrap
from os import path

from beancount.core import amount
from beancount.core.number import D


class Params:
    # Flags on whether to draw various pieces of text.
    draw_acctype = True
    draw_account = True
    draw_balance = True

    draw_before = True
    draw_clearing = False
    draw_opening = False
    draw_period = True
    draw_close = False
    draw_after = True

    # Min/max number of postings.
    min_postings, max_postings = 2, 6

    # Vertically, we have
    # | above_line line
    y_margin = 5  # Vertical distance between elements.
    y_margin_group = 15  # Vertical distance between groups.
    y_margin_lines = 4  # Vertical gap between lines.
    y_aboveline = 10  # Occupied space above the timeline.
    y_interline = y_margin_lines + y_aboveline  # Vertical distance between lines.

    # Sizing of posting and transaction nodes.
    post_radius = int(y_interline * 0.4)  # Radius of posting node
    txn_radius = 16  # Radius of transaction node (match

    x_margin = 5  # Horizontal margin between all components.
    x_arrow_margin = 20  # Margin at the right of the timeline to leave space for an arrow
    x_txn_offset = int(
        post_radius * 2 * 1.2
    )  # Offset of txn node to the right of the posting nodes
    x_section_margin = (
        x_txn_offset + txn_radius + x_margin
    )  # Horizontal margin between sections

    # x_inter_distance = post_radius + x_txn_offset + txn_radius + x_margin # Horizontal distance between transactions.
    x_inter_distance = post_radius * 0.9
    x_clear_distance = 2  # Horizontal distance between clearing transactions

    # Horizontally, we have
    # | acctype account | timeline | balances |
    x_acctype_width = 60  # Width of account type text.
    x_account_width = 150  # Width of account text.
    x_timeline_length = 900  # Length of timeline.

    x_timeline_before = 270  # Length of before zone.
    x_timeline_open = 43  # Length of open/clearing zone.
    x_timeline_period = 250  # Length of period zone.
    x_timeline_close = 45  # Length of closing zone.
    x_timeline_after = 100  # Length of closing zone.
    assert (
        x_timeline_before
        + x_timeline_open
        + x_timeline_period
        + x_timeline_close
        + x_timeline_after
    ) < x_timeline_length

    x_balance_width = 110  # Width of balances rendering.

    x_timeline_start = x_margin + x_acctype_width + x_account_width + x_margin
    x_timeline_end = x_timeline_start + x_margin + x_timeline_length + x_margin
    x_width = x_timeline_end + x_margin + x_balance_width + x_margin

    # Max number of attempts fitting in a transaction.
    max_attempts = 1024


def groupby(sequence, key):
    groups = collections.defaultdict(list)
    for el in sequence:
        groups[key(el)].append(el)
    return dict(groups)


def main():
    parser = argparse.ArgumentParser(description=__doc__.strip())

    parser.add_argument(
        "-b",
        "--balances-filename",
        action="store",
        default=path.join(path.dirname(__file__), "balances.csv"),
        help="A CSV file with balances in it",
    )

    parser.add_argument("-s", "--seed", action="store", type=int)

    args = parser.parse_args()

    # Seed randomness for consistency.
    if args.seed is None:
        args.seed = random.randint(0, 2**20)
        print("seed = %s" % args.seed)
    random.seed(args.seed)

    # Read the balances.
    with open(args.balances_filename, "r") as balfile:
        balfile.readline()
        balances = sorted(
            [
                (account, balance)
                for (account, balance) in csv.reader(balfile)
                if (
                    not re.search(r"201\d", account)
                    and re.search(r"\bUSD\b", balance)
                    and balance.strip()
                )
            ]
        )

    # balance_groups = [random.sample(balances, random.randint(5, 24)) for _ in range(2)]

    sbalances = [bal for bal in balances if not bal[0].startswith("Equity")]
    random.shuffle(sbalances)

    random.seed(args.seed)
    p = Params()
    p.draw_acctype = False
    p.y_interline += 2.5
    draw_diagram(p, [sbalances], "svg3.html")

    random.seed(args.seed)
    p.draw_acctype = True
    draw_diagram(p, [sbalances], "svg4.html")

    random.seed(args.seed)
    p.draw_acctype = False
    p.x_timeline_end /= 2
    p.x_width = p.x_timeline_end + p.x_margin + p.x_balance_width + p.x_margin
    p.draw_after = False
    p.draw_period = False
    draw_diagram(p, [sbalances[0 : len(sbalances) // 2]], "svg2.html")

    random.seed(args.seed)
    p.x_timeline_end *= 0.7
    p.x_width = p.x_timeline_end + p.x_margin + p.x_balance_width + p.x_margin
    p.x_timeline_before = 100
    draw_diagram(p, [sbalances[0 : len(sbalances) // 3]], "svg1.html")

    p = Params()
    random.seed(args.seed + 1)
    group_dict = groupby(balances, lambda item: item[0].split(":", 1)[0])
    group_dict["Equity"].reverse()
    groups = [group_dict[x] for x in "Assets Liabilities Equity Income Expenses".split()]
    draw_diagram(p, groups, "svg5.html")

    random.seed(args.seed + 1)
    p.draw_clearing = True
    draw_diagram(p, groups, "svg6.html", scale_income=D("0.3"))

    random.seed(args.seed + 1)
    p.draw_opening = True
    draw_diagram(p, groups, "svg7.html")

    random.seed(args.seed + 1)
    p.draw_before = False
    draw_diagram(p, groups, "svg8.html")

    random.seed(args.seed + 1)
    p.draw_after = False
    draw_diagram(p, groups, "svg9.html")

    random.seed(args.seed + 1)
    p.draw_close = True
    draw_diagram(p, groups, "svgA.html")

    # random.seed(args.seed+1)
    # p.draw_close = True
    # draw_diagram(p, groups, 'svg6.html')

    # random.seed(args.seed+1)
    # p.draw_after = False
    # draw_diagram(p, groups, 'svg7.html')

    # random.seed(args.seed+1)
    # p.draw_before = False
    # draw_diagram(p, groups, 'svg8.html')

    # random.seed(args.seed+1)
    # p.draw_before = False
    # p.draw_after = True
    # draw_diagram(p, groups, 'svg9.html')

    # random.seed(args.seed+1)
    # p.draw_before = False
    # p.draw_after = True
    # p.draw_close = False
    # draw_diagram(p, groups, 'svg10.html')

    # random.seed(args.seed+1)
    # p.draw_before = False
    # p.draw_after = False
    # p.draw_close = False
    # draw_diagram(p, groups, 'svg11.html')


def draw_diagram(p, balance_groups, filename, scale_income=None):
    y_height = (
        p.y_margin
        + sum(map(len, balance_groups)) * p.y_interline
        + len(balance_groups) * p.y_margin_group
    )  # Total height of the entire thing
    with open(filename, "w") as outfile:
        pr = lambda *args: print(*args, file=outfile)
        pr_null = lambda *args: print(*args, file=open("/dev/null", "w"))

        pr("<html>")
        pr("<head>")
        pr('<style type="text/css">')
        pr(
            textwrap.dedent("""\

          /* margin: 0px; */
          /* padding: 0px; */

          /* Defaults */
          stroke-width: 2px;
          stroke: #000;

        /* div#top-level-svg { } */
        /* svg { border: thin solid blue; }*/

        """)
        )
        pr("</style>")
        pr("</head>")
        pr("<body>")
        pr('<div id="top-level-svg">')

        # Make some definitions.
        pr(
            """
          <svg width="{width}px" height="{height}px" font-size="12px" >
            <defs>
              <marker id="arrow" markerWidth="10" markerHeight="6" refX="0" refY="2" orient="auto" markerUnits="strokeWidth">
                <path d="M0,0 L0,4 L9,2 Z" fill="#000" />
              </marker>

              <g id="txn" transform="translate(-{r2},-{r2})">
                <rect x="0" y="0" width="{r}" height="{r}" fill="#BBB" />
                <text x="4" y="9" font-family="Courier" font-weight="bold" font-size="13px" alignment-baseline="central" >
                  T
                </text>
              </g>
            </defs>
        """.format(width=p.x_width, height=y_height, r=p.txn_radius, r2=p.txn_radius / 2)
        )

        # pr('<g transform="scale(1.0)">')
        y = 0
        all_lines = []
        all_line_pairs = []
        for balance_group in balance_groups:
            # if p.draw_clearing:
            #     balance_group = [(account, ('0.00 USD'
            #                                 if re.match('Income|Expenses', account)
            #                                 else balance))
            #                      for account, balance in balance_group]

            if not p.draw_close:
                balance_group = [
                    (account, b)
                    for (account, b) in balance_group
                    if account.strip() != "Equity:Earnings:Current"
                ]

            y_lines = draw_type(p, y + p.y_margin_group, balance_group, pr, scale_income)
            y = y_lines[-1]

            assert len(balance_group) == len(y_lines)
            all_line_pairs.extend(zip(balance_group, y_lines))

            # Skip rendering postings on the Equity lines.
            if balance_group[0][0].startswith("Equity"):
                continue
            all_lines.extend(y_lines)

        # Create and render all transactions.
        boxes = []
        x = p.x_timeline_start + p.x_section_margin
        x_end = x + p.x_timeline_before

        # Before zone.
        if p.draw_before:
            pr('<g style="stroke: #000">')
            create_txns(p, x, x_end, all_lines, boxes, pr)
            pr("</g>")
        else:
            # To maintain random.
            create_txns(p, x, x_end, all_lines, boxes, pr_null)

        # Clearing zone.
        if p.draw_clearing or p.draw_opening:
            x = x_end + p.x_section_margin
            x_end = x + p.x_timeline_open
            if p.draw_clearing:
                pr('<g style="stroke: #040">')
                draw_summarizing_txns(
                    p, x, "Equity:Earnings:Previous", "Income|Expenses", all_line_pairs, pr
                )
                pr("</g>")
            if p.draw_opening:
                pr('<g style="stroke: #006">')
                draw_summarizing_txns(
                    p,
                    x,
                    "Equity:Opening-Balances",
                    "Assets|Liabilities",
                    list(reversed(all_line_pairs)),
                    pr,
                )
                pr("</g>")

        # Period zone.
        if p.draw_period:
            x = x_end + p.x_section_margin
            x_end = x + p.x_timeline_period
            pr('<g style="stroke: #000">')
            create_txns(p, x, x_end, all_lines, boxes, pr)
            pr("</g>")

        # Close zone.
        if p.draw_close:
            x = x_end + p.x_section_margin
            x_end = x + p.x_timeline_close
            pr('<g style="stroke: #040">')
            draw_summarizing_txns(
                p, x, "Equity:Earnings:Current", "Income|Expenses", all_line_pairs, pr
            )
            pr("</g>")

        # After zone.
        if p.draw_after:
            x = x_end + p.x_section_margin
            x_end = x + p.x_timeline_after
            pr('<g style="stroke: #000">')
            create_txns(p, x, x_end, all_lines, boxes, pr)
            pr("</g>")

        # pr('</g>')
        pr("</svg>")
        pr("</div>")
        pr("</body>")
        pr("</html>")


def create_txns(p, x, x_end, all_lines, boxes, pr):
    # Note: Side-effects on 'boxes'.
    while x <= x_end:
        x += p.x_inter_distance
        txn = create_txn(p, x, all_lines, boxes)
        if txn is None:
            continue  # Skip it.
        y_selected, box = txn
        draw_txn(p, x, y_selected, all_lines, pr)
        boxes.append(box)


def draw_summarizing_txns(p, x, equity_account, account_regexp, line_pairs, pr):
    all_y = sorted(p[1] for p in line_pairs)
    y_previous = None
    for (account, _), y in line_pairs:
        if re.match(equity_account, account):
            y_previous = y
    if y_previous:
        for (account, _), y in line_pairs:
            if re.match(account_regexp, account):
                y_selected = sorted([y_previous, y])
                x += p.x_clear_distance
                draw_txn(p, x, y_selected, all_y, pr)


def draw_type(p, y_start, balances, pr, scale_income):
    """Draw a set of lines with the given balances."""
    # Draw the supporting lines.
    y_lines = []
    for iy, (account, balance_amount) in enumerate(balances):
        y = y_start + p.y_interline * (iy + 1)
        y_lines.append(y)

        # Select some balance and draw it at the end of the timeline.
        acctype, account_name = account.strip().split(":", 1)
        acctype += ":"

        if scale_income is not None and acctype in ("Income:", "Expenses:"):
            amt = amount.from_string(balance_amount)
            amt = amount.Amount(
                (amt.number * scale_income).quantize(D("0.00")), amt.currency
            )
            balance_amount = "{:>15}".format(amt.to_string().replace(" ", "  "))

        draw_line(p, y, acctype, account_name, balance_amount, pr)

    return y_lines


def draw_line(p, y, acctype, account_name, balance_amount, pr):
    """Draw a single line with its account name and balance amount."""
    pr("")
    pr(
        '<line x1="{x}" y1="{y}"  x2="{x2}" y2="{y}" stroke="#000" marker-end="url(#arrow)" />'.format(
            x=p.x_timeline_start, x2=p.x_timeline_end, y=y
        )
    )

    if p.draw_acctype:
        pr(
            '<text x="{x}" y="{y}" text-anchor="end" alignment-baseline="central" font-family="Helvetica" font-weight="bold">'.format(
                x=p.x_margin + p.x_acctype_width, y=y
            )
        )
        pr(acctype)
        pr("</text>")

    if p.draw_account:
        pr(
            '<text x="{x}" y="{y}" text-anchor="start" alignment-baseline="central" font-family="Helvetica" font-weight="bold">'.format(
                x=p.x_margin + p.x_acctype_width, y=y
            )
        )
        pr(account_name)
        pr("</text>")

    if p.draw_balance:
        pr(
            '<text x="{x}" y="{y}" text-anchor="end" alignment-baseline="central" font-family="Courier">'.format(
                x=p.x_width - p.x_margin, y=y
            )
        )
        pr(balance_amount)
        pr("</text>")

    pr("")


def iterpairs(seq):
    """Iterate over sequential pairs."""
    seqiter = iter(seq)
    prev = next(seqiter)
    for el in seqiter:
        yield prev, el
        prev = el


def intersects(minmax1, minmax2):
    min1, max1 = minmax1
    min2, max2 = minmax2
    return not (min1 >= max2 or max1 <= min2)


def create_txn(p, x, y_lines, boxes):
    """Create a new transaction, avoiding existing ones."""
    x_minmax = x - p.post_radius, x + p.x_txn_offset + p.txn_radius

    # Filter boxes to the list which might intersect ours on the x axis.
    blocker_boxes = []
    for box in boxes:
        bx_minmax, _ = box
        if intersects(x_minmax, bx_minmax):
            blocker_boxes.append(box)

    y_selected = None
    i = 0
    while y_selected is None:
        if i >= p.max_attempts:
            return None
        i += 1

        # Select a number of postings.
        num_postings = 0
        while not (p.min_postings <= num_postings <= min(p.max_postings, len(y_lines))):
            num_postings = int(random.lognormvariate(1.0, 0.4))

        # Select some subset of lines. Try to keep the lines together.
        first_line = random.randint(0, len(y_lines) - num_postings)
        lines = [first_line]
        probab_select = 0.3
        line = first_line
        while len(lines) < num_postings:
            for line in range(first_line + 1, len(y_lines)):
                if random.random() < probab_select:
                    lines.append(line)
                if len(lines) >= num_postings:
                    break
        y_selected = [y_lines[line] for line in sorted(lines)]

        y_minmax = y_selected[0] - p.post_radius, y_selected[-1] + p.post_radius
        for box in blocker_boxes:
            _, by_minmax = box
            if intersects(y_minmax, by_minmax):
                y_selected = None
                break

    box = (x_minmax, y_minmax)

    return y_selected, box


def find_txn_location(y_selected, y_lines):
    # Select the y in the largest gap in the y lines.
    maxgap = 0
    maxloc = -1
    for i, (yp, yn) in enumerate(iterpairs(y_selected)):
        gap = yn - yp
        if gap > maxgap:
            maxgap = gap
            maxloc = i
    yt = (y_selected[maxloc + 1] + y_selected[maxloc]) / 2.0

    # Shift the y location away from any existing line.
    i = bisect.bisect_left(y_lines, yt)
    yt = (y_lines[i - 1] + y_lines[i]) / 2

    return yt


def draw_txn(p, x, y_selected, y_lines, pr, debug=False):
    """Draw a single transaction."""

    yt = find_txn_location(y_selected, y_lines)

    xt = x + p.x_txn_offset

    # Draw the joining lines between postings and transactions.
    for y in y_selected:
        pr(
            '<path d="M {} {} C {} {}, {} {}, {} {}" fill="transparent" />'.format(
                x, y, xt, y, xt, (yt + y) / 2, xt, yt
            )
        )

    # Draw the post icons (circles).
    for y in y_selected:
        pr(
            '<circle cx="{x}" cy="{y}" r="{r}" fill="#FFF" />'.format(
                x=x, y=y, r=p.post_radius
            )
        )

    # Draw the transaction icon.
    pr('<use xlink:href="#txn" x="{x}" y="{y}" />'.format(x=xt, y=yt))


if __name__ == "__main__":
    main()

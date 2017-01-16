"""Rename payees based on a set of rules.

This can be used to clean up dirty imported payee names.

This plugin accepts a list of rules in this format:

  plugin "beancount.plugins.fix_payees" "[
      (PAYEE, MATCH1, MATCH2, ...),
  ]"

Each of the "MATCH" clauses is a string, in the format:

  "A:<regexp>" : Match the account name.
  "D:<regexp>" : Match the payee or the narration.

The plugin matches the Transactions in the file and if there is a
case-insensitive match against the regular expression (we use re.search()),
replaces the payee name by "PAYEE". If multiple rules match, only the first rule
is used.

For example:

  plugin "beancount.plugins.fix_payees" "[

      ("T-Mobile USA",
       "A:Expenses:Communications:Phone",
       "D:t-mobile"),

      ("Con Edison",
       "A:Expenses:Home:Electricity",
       "D:con ?ed"),

      ("Birreria @ Eataly",
       "D:EATALY BIRRERIA"),

  ]"

"""
__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

import ast
import collections
import re

from beancount.core import data


__plugins__ = ('fix_payees',)


FixPayeesError = collections.namedtuple('FixPayeesError', 'source message entry')


# Set this to true to dump debug outpout.
_DEBUG = False


def fix_payees(entries, options_map, config):
    """Rename payees based on a set of rules. See module docstring for details.

    Args:
      entries: a list of entry instances
      options_map: a dict of options parsed from the file
      config: A configuration string, which is intended to be a list of
        (PAYEE, MATCH, ...) rules. See module docstring for details.
    Returns:
      A tuple of entries and errors.
    """
    errors = []
    if config.strip():
        try:
            expr = ast.literal_eval(config)
        except SyntaxError:
            meta = data.new_metadata(options_map['filename'], 0)
            errors.append(FixPayeesError(meta,
                                         "Syntax error in config: {}".format(config),
                                         None))
            return entries, errors
    else:
        return entries, errors

    # Pre-compile the regular expressions for performance.
    rules = []
    for rule in ast.literal_eval(config):
        clauses = iter(rule)
        new_payee = next(clauses)
        regexps = []
        for clause in clauses:
            match = re.match('([AD]):(.*)', clause)
            if not match:
                meta = data.new_metadata(options_map['filename'], 0)
                errors.append(FixPayeesError(meta,
                                             "Invalid clause: {}".format(clause),
                                             None))
                continue
            command, regexp = match.groups()
            regexps.append((command, re.compile(regexp, re.I).search))
        new_rule = [new_payee] + regexps
        rules.append(tuple(new_rule))

    # Run the rules over the transaction objects.
    new_entries = []
    replaced_entries = {rule[0]: [] for rule in rules}
    for entry in entries:
        if isinstance(entry, data.Transaction):
            for rule in rules:
                clauses = iter(rule)
                new_payee = next(clauses)

                # Attempt to match all the clauses.
                for clause in clauses:
                    command, func = clause
                    if command == 'D':
                        if not ((entry.payee is not None and func(entry.payee)) or
                                (entry.narration is not None and func(entry.narration))):
                            break
                    elif command == 'A':
                        if not any(func(posting.account) for posting in entry.postings):
                            break
                else:
                    # Make the replacement.
                    entry = entry._replace(payee=new_payee)
                    replaced_entries[new_payee].append(entry)
        new_entries.append(entry)

    if _DEBUG:
        # Print debugging info.
        for payee, repl_entries in sorted(replaced_entries.items(),
                                          key=lambda x: len(x[1]),
                                          reverse=True):
            print('{:60}: {}'.format(payee, len(repl_entries)))

    return new_entries, errors

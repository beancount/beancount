#!/usr/bin/env python3

from beancount import loader
from beancount.core import data


entries, _, options_map = loader.load_file('/Users/blais/r/q/office/accounting/blais.beancount')
for entry in entries:
    if not isinstance(entry, data.Transaction):
        continue
    # if entry.payee is not None:
    #     print(repr(entry.payee).encode('utf8'))
    if entry.narration is not None:
        print(repr(entry.narration).encode('utf8'))
    

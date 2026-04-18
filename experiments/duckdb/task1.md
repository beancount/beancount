I would like to convert a beancount ledger into a flat table of postings in a
duckdb database.

- You can read prior work in @experiments/sql to learn how a conversion of a
  beancount ledger has been made in the past to a sqlite3 database.

- You may also read the beancount source code under beancount/core to learn
  about the schema - in Python - of the data that we want to represent.

- You can use the "examples/example.beancount" file as a good input file for testing.

- You can look at the /home/blais/p/beanquery repository for how a home-made
  query engine has been done in the past. I would like as much as possible to
  match the column names defined in that system.

- I would like you to use a struct for the position's attributes instead of a
  flat table, because we will want to aggregate positions in some future work.

In your suggested design, I would like you to consider everything from scratch.
If anything in the previous work referenced above could be improved, feel free
to make a suggestion. Except for beancount itself, this previous work in
querying is either very old or experimental. My ultimate goal is to replace it
with custom duckdb types.

Make me a nice detailed design proposal and let me review it.

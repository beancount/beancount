#!/usr/bin/env python3
import os
import shutil
import sys

import duckdb
from prompt_toolkit import PromptSession
from prompt_toolkit.history import FileHistory
from prompt_toolkit.lexers import PygmentsLexer
from pygments.lexers.sql import SqlLexer

# Add the current directory to path so we can import from experiments
sys.path.insert(0, os.getcwd())

from experiments.duckdb.duckdb_core import register_duckdb


def main():
    if len(sys.argv) < 2:
        print("Usage: python3 experiments/duckdb/duckdb_cli_local.py <database_file>")
        sys.exit(1)

    db_file = sys.argv[1]

    # Connect to the database
    try:
        con = duckdb.connect(db_file)
    except Exception as e:
        print(f"Failed to connect to {db_file}: {e}")
        sys.exit(1)

    # Register the Beancount UDFs and Macros
    register_duckdb(con)

    # Setup history file
    history_file = os.path.expanduser("~/.duckdb_beancount_history")
    session = PromptSession(
        history=FileHistory(history_file), lexer=PygmentsLexer(SqlLexer)
    )

    print(f"Connected to {db_file}")
    print("Beancount UDFs and Macros registered.")
    print(f"History file: {history_file}")
    print("\nAvailable macros:")
    print("  bnstr(obj)  -> VARCHAR (renders Amount, Position, or Inventory)")
    print("  bnsum(pos)  -> Inventory (aggregate sum of positions)")
    print("  bnj()       -> TABLE (standard journal columns)")
    print("  bnb()       -> TABLE (standard balance report)")
    print("  bnpos(...)  -> Position (constructor)")
    print("  bninv(list) -> Inventory (constructor)")
    print("\nType your SQL queries below. End with ';' to execute.")
    print("Press Ctrl-C or Type 'exit' to quit.")

    while True:
        try:
            # First line
            text = session.prompt("duckdb> ")
            if not text:
                continue
            if text.lower() in ("exit", "quit", ".exit", ".quit"):
                break

            # Basic multi-line support: continue until semicolon
            while not text.strip().endswith(";"):
                more = session.prompt("   ...> ")
                if not more.strip():
                    break
                text += " " + more

            # Execute and show
            rel = con.sql(text)
            if rel:
                rel.show(max_rows=1000, max_width=shutil.get_terminal_size().columns)
        except KeyboardInterrupt:
            # Just clear the current prompt line
            continue
        except EOFError:
            print("\nBye.")
            break
        except Exception as e:
            print(f"Error: {e}")


if __name__ == "__main__":
    main()

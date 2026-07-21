# Experiments Makefile for DuckDB adapter

PYTHON ?= python3
INPUT ?= ../../examples/example.beancount
DB ?= example.duckdb

ADAPTER = duckdb_adapter.py
CLI = duckdb_cli_local.py

.PHONY: all ingest cli clean test

all: ingest

ingest: $(DB)

$(DB): $(INPUT) $(ADAPTER)
	$(PYTHON) $(ADAPTER) $(INPUT) $(DB)

cli: $(DB)
	PYTHONPATH=../../ $(PYTHON) $(CLI) $(DB)

test:
	PYTHONPATH=../../ $(PYTHON) -m pytest duckdb_core_test.py

clean:
	rm -f $(DB)
	rm -rf __pycache__

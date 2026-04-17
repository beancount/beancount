#!/usr/bin/env make

.PHONY: install build serve

install:
	uv sync

build:
	uv run zensical build -f zensical.yml

serve:
	uv run zensical serve -f zensical.yml

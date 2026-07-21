# Beancount Documentation

This directory now contains a self-contained Zensical site scaffold for Beancount.

Use it from inside `beancount/docs` so the main repository root stays clean.

## Local usage

Install dependencies:

```shell
uv sync
```

Serve the site locally:

```shell
make serve
```

Build the static site:

```shell
make build
```

## Notes

- The site configuration is adapted from the standalone `docs` repository.
- Source pages live under `beancount/docs/site_docs/`.
- Built output is written to `beancount/docs/site/`.

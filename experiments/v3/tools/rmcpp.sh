#!/bin/bash
git rm WORKSPACE
find . -name 'BUILD*' -exec git rm {} \;
git rm -r bazel
git rm -r experiments/v3
git rm -r beancount/ccore
git rm -r beancount/cparser
git rm -r requirements
git rm .bazel*
git rm tools/gen_bazel_rule.py
git rm tools/gen_version_header.py

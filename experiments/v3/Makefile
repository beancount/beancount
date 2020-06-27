#!/usr/bin/env make


all: build export convert view

build:
	bazel build -c opt //experiments/...

export:
	time ./bazel-bin/experiments/protos/export_as_protos $(L)

convert:
	time ./bazel-bin/experiments/tools/riegeli_print /tmp/ledger.rieg > /tmp/ledger.text

view:
	less /tmp/ledger.text

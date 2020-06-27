#!/usr/bin/env make

beancount_pb2.py: beancount.proto
	protoc -I . --python_out . --proto_path . $<

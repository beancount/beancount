#!/usr/bin/env make

all: config_pb2.py

config_pb2.py: returns_config.proto
	protoc -I . --python_out . --proto_path . $<

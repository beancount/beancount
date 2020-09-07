#!/usr/bin/env make

all: returns_config_pb2.py

returns_config_pb2.py: returns_config.proto
	protoc -I . --python_out . --proto_path . $<

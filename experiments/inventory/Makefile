#!/usr/bin/env make

assets_pb2.py: assets.proto
	protoc -I . --python_out . --proto_path . $<

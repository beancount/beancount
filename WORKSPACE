"""Bazel workspace for the Beancount."""
workspace(name="beancount")

# Build rules.
local_repository(name = "oblique", path = "/home/blais/p/oblique",)

load("@oblique//third_party/foreign:setup.bzl", "setup_rules_foreign")
setup_rules_foreign()

load("@oblique//third_party/cppbase:setup.bzl", "setup_cppbase")
setup_cppbase()

load("@oblique//third_party/python:setup.bzl", "setup_python")
setup_python()

load("@oblique//third_party/proto:setup.bzl", "setup_proto", "setup_riegeli")
setup_proto()
setup_riegeli()
load("@rules_proto//proto:repositories.bzl", "rules_proto_dependencies", "rules_proto_toolchains")
rules_proto_dependencies()
rules_proto_toolchains()

load("@oblique//third_party/parser:setup.bzl", "setup_parser")
setup_parser()

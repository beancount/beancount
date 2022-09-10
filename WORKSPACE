"""Bazel workspace for the Beancount.

Note that:

- This is using a local Python runtime, which must be 3.10
- The pip package for 'protobuf' version *must* match that which is in use in this build (v21.5)
- Your version of Bazel installed must match that which is used in the github actions (5.3).

"""
workspace(name="beancount")

# Bazel general rules packages
load("//third_party/bazel:repositories.bzl", "setup_repositories")
setup_repositories()
load("//third_party/bazel:deps.bzl", "setup_rules_dependencies")
setup_rules_dependencies()

# Basic C++ environment with Abseil and/oor Boost, unit testing library.
load("//third_party/cppbase:setup.bzl", "setup_cppbase_dependencies")
setup_cppbase_dependencies()

# Support for scanners & parser generators.
load("//third_party/parser:setup.bzl", "setup_parser_dependencies")
setup_parser_dependencies()

# Support for Python, building Python, Python/C++ bindings.
load("//third_party/python:setup.bzl", "setup_python_dependencies")
setup_python_dependencies()

# Support for protocol buffers in all languages.
load("//third_party/proto:setup.bzl", "setup_proto_dependencies")
setup_proto_dependencies()
load("@rules_proto//proto:repositories.bzl", "rules_proto_dependencies", "rules_proto_toolchains")
rules_proto_dependencies()
rules_proto_toolchains()

# TODO(blais): Add _github_archive() utility to all, including conditional.
# TODO(blais): Update RE-flex version and code.
# TODO(blais): Update proto version & move out rules_* dependencies of it.

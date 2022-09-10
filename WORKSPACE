"""Bazel workspace for the Beancount.

Note that:

- This is using a local Python runtime, which must be 3.10
- The pip package for 'protobuf' version *must* match that which is in use in this build (v21.5)
- Your version of Bazel installed must match that which is used in the github actions (5.3).

"""
workspace(name="beancount")

#------------------------------------------------------------------------------
# Beancount direct dependencies.

# Bazel general rules packages
load("//third_party/bazel:repositories.bzl", "setup_bazel_dependencies")
setup_bazel_dependencies()

# Basic C++ environment with Abseil and/oor Boost, unit testing library.
load("//third_party/cppbase:repositories.bzl", "setup_cppbase_dependencies")
setup_cppbase_dependencies()

# Support for scanners & parser generators.
load("//third_party/parser:repositories.bzl", "setup_parser_dependencies")
setup_parser_dependencies()

# Support for Python, building Python, Python/C++ bindings.
load("//third_party/python:repositories.bzl", "setup_python_dependencies")
setup_python_dependencies()

# Support for protocol buffers in all languages.
load("//third_party/proto:repositories.bzl", "setup_proto_dependencies")
setup_proto_dependencies()

#------------------------------------------------------------------------------
# Indirect dependencies from packages.

load("@bazel_skylib//:workspace.bzl", "bazel_skylib_workspace")
bazel_skylib_workspace()

load("@rules_foreign_cc//foreign_cc:repositories.bzl", "rules_foreign_cc_dependencies")
rules_foreign_cc_dependencies()

load("@rules_pkg//:deps.bzl", "rules_pkg_dependencies")
rules_pkg_dependencies()

load("@rules_cc//cc:repositories.bzl", "rules_cc_dependencies")
rules_cc_dependencies()

load("@rules_proto//proto:repositories.bzl", "rules_proto_dependencies", "rules_proto_toolchains")
rules_proto_dependencies()
rules_proto_toolchains()

# TODO(blais): Update RE-flex version and code.
# TODO(blais): Reorder too name/urls (url), strip_prefix, sha256.
# TODO(blais): Join files... there isn't much left. Maybe a single bit fat directory is best?

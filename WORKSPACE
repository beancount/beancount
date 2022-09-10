"""Bazel workspace for the Beancount.

Note that:

- This is using a local Python runtime, which must be 3.10
- The pip package for 'protobuf' version *must* match that which is in use in this build (v21.5)
- Your version of Bazel installed must match that which is used in the github actions (5.3).

"""
workspace(name="beancount")

# Setup for building third-party deps using configure/make/make-install.
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
http_archive(
    name = "rules_foreign_cc",
    # 2022-09-10
    urls = ["https://github.com/bazelbuild/rules_foreign_cc/archive/refs/tags/0.9.0.tar.gz"],
    sha256 = "2a4d07cd64b0719b39a7c12218a3e507672b82a97b98c6a89d38565894cf7c51",
    strip_prefix = "rules_foreign_cc-0.9.0",
)
load("@rules_foreign_cc//foreign_cc:repositories.bzl", "rules_foreign_cc_dependencies")
rules_foreign_cc_dependencies()

# Setup for packaging rules.
http_archive(
    name = "rules_pkg",
    urls = ["https://mirror.bazel.build/github.com/bazelbuild/rules_pkg/releases/download/0.7.1/rules_pkg-0.7.0.tar.gz",
            "https://github.com/bazelbuild/rules_pkg/releases/download/0.7.1/rules_pkg-0.7.1.tar.gz"],
    sha256 = "451e08a4d78988c06fa3f9306ec813b836b1d076d0f055595444ba4ff22b867f",
)
load("@rules_pkg//:deps.bzl", "rules_pkg_dependencies")
rules_pkg_dependencies()

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

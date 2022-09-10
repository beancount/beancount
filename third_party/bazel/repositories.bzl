"""Bazel support for general build rules."""

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("//third_party/bazel:maybe_archive.bzl", "maybe_http_archive")


def setup_bazel_dependencies():
    # Setup for building third-party deps using configure/make/make-install.
    maybe_http_archive(
        name = "bazel_skylib",
        urls = ["https://mirror.bazel.build/github.com/bazelbuild/bazel-skylib/releases/download/1.3.0/bazel-skylib-1.3.0.tar.gz",
                "https://github.com/bazelbuild/bazel-skylib/releases/download/1.3.0/bazel-skylib-1.3.0.tar.gz"],
        sha256 = "74d544d96f4a5bb630d465ca8bbcfe231e3594e5aae57e1edbf17a6eb3ca2506",
    )

    # Setup for building third-party deps using configure/make/make-install.
    maybe_http_archive(
        name = "rules_foreign_cc",
        # 2022-09-10
        urls = ["https://github.com/bazelbuild/rules_foreign_cc/archive/refs/tags/0.9.0.tar.gz"],
        sha256 = "2a4d07cd64b0719b39a7c12218a3e507672b82a97b98c6a89d38565894cf7c51",
        strip_prefix = "rules_foreign_cc-0.9.0",
    )

    # Setup for packaging rules.
    maybe_http_archive(
        name = "rules_pkg",
        urls = ["https://mirror.bazel.build/github.com/bazelbuild/rules_pkg/releases/download/0.7.1/rules_pkg-0.7.0.tar.gz",
                "https://github.com/bazelbuild/rules_pkg/releases/download/0.7.1/rules_pkg-0.7.1.tar.gz"],
        sha256 = "451e08a4d78988c06fa3f9306ec813b836b1d076d0f055595444ba4ff22b867f",
    )

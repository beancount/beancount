""" Define rules_foreign_cc if it is not defined natively. """

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

load("//third_party/foreign:bazel_version.bzl", "bazel_version")

def setup_rules_foreign():
    bazel_version()

    if not native.existing_rule("rules_foreign_cc"):
        # 2020-06-12
        http_archive(
            name = "rules_foreign_cc",
            urls = ["https://github.com/bazelbuild/rules_foreign_cc/archive/refs/tags/0.9.0.tar.gz"],
            sha256 = "2a4d07cd64b0719b39a7c12218a3e507672b82a97b98c6a89d38565894cf7c51",
            strip_prefix = "rules_foreign_cc-0.9.0",
        )

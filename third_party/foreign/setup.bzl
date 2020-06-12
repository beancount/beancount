""" Define rules_foreign_cc if it is not defined natively. """

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

load("//third_party/foreign:bazel_version.bzl", "bazel_version")

def setup_rules_foreign():
    bazel_version()

    if not native.existing_rule("rules_foreign_cc"):
        # 2020-06-12
        http_archive(
            name = "rules_foreign_cc",
            strip_prefix = "rules_foreign_cc-f54b7ae56dcf1b81bcafed3a08d58fc08ac095a7",
            urls = [
                "https://github.com/bazelbuild/rules_foreign_cc/archive/f54b7ae56dcf1b81bcafed3a08d58fc08ac095a7.tar.gz",
            ],
            sha256 = "7ca49ac5b0bc8f5a2c9a7e87b7f86aca604bda197259c9b96f8b7f0a4f38b57b",
        )

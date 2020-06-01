""" Define rules_foreign_cc if it is not defined natively. """

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

load("//third_party/foreign:bazel_version.bzl", "bazel_version")

def setup_rules_foreign():
    bazel_version()

    if not native.existing_rule("rules_foreign_cc"):
        http_archive(
            name = "rules_foreign_cc",
            strip_prefix = "rules_foreign_cc-ed3db61a55c13da311d875460938c42ee8bbc2a5",
            urls = [
                "https://github.com/bazelbuild/rules_foreign_cc/archive/ed3db61a55c13da311d875460938c42ee8bbc2a5.tar.gz",
            ],
            sha256 = "219bc7280bbb9305938d76067c816954ad2cc0629063412e8b765e9bc6972304",
        )

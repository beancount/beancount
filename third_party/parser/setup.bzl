"""Lex, flex, reflex, yacc, Bison, scanners & parser generators."""

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")


def setup_parser():
    if not native.existing_rule("m4"):
        http_archive(
            name = "rules_m4",
            urls = ["https://github.com/jmillikin/rules_m4/releases/download/v0.2/rules_m4-v0.2.tar.xz"],
            sha256 = "c67fa9891bb19e9e6c1050003ba648d35383b8cb3c9572f397ad24040fb7f0eb",
        )

    # http_archive(
    #     name = "flex",
    #     build_file_content = all_content,
    #     strip_prefix = "flex-2.6.4",
    #     sha256 = "e87aae032bf07c26f85ac0ed3250998c37621d95f8bd748b31f15b33c45ee995",
    #     urls = ["https://github.com/westes/flex/releases/download/v2.6.4/flex-2.6.4.tar.gz"],
    # )

    # Reflex.
    # Create a target of the headers, because generated code needs to include
    # and links against a reflex library.
    all_content = """
filegroup(
  name = "all",
  srcs = glob(["**"]),
  visibility = ["//visibility:public"]
)
    """
    headers_content = """
filegroup(
  name = "headers",
  srcs = glob(["include/reflex/*.h"]),
  visibility = ["//visibility:public"]
)
    """
    http_archive(
        name = "reflex",
        build_file_content = all_content + headers_content,
        strip_prefix = "RE-flex-1.6.6",
        sha256 = "f044631311fdabd71d9c6303b8f9edc7052289c099c7c47a1a42f5d9e8ad4905",
        urls = ["https://github.com/Genivia/RE-flex/archive/v1.6.6.zip"],
    )

    # 2020-05-09
    http_archive(
        name = "bison",
        build_file_content = all_content,
        strip_prefix = "bison-3.6.1",
        sha256 = "1120f8bfe2cc13e5e1e3f671dc41b1a535ca5a75a70d5b349c19da9d4389f74d",
        urls = ["https://ftp.gnu.org/gnu/bison/bison-3.6.1.tar.gz"],
    )

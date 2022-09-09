"""Lex, flex, reflex, yacc, Bison, scanners & parser generators.

Note that we specifically avoid rules_m4 and rules_bison and instead build our
own local versions of these binaries, for isolation. See the
`google/zetasql/bazel` configuration for an example.
"""

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")


def setup_parser():
    setup_m4()
    setup_bison()
    setup_reflex()


all_content = """
filegroup(name = "all", srcs = glob(["**"]), visibility = ["//visibility:public"])
"""


def setup_m4():
    if not native.existing_rule("m4"):
        http_archive(
            name = "m4",
            build_file_content = all_content,
            # 2022-09-08
            strip_prefix = "m4-1.4.19",
            sha256 = "3be4a26d825ffdfda52a56fc43246456989a3630093cced3fbddf4771ee58a70",
            urls = [
                "https://ftp.gnu.org/gnu/m4/m4-1.4.19.tar.gz",
                "https://mirrors.kernel.org/gnu/m4/m4-1.4.19.tar.gz",
            ],
        )


bison_build_file_content = all_content + """
filegroup(
    name = "bison_runtime_data",
    srcs = glob(["data/**/*"]),
    output_licenses = ["unencumbered"],
    path = "data",
    visibility = ["//visibility:public"],

)
exports_files(["data"])
"""


def setup_bison():
    if not native.existing_rule("bison"):
        http_archive(
            name = "bison",
            build_file_content = bison_build_file_content,
            # 2022-09-08
            strip_prefix = "bison-3.8.2",
            sha256 = "06c9e13bdf7eb24d4ceb6b59205a4f67c2c7e7213119644430fe82fbd14a0abb",
            urls = ["https://ftp.gnu.org/gnu/bison/bison-3.8.2.tar.gz"],
        )


reflex_build_file_content = all_content + """
filegroup(
  name = "headers",
  srcs = glob(["include/reflex/*.h"]),
  visibility = ["//visibility:public"]
)
"""


def setup_reflex():
    if not native.existing_rule("reflex"):
        # Reflex.
        # Create a target of the headers, because generated code needs to include
        # and links against a reflex library.
        http_archive(
            name = "reflex",
            build_file_content = reflex_build_file_content,
            strip_prefix = "RE-flex-3.0.1",
            sha256 = "f07188377bb8dfde54c6b19f219c1c60d43d501f5458936c686bd29d684cce19",
            urls = ["https://github.com/Genivia/RE-flex/archive/v3.0.1.zip"],
        )
        # This newer version currently fails the syntax, we have to fix it.
        # TODO(blais): Fix the scanner error and upgrade.
        # http_archive(
        #     name = "reflex",
        #     build_file_content = all_content + headers_content,
        #     # 2022-09-08
        #     strip_prefix = "RE-flex-3.2.11",
        #     sha256 = "2d61eaefec94836d4807aa61df5f946b183699db77c4c384db9843a83c33c63b",
        #     urls = ["https://github.com/Genivia/RE-flex/archive/v3.2.11.zip"],
        # )

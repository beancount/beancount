"""Lex, flex, reflex, yacc, Bison, scanners & parser generators.

Note that we specifically avoid rules_m4 and rules_bison and instead build our
own local versions of these binaries, for isolation. See the
`google/zetasql/bazel` configuration for an example.
"""

load("//third_party/build:maybe_archive.bzl", "maybe_http_archive")


def beancount_parser_dependencies():
    maybe_http_archive(
        name = "m4",
        # 2022-09-08
        urls = ["https://ftp.gnu.org/gnu/m4/m4-1.4.19.tar.gz",
                "https://mirrors.kernel.org/gnu/m4/m4-1.4.19.tar.gz"],
        strip_prefix = "m4-1.4.19",
        sha256 = "3be4a26d825ffdfda52a56fc43246456989a3630093cced3fbddf4771ee58a70",
        build_file = "//third_party/parser:m4.BUILD",
    )

    maybe_http_archive(
        name = "bison",
        # 2022-09-08
        url = "https://ftp.gnu.org/gnu/bison/bison-3.8.2.tar.gz",
        strip_prefix = "bison-3.8.2",
        sha256 = "06c9e13bdf7eb24d4ceb6b59205a4f67c2c7e7213119644430fe82fbd14a0abb",
        build_file = "//third_party/parser:bison.BUILD",
    )

    # Reflex.
    # Create a target of the headers, because generated code needs to include
    # and links against a reflex library.
    maybe_http_archive(
        name = "reflex",
        url = "https://github.com/Genivia/RE-flex/archive/v3.0.1.zip",
        strip_prefix = "RE-flex-3.0.1",
        sha256 = "f07188377bb8dfde54c6b19f219c1c60d43d501f5458936c686bd29d684cce19",
        build_file = "//third_party/parser:reflex.BUILD",
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

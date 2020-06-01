"""Base C++ environment with Abseil or Boost and unit testing library."""

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@rules_foreign_cc//:workspace_definitions.bzl", "rules_foreign_cc_dependencies")


def setup_cppbase():
    setup_absl()
    setup_googletest()


def setup_absl():
    rules_foreign_cc_dependencies()

    # Abseil
    if not native.existing_rule("com_google_absl"):
        # How to update:
        # Abseil generally just does daily (or even subdaily) releases. None are
        # special, so just periodically update as necessary.
        #
        #  https://github.com/abseil/abseil-cpp/commits/master
        #  pick a recent release.
        #  Hit the 'clipboard with a left arrow' icon to copy the commit hex
        #
        # 2020-05-08
        http_archive(
            name = "com_google_absl",
            url = "https://github.com/abseil/abseil-cpp/archive/c45d1c09d517e145d722e00deea9be6c8be8dd57.tar.gz",
            sha256 = "29b3781679d58af5f26d5a889b9710ade67b7b259f198d3a5025ca7b014b5789",
            strip_prefix = "abseil-cpp-c45d1c09d517e145d722e00deea9be6c8be8dd57",
        )


def setup_googletest():
    # GoogleTest/GoogleMock framework. Used by most unit-tests.
    if not native.existing_rule("com_google_googletest"):
        # How to update:
        # Googletest generally just does daily (or even subdaily) releases along
        # with occasional numbered releases.
        #
        #  https://github.com/google/googletest/commits/master
        #  pick a recent release.
        #  Hit the 'clipboard with a left arrow' icon to copy the commit hex
        # 2020-05-05
        http_archive(
            name = "com_google_googletest",
            url = "https://github.com/google/googletest/archive/0eea2e9fc63461761dea5f2f517bd6af2ca024fa.tar.gz",
            sha256 = "552f1b59b8332195ae257b01182101dc93a3a16c3f808e763af9a1c3e4b1756a",
            strip_prefix = "googletest-0eea2e9fc63461761dea5f2f517bd6af2ca024fa",
        )

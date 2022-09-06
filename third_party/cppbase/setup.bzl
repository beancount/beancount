"""Base C++ environment with Abseil or Boost and unit testing library."""

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@rules_foreign_cc//:workspace_definitions.bzl", "rules_foreign_cc_dependencies")


def setup_cppbase():
    setup_absl()
    setup_regexp()
    setup_googletest()
    setup_decimal()


def setup_regexp():
    # RE2
    if not native.existing_rule("com_google_re2"):
        # 2020-08-09
        http_archive(
            name = "com_google_re2",
            url = "https://github.com/google/re2/archive/ca11026a032ce2a3de4b3c389ee53d2bdc8794d6.tar.gz",
            sha256 = "18048cd4aeaed8d0b08745ea4a573ca7f27221e7df6c4d1c46a0cb286890d7e7",
            strip_prefix = "re2-ca11026a032ce2a3de4b3c389ee53d2bdc8794d6",
        )


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
        # 2020-09-23
        http_archive(
            name = "com_google_absl",
            url = "https://github.com/abseil/abseil-cpp/archive/refs/heads/lts_2021_11_02.tar.gz",
            sha256 = "140924c72546b7cda82cd7f19a6a1980d65c8285882eda4782f57e11fbfc5618",
            strip_prefix = "abseil-cpp-lts_2021_11_02",
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


def setup_decimal():
    # GoogleTest/GoogleMock framework. Used by most unit-tests.
    if not native.existing_rule("mpdecimal"):
        # 2020-07-25
        http_archive(
            name = "mpdecimal",
            url = "http://www.bytereef.org/software/mpdecimal/releases/mpdecimal-2.5.1.tar.gz",
            sha256 = "9f9cd4c041f99b5c49ffb7b59d9f12d95b683d88585608aa56a6307667b2b21f",
            strip_prefix = "mpdecimal-2.5.1",
            build_file = "//third_party/cppbase:mpdecimal.BUILD",
        )

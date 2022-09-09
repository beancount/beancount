"""Base C++ environment with Abseil or Boost and unit testing library."""

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")


def setup_cppbase():
    setup_absl()
    setup_regexp()
    setup_googletest()
    setup_decimal()


def setup_regexp():
    if not native.existing_rule("com_google_re2"):
        http_archive(
            name = "com_google_re2",
            # 2022-09-06
            url = "https://github.com/google/re2/archive/d61aa2ebbd25704fa3f9431369e9ebed091e46e3.tar.gz",
            sha256 = "2cebce88d2a026dc153e58429f0a24ca91f7099a5bde6b9712a64f5f96b5228d",
            strip_prefix = "re2-d61aa2ebbd25704fa3f9431369e9ebed091e46e3",
        )


def setup_absl():
    if not native.existing_rule("com_google_absl"):
        # How to update:
        # Abseil generally just does daily (or even subdaily) releases. None are
        # special, so just periodically update as necessary.
        #
        #  https://github.com/abseil/abseil-cpp/commits/master
        #  pick a recent release.
        #  Hit the 'clipboard with a left arrow' icon to copy the commit hex
        #
        http_archive(
            name = "com_google_absl",
            # 2022-09-06
            url = "https://github.com/abseil/abseil-cpp/archive/refs/heads/lts_2022_06_23.tar.gz",
            sha256 = "5acf752f4c9a6b4d935e73581ebc86c21722d72bc054b1cc8949a55ae179bc59",
            strip_prefix = "abseil-cpp-lts_2022_06_23",
        )


def setup_googletest():
    if not native.existing_rule("com_google_googletest"):
        # How to update:
        # Googletest generally just does daily (or even subdaily) releases along
        # with occasional numbered releases.
        #
        #  https://github.com/google/googletest/commits/master
        #  pick a recent release.
        #  Hit the 'clipboard with a left arrow' icon to copy the commit hex
        http_archive(
            name = "com_google_googletest",
            # 2022-09-06
            url = "https://github.com/google/googletest/archive/0e0d9feefab1b51aaab9dfd70132e93c0b6964e5.tar.gz",
            sha256 = "b65f43a9e1fc0bc937bb2b25f985bf54de7a60d7a5558b56f9627a9e3b08528f",
            strip_prefix = "googletest-0e0d9feefab1b51aaab9dfd70132e93c0b6964e5",
        )


def setup_decimal():
    if not native.existing_rule("mpdecimal"):
        http_archive(
            name = "mpdecimal",
            # 2022-09-06
            url = "http://www.bytereef.org/software/mpdecimal/releases/mpdecimal-2.5.1.tar.gz",
            sha256 = "9f9cd4c041f99b5c49ffb7b59d9f12d95b683d88585608aa56a6307667b2b21f",
            strip_prefix = "mpdecimal-2.5.1",
            build_file = "//third_party/cppbase:mpdecimal.BUILD",
        )

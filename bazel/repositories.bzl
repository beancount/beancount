load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:utils.bzl", "maybe")


def maybe_http_archive(**kwargs):
    kwargs.pop("updated", None)
    maybe(http_archive, **kwargs)


def beancount_dependencies():
    _build_dependencies()
    _cppbase_dependencies()
    _parser_dependencies()
    _python_dependencies()
    _proto_dependencies()


def _build_dependencies():
    # Setup for building third-party deps using configure/make/make-install.
    maybe_http_archive(
        name = "bazel_skylib",
        urls = ["https://mirror.bazel.build/github.com/bazelbuild/bazel-skylib/releases/download/1.3.0/bazel-skylib-1.3.0.tar.gz",
                "https://github.com/bazelbuild/bazel-skylib/releases/download/1.3.0/bazel-skylib-1.3.0.tar.gz"],
        sha256 = "74d544d96f4a5bb630d465ca8bbcfe231e3594e5aae57e1edbf17a6eb3ca2506",
        updated = "2022-09-10",
    )

    # Setup for building third-party deps using configure/make/make-install.
    maybe_http_archive(
        name = "rules_foreign_cc",
        url = "https://github.com/bazelbuild/rules_foreign_cc/archive/refs/tags/0.9.0.tar.gz",
        strip_prefix = "rules_foreign_cc-0.9.0",
        sha256 = "2a4d07cd64b0719b39a7c12218a3e507672b82a97b98c6a89d38565894cf7c51",
        updated = "2022-09-10",
    )

    # Setup for packaging rules.
    maybe_http_archive(
        name = "rules_pkg",
        urls = ["https://mirror.bazel.build/github.com/bazelbuild/rules_pkg/releases/download/0.7.1/rules_pkg-0.7.1.tar.gz",
                "https://github.com/bazelbuild/rules_pkg/releases/download/0.7.1/rules_pkg-0.7.1.tar.gz"],
        sha256 = "451e08a4d78988c06fa3f9306ec813b836b1d076d0f055595444ba4ff22b867f",
        updated = "2022-09-10",
    )


def _cppbase_dependencies():
    """Base C++ environment with Abseil or Boost and unit testing library."""

    maybe_http_archive(
        name = "rules_cc",
        urls = ["https://github.com/bazelbuild/rules_cc/archive/807a87190a117d5798f9f2c69427e9037e9891d3.zip"],
        strip_prefix = "rules_cc-807a87190a117d5798f9f2c69427e9037e9891d3",
        sha256 = "1b481c4cba4837117778f6296d33e2f14952bb356568cc8b526b91d75cbaec87",
        updated = "2022-09-10",
    )

    # How to update:
    # Abseil generally just does daily (or even subdaily) releases. None are
    # special, so just periodically update as necessary.
    #
    #  https://github.com/abseil/abseil-cpp/commits/master
    #  pick a recent release.
    #  Hit the 'clipboard with a left arrow' icon to copy the commit hex
    #
    maybe_http_archive(
        name = "com_google_absl",
        url = "https://github.com/abseil/abseil-cpp/archive/refs/heads/lts_2022_06_23.tar.gz",
        strip_prefix = "abseil-cpp-lts_2022_06_23",
        sha256 = "9c32fdb4a7d8c38f74145aa0d9d33ca639822022968732143fcd2cc5aee86701",
        updated = "2022-09-06",
    )

    maybe_http_archive(
        name = "com_google_re2",
        url = "https://github.com/google/re2/archive/d61aa2ebbd25704fa3f9431369e9ebed091e46e3.tar.gz",
        strip_prefix = "re2-d61aa2ebbd25704fa3f9431369e9ebed091e46e3",
        sha256 = "2cebce88d2a026dc153e58429f0a24ca91f7099a5bde6b9712a64f5f96b5228d",
        updated = "2022-09-06",
    )

    maybe_http_archive(
        name = "mpdecimal",
        url = "http://www.bytereef.org/software/mpdecimal/releases/mpdecimal-2.5.1.tar.gz",
        strip_prefix = "mpdecimal-2.5.1",
        sha256 = "9f9cd4c041f99b5c49ffb7b59d9f12d95b683d88585608aa56a6307667b2b21f",
        build_file = "//bazel/cppbase:mpdecimal.BUILD",
        updated = "2022-09-06",
    )

    # How to update:
    # Googletest generally just does daily (or even subdaily) releases along
    # with occasional numbered releases.
    #
    #  https://github.com/google/googletest/commits/master
    #  pick a recent release.
    #  Hit the 'clipboard with a left arrow' icon to copy the commit hex
    maybe_http_archive(
        name = "com_google_googletest",
        url = "https://github.com/google/googletest/archive/0e0d9feefab1b51aaab9dfd70132e93c0b6964e5.tar.gz",
        strip_prefix = "googletest-0e0d9feefab1b51aaab9dfd70132e93c0b6964e5",
        sha256 = "b65f43a9e1fc0bc937bb2b25f985bf54de7a60d7a5558b56f9627a9e3b08528f",
        updated = "2022-09-06",
    )


def _parser_dependencies():
    """Lex, flex, reflex, yacc, Bison, scanners & parser generators.

    Note that we specifically avoid rules_m4 and rules_bison and instead build our
    own local versions of these binaries, for isolation. See the
    `google/zetasql/bazel` configuration for an example.
    """

    maybe_http_archive(
        name = "m4",
        urls = ["https://ftp.gnu.org/gnu/m4/m4-1.4.19.tar.gz",
                "https://mirrors.kernel.org/gnu/m4/m4-1.4.19.tar.gz"],
        strip_prefix = "m4-1.4.19",
        sha256 = "3be4a26d825ffdfda52a56fc43246456989a3630093cced3fbddf4771ee58a70",
        build_file = "//bazel/parser:m4.BUILD",
        updated = "2022-09-08",
    )

    maybe_http_archive(
        name = "bison",
        url = "https://ftp.gnu.org/gnu/bison/bison-3.8.2.tar.gz",
        strip_prefix = "bison-3.8.2",
        sha256 = "06c9e13bdf7eb24d4ceb6b59205a4f67c2c7e7213119644430fe82fbd14a0abb",
        build_file = "//bazel/parser:bison.BUILD",
        updated = "2022-09-08",
    )

    # Note: Create a target of the headers, because generated code needs to include
    # and links against a reflex library.
    maybe_http_archive(
        name = "reflex",
        url = "https://github.com/Genivia/RE-flex/archive/v3.2.11.zip",
        strip_prefix = "RE-flex-3.2.11",
        sha256 = "2d61eaefec94836d4807aa61df5f946b183699db77c4c384db9843a83c33c63b",
        build_file = "//bazel/parser:reflex.BUILD",
        updated = "2022-09-08",
    )


def _proto_dependencies():
    """Protocol buffer support."""

    maybe_http_archive(
        name = "rules_proto",
        url = "https://github.com/bazelbuild/rules_proto/archive/b5e5fc85f70cf6bbef66c69b679c86168ad1bea6.zip",
        strip_prefix = "rules_proto-b5e5fc85f70cf6bbef66c69b679c86168ad1bea6",
        sha256 = "ac1a4caf8d3cb7483d16f60c91dc7a76fa75962a06327f83a06e4d0a32505fc2",
        updated = "2022-11-16",
    )

    maybe_http_archive(
        name = "com_google_protobuf",
        url = "https://github.com/protocolbuffers/protobuf/archive/refs/tags/v21.12.zip",
        strip_prefix = "protobuf-21.12",
        sha256 = "6a31b662deaeb0ac35e6287bda2f3369b19836e6c9f8828d4da444346f420298",
        # patches = [
        #     # Add a publicly visible static library target so that we can
        #     # link the protobuf module itself into our library.
        #     "//bazel/proto:protobuf_pyext_target.patch",
        # ],
        # patch_args = ["-p1"],
        updated = "2022-12-22",
    )

    # We managed to get protobuf working in a single module with the fast cpp proto casters.
    #
    # maybe_http_archive(
    #     name = "upb",
    #     urls = ["https://github.com/protocolbuffers/upb/archive/333722e94b35c26b9eb48bd7e471235374ab3737.zip"],
    #     strip_prefix = "upb-333722e94b35c26b9eb48bd7e471235374ab3737",
    #     sha256 = "f973aefa29d4191aad76cd1ba74ee3be4d2161b6c95d73c137f82560983912c6",
    # )
    #
    # Disable upb experiments for now. See {1fdb0ce4215b}
    # if not native.existing_rule("upb_extras"):
    #     native.local_repository(
    #         name = "upb_extras",
    #         path = "/home/blais/p/upb-extras",
    #     )


def _python_dependencies():
    """Setup for Python targets."""

    # Rules for building python.
    maybe_http_archive(
        name = "rules_python",
        url = "https://github.com/bazelbuild/rules_python/archive/refs/tags/0.16.1.tar.gz",
        strip_prefix = "rules_python-0.16.1",
        sha256 = "497ca47374f48c8b067d786b512ac10a276211810f4a580178ee9b9ad139323a",
        updated = "2022-12-07",
    )

    # Historically, subpar was the only way to produce a deployable Python
    # artifact in Bazel. This is no longer quite true; --build_python_zip allows
    # you to create executable Python zip artifacts with the standard py_binary
    # rule. Subpar still supports some use cases --build_python_zip doesn't: In
    # particular, it allows you to build archives of specific targets without
    # using a global command-line flag, and in some cases the archives can run
    # in-place without extraction.
    #
    # # Support building par files (Python archives).
    # maybe_http_archive(
    #     name = "subpar",
    #     url = "https://github.com/google/subpar/archive/2.0.0.tar.gz",
    #     strip_prefix = "subpar-2.0.0",
    #     sha256 = "b80297a1b8d38027a86836dbadc22f55dc3ecad56728175381aa6330705ac10f",
    # )

    # Note: "@pybind11_bazel" is declared in the top-level WORKSPACE in order to
    # allow loading of externally defined python_configure.
    #
    # TODO DELETE THIS:
    # maybe_http_archive(
    #     name = "pybind11_bazel",
    #     url = "https://github.com/pybind/pybind11_bazel/archive/faf56fb3df11287f26dbc66fdedf60a2fc2c6631.zip",
    #     strip_prefix = "pybind11_bazel-faf56fb3df11287f26dbc66fdedf60a2fc2c6631",
    #     sha256 = "a185aa68c93b9f62c80fcb3aadc3c83c763854750dc3f38be1dadcb7be223837",
    #     updated = "2022-11-03",
    # )

    # Rules for easy extension modules.
    maybe_http_archive(
        name = "pybind11",
        url = "https://github.com/pybind/pybind11/archive/refs/tags/v2.10.2.tar.gz",
        strip_prefix = "pybind11-2.10.2",
        sha256 = "93bd1e625e43e03028a3ea7389bba5d3f9f2596abc074b068e70f4ef9b1314ae",
        build_file = "@pybind11_bazel//:pybind11.BUILD",
        updated = "2022-12-21",
    )

    maybe_http_archive(
        name = "pybind11_protobuf",
        url = "https://github.com/blais/pybind11_protobuf/archive/505e54e81f77ae8f684630ba276af5cd029f7606.zip",
        strip_prefix = "pybind11_protobuf-505e54e81f77ae8f684630ba276af5cd029f7606",
        sha256 = "8a45cc912e9b8ad56f3b67424927b626315c143fe94d236059822d896eac2519",
        updated = "2022-12-09",
    )

    # native.local_repository(
    #     name = "pybind11_protobuf",
    #     path = "/home/blais/src/pybind11_protobuf",
    # )

    # See also: https://github.com/pybind/pybind11_protobuf/tree/experimental,
    # which implement C++ proto casters. We will use this eventually to expose protobufs.

    # abseil (Python)
    maybe_http_archive(
        name = "com_google_absl_py",
        url = "https://github.com/abseil/abseil-py/archive/v1.3.0.tar.gz",
        strip_prefix = "abseil-py-1.3.0",
        sha256 = "c0bf3e839b7b1c58ac75e41f72a708597087a6c7dd0582aec4914e0d98ec8b04",
        updated = "2022-12-22",
    )

    # Note: This appears unused anymore.
    # # Required by protobuf_python
    # # Release 1.12.0 -- Used by protobuf itself. See six.BUILD
    # maybe_http_archive(
    #     name = "six",
    #     url = "https://pypi.python.org/packages/source/s/six/six-1.12.0.tar.gz",
    #     sha256 = "d16a0141ec1a18405cd4ce8b4613101da75da0e9a7aec5bdd4fa804d0e0eba73",
    #     build_file = "@com_google_protobuf//bazel:six.BUILD",
    # )
    # # TODO(blais): With the latest version of protobuf, bind() does not appear
    # # to solve the problem of protobuf depending on this package. Define the
    # # archive directly above as 'six' instead of 'six_archive', seems to make
    # # py_proto_library() work.
    # #
    # # native.bind(
    # #     name = "six",
    # #     actual = "@six_archive//:six",
    # # )

    # Local Python installation
    # TODO(blais): There's a better version of this in https://github.com/pybind/pybind11_bazel.
    ## python_configure(name = "local_config_python")
    ## native.bind(
    ##     name = "python_headers",
    ##     actual = "@local_config_python//:python_headers",
    ## )
    ## native.register_toolchains("@local_config_python//:toolchain")

    maybe_http_archive(
        name = "python_magic",
        url = "https://github.com/ahupp/python-magic/archive/0.4.18.zip",
        strip_prefix = "python-magic-0.4.18",
        sha256 = "ed8b7ae88548bb1bfec5be448d4a515f7fe267bc50d184aa9f1da1734d70aee9",
        build_file = "//bazel/python:python_magic.BUILD",
    )

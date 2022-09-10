"""Setup for Python targets."""

load("//third_party/bazel:maybe_archive.bzl", "maybe_http_archive")

# TODO(blais): It would be real nice if this was available externally.
load("//third_party/python:python_configure.bzl", "python_configure")


def beancount_python_dependencies():
    # Rules for building python.
    maybe_http_archive(
        name = "rules_python",
        # 2022-09-10
        url = "https://github.com/bazelbuild/rules_python/archive/refs/tags/0.12.0.tar.gz",
        strip_prefix = "rules_python-0.12.0",
        sha256 = "b593d13bb43c94ce94b483c2858e53a9b811f6f10e1e0eedc61073bd90e58d9c",
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
    #     strip_prefix = "subpar-2.0.0",
    #     urls = ["https://github.com/google/subpar/archive/2.0.0.tar.gz"],
    #     sha256 = "b80297a1b8d38027a86836dbadc22f55dc3ecad56728175381aa6330705ac10f",
    # )

    # Rules for easy extension modules.
    # 2020-11-25
    maybe_http_archive(
        name = "pybind11_bazel",
        strip_prefix = "pybind11_bazel-26973c0ff320cb4b39e45bc3e4297b82bc3a6c09",
        sha256 = "a5666d950c3344a8b0d3892a88dc6b55c8e0c78764f9294e806d69213c03f19d",
        urls = ["https://github.com/pybind/pybind11_bazel/archive/26973c0ff320cb4b39e45bc3e4297b82bc3a6c09.zip"],
    )
    maybe_http_archive(
        name = "pybind11",
        build_file = "@pybind11_bazel//:pybind11.BUILD",
        sha256 = "cdbe326d357f18b83d10322ba202d69f11b2f49e2d87ade0dc2be0c5c34f8e2a",
        strip_prefix = "pybind11-2.6.1",
        urls = ["https://github.com/pybind/pybind11/archive/v2.6.1.tar.gz"],
    )

    # NOTE: We've had to fork and patch up the given repository in order for it
    # to work, this is too recent.
    maybe_http_archive(
        name = "pybind11_protobuf",
        strip_prefix = "pybind11_protobuf-experimental",
        sha256 = "970bb8bee40bd8bf1f2ed5a671142c78518e610256a676598244348e3316a8d8",
        urls = ["https://github.com/blais/pybind11_protobuf/archive/experimental.zip"],
    )
    # native.local_repository(
    #     name = "pybind11_protobuf",
    #     path = "/home/blais/src/pybind11_protobuf",
    # )

    # See also: https://github.com/pybind/pybind11_protobuf/tree/experimental,
    # which implement C++ proto casters. We will use this eventually to expose protobufs.

    # abseil (Python)
    # 2020-05-11
    maybe_http_archive(
        name = "com_google_absl_py",
        urls = ["https://github.com/abseil/abseil-py/archive/pypi-v0.9.0.tar.gz"],
        sha256 = "e7f5624c861c51901d9d40ebb09490cf728e3bd6133c9ce26059cdc548fc201e",
        strip_prefix = "abseil-py-pypi-v0.9.0",
    )

    # Required by protobuf_python
    # Release 1.12.0 -- Used by protobuf itself. See six.BUILD
    maybe_http_archive(
        name = "six",
        build_file = "@com_google_protobuf//third_party:six.BUILD",
        url = "https://pypi.python.org/packages/source/s/six/six-1.12.0.tar.gz",
        sha256 = "d16a0141ec1a18405cd4ce8b4613101da75da0e9a7aec5bdd4fa804d0e0eba73",
    )
    # TODO(blais): With the latest version of protobuf, bind() does not appear
    # to solve the problem of protobuf depending on this package. Define the
    # archive directly above as 'six' instead of 'six_archive', seems to make
    # py_proto_library() work.
    #
    # native.bind(
    #     name = "six",
    #     actual = "@six_archive//:six",
    # )

    # Local Python installation
    # TODO(blais): There's a better version of this in https://github.com/pybind/pybind11_bazel.
    python_configure(name = "local_config_python")
    native.bind(
        name = "python_headers",
        actual = "@local_config_python//:python_headers",
    )
    native.register_toolchains("@local_config_python//:toolchain")

    maybe_http_archive(
        name = "python_magic",
        url = "https://github.com/ahupp/python-magic/archive/0.4.18.zip",
        sha256 = "ed8b7ae88548bb1bfec5be448d4a515f7fe267bc50d184aa9f1da1734d70aee9",
        build_file = "//third_party/python:python_magic.BUILD",
        strip_prefix = "python-magic-0.4.18",
    )

"""Setup for Python targets."""

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

# TODO(blais): It would be real nice if this was available externally.
load("//third_party/python:python_configure.bzl", "python_configure")


def setup_python():
    # Rules for building python.
    if not native.existing_rule("rules_python"):
        http_archive(
            name = "rules_python",
            url = "https://github.com/bazelbuild/rules_python/archive/708ed8679d7510a331ce9a7b910a2a056d24f7b1.tar.gz",
            strip_prefix = "rules_python-708ed8679d7510a331ce9a7b910a2a056d24f7b1",
            sha256 = "f352c434f9a81c655375deb071b25e26757b7e95f25d50c4833da4e5f9f00840",
        )

    # Support building par files (Python archives).
    if not native.existing_rule("subpar"):
        http_archive(
            name = "subpar",
            strip_prefix = "subpar-2.0.0",
            urls = [
                "https://github.com/google/subpar/archive/2.0.0.tar.gz",
            ],
            sha256 = "b80297a1b8d38027a86836dbadc22f55dc3ecad56728175381aa6330705ac10f",
        )

    # Rules for easy extension modules.
    # 2020-11-25
    http_archive(
        name = "pybind11_bazel",
        strip_prefix = "pybind11_bazel-26973c0ff320cb4b39e45bc3e4297b82bc3a6c09",
        sha256 = "a5666d950c3344a8b0d3892a88dc6b55c8e0c78764f9294e806d69213c03f19d",
        urls = ["https://github.com/pybind/pybind11_bazel/archive/26973c0ff320cb4b39e45bc3e4297b82bc3a6c09.zip"],
    )
    http_archive(
        name = "pybind11",
        build_file = "@pybind11_bazel//:pybind11.BUILD",
        sha256 = "cdbe326d357f18b83d10322ba202d69f11b2f49e2d87ade0dc2be0c5c34f8e2a",
        strip_prefix = "pybind11-2.6.1",
        urls = ["https://github.com/pybind/pybind11/archive/v2.6.1.tar.gz"],
    )
    http_archive(
        name = "pybind11_protobuf",
        # build_file = "//third_party/python:pybind11_protobuf.BUILD",
        strip_prefix = "pybind11_protobuf-experimental",
        sha256 = "144e74162e115b91f658d1039dadd41b1f1f00c295ba77f13b50c483716c93bb",
        urls = ["https://github.com/pybind/pybind11_protobuf/archive/experimental.zip"],
    )

    # See also: https://github.com/pybind/pybind11_protobuf/tree/experimental,
    # which implement C++ proto casters. We will use this eventually to expose protobufs.

    # abseil (Python)
    if not native.existing_rule("com_google_absl_py"):
        # 2020-05-11
        http_archive(
            name = "com_google_absl_py",
            urls = ["https://github.com/abseil/abseil-py/archive/pypi-v0.9.0.tar.gz"],
            sha256 = "e7f5624c861c51901d9d40ebb09490cf728e3bd6133c9ce26059cdc548fc201e",
            strip_prefix = "abseil-py-pypi-v0.9.0",
        )

    # Required by protobuf_python
    if not native.existing_rule("six"):
        # Release 1.12.0 -- Used by protobuf itself. See six.BUILD
        http_archive(
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

    # Add some of these later if you need pip repos to build an hermetic version.
    #
    # load("@rules_python//python:repositories.bzl", "py_repositories")
    # py_repositories()
    # # Only needed if using the packaging rules.
    # load("@rules_python//python:pip.bzl", "pip_repositories")
    # pip_repositories()

    if not native.existing_rule("python_magic"):
        http_archive(
            name = "python_magic",
            url = "https://github.com/ahupp/python-magic/archive/0.4.18.zip",
            sha256 = "ed8b7ae88548bb1bfec5be448d4a515f7fe267bc50d184aa9f1da1734d70aee9",
            build_file = "//third_party/python:python_magic.BUILD",
            strip_prefix = "python-magic-0.4.18",
        )

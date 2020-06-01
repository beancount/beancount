"""Setup for Python targets."""

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

# TODO(blais): It would be real nice if this was available externally.
load("//third_party/python:python_configure.bzl", "python_configure")


def setup_python():
    # Rules for building python.
    if not native.existing_rule("rules_python"):
        http_archive(
            name = "rules_python",
            url = "https://github.com/bazelbuild/rules_python/releases/download/0.0.2/rules_python-0.0.2.tar.gz",
            sha256 = "aa96a691d3a8177f3215b14b0edc9641787abaaa30363a080165d06ab65e1161",
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
    # 2020-05-11
    http_archive(
        name = "pybind11_bazel",
        strip_prefix = "pybind11_bazel-16ed1b8f308d2b3dec9d7e6decaad49ce4d28b43",
        sha256 = "f1044df0475bbe819e285785ee9599d94f98ac3c86ddfb73fe16cfeb568bb381",
        urls = ["https://github.com/pybind/pybind11_bazel/archive/16ed1b8f308d2b3dec9d7e6decaad49ce4d28b43.zip"],
    )
    http_archive(
        name = "pybind11",
        build_file = "@pybind11_bazel//:pybind11.BUILD",
        sha256 = "97504db65640570f32d3fdf701c25a340c8643037c3b69aec469c10c93dc8504",
        strip_prefix = "pybind11-2.5.0",
        urls = ["https://github.com/pybind/pybind11/archive/v2.5.0.tar.gz"],
    )

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

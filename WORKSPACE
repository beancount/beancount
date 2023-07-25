"""Bazel workspace for the Beancount.

Note that:

- This is using a hermetic Python runtime, which must be 3.10.
- The pip package for 'protobuf' version *must* match that which is in use in
  this build.
- Your version of Bazel installed must match that which is used in the github
  actions (see beancount/.bazelversion).

"""
workspace(name="beancount")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

rules_python_version = "93f5ea2f01ce7eb870d3ad3943eda5d354cdaac5"

http_archive(
    name = "rules_python",
    strip_prefix = "rules_python-{}".format(rules_python_version),
    url = "https://github.com/bazelbuild/rules_python/archive/{}.zip".format(rules_python_version),
    sha256 = "179541b519e8fd7c8fbfd0d2a2a51835cf7c83bd6a8f0f3fd599a0910d1a0981"
)

load("@rules_python//python:repositories.bzl", "py_repositories", "python_register_toolchains")

py_repositories()

python_register_toolchains(
    name = "python3_10",
    python_version = "3.10",
)

load("@python3_10//:defs.bzl", "interpreter")

load("@rules_python//python:pip.bzl", "pip_parse")

pip_parse(
   name = "pypi",
   requirements_lock = "//requirements:dev_lock.txt",
   python_interpreter_target = interpreter,
)

load("@pypi//:requirements.bzl", "install_deps")

install_deps()

pip_parse(
   name = "pypi_tools",
   requirements_lock = "//requirements:tools_lock.txt",
   python_interpreter_target = interpreter,
)

#------------------------------------------------------------------------------
# Direct dependencies of Beancount.
#
# Each of the following files defines `http_archive` repositories. We group them
# into various subgroups of related features. This could theoretically be
# serialized to a single file, but we keep them to subdirectories to organize
# all the related files. We use maybe_http_archive() throughout.

# Allow an external python_configure() definition to be pulled from pybind11_bael.
http_archive(
    name = "pybind11_bazel",
    url = "https://github.com/pybind/pybind11_bazel/archive/faf56fb3df11287f26dbc66fdedf60a2fc2c6631.zip",
    strip_prefix = "pybind11_bazel-faf56fb3df11287f26dbc66fdedf60a2fc2c6631",
    sha256 = "a185aa68c93b9f62c80fcb3aadc3c83c763854750dc3f38be1dadcb7be223837",
)  # updated = "2022-11-03",

load("@pybind11_bazel//:python_configure.bzl", "python_configure")
python_configure(name = "local_config_python", python_interpreter_target = interpreter)
bind(
    name = "python_headers",
    actual = "@local_config_python//:python_headers",
)

# Bazel general rules packages
load("//bazel:repositories.bzl", "beancount_dependencies")
beancount_dependencies()

#------------------------------------------------------------------------------
# Indirect dependencies from packages we depend on.
#
# Calling these functions will define more repositories, and if there are
# collisions the ones we depend on above will prevail; hopefully the ones we
# define above are compatible with those required by the packages themselves.

load("@bazel_skylib//:workspace.bzl", "bazel_skylib_workspace")
bazel_skylib_workspace()

load("@rules_foreign_cc//foreign_cc:repositories.bzl", "rules_foreign_cc_dependencies")
rules_foreign_cc_dependencies()

load("@rules_pkg//:deps.bzl", "rules_pkg_dependencies")
rules_pkg_dependencies()

load("@rules_cc//cc:repositories.bzl", "rules_cc_dependencies")
rules_cc_dependencies()

load("@rules_proto//proto:repositories.bzl", "rules_proto_dependencies", "rules_proto_toolchains")
rules_proto_dependencies()
rules_proto_toolchains()

# TODO(blais): Update RE-flex version and code.
# TODO(blais): Upgrade the pybind11 deps.

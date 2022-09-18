"""Bazel workspace for the Beancount.

Note that:

- This is using a local Python runtime, which must be 3.10
- The pip package for 'protobuf' version *must* match that which is in use in
  this build.
- Your version of Bazel installed must match that which is used in the github
  actions (see beancount/.bazelversion).

"""
workspace(name="beancount")

#------------------------------------------------------------------------------
# Direct dependencies of Beancount.
#
# Each of the following files defines `http_archive` repositories. We group them
# into various subgroups of related features. This could theoretically be
# serialized to a single file, but we keep them to subdirectories to organize
# all the related files. We use maybe_http_archive() throughout.

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

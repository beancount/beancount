load("@bazel_skylib//:workspace.bzl", "bazel_skylib_workspace")
load("@rules_foreign_cc//foreign_cc:repositories.bzl", "rules_foreign_cc_dependencies")
load("@rules_pkg//:deps.bzl", "rules_pkg_dependencies")

def setup_rules_dependencies():
    bazel_skylib_workspace()
    rules_foreign_cc_dependencies()
    rules_pkg_dependencies()

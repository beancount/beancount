"""Workaround for the 'bazel_version' rule.

Some other libraries requires a "bazel_version" repo but depend on it being
something different. Override them both by defining the repo first.
"""

# Makes Bazel version available in BUILD files as bazel_version.
def _impl(repository_ctx):
    repository_ctx.file("bazel_version.bzl", "bazel_version='" + native.bazel_version + "'")
    repository_ctx.file("def.bzl", "BAZEL_VERSION='" + native.bazel_version + "'")
    repository_ctx.file(
        "BUILD",
        "exports_files(['bazel_version.bzl', 'def.bzl'])",
    )

def bazel_version():
    # Required by gRPC
    if not native.existing_rule("bazel_version"):
        _bazel_version_repository = repository_rule(
            implementation = _impl,
            local = True,
        )
        _bazel_version_repository(name = "bazel_version")

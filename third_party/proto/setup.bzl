"""Protocol buffer support."""

load("//third_party/bazel:maybe_archive.bzl", "maybe_http_archive")


def setup_proto_dependencies():
    # v21.5
    maybe_http_archive(
        name = "com_google_protobuf",
        urls = ["https://github.com/protocolbuffers/protobuf/archive/refs/tags/v21.5.zip"],
        sha256 = "468a16f50694822291da57e304197f5322607dbed1a9d93192ff18de642c6cac",
        strip_prefix = "protobuf-21.5",
        patches = [
            # Add a publicly visible static library target so that we can
            # link the protobuf module itself into our library.
            "//third_party/proto:protobuf_pyext_target.patch",
        ],
        patch_args = ["-p1"],
    )

    # The following is copied from file '@protobuf//:protobuf_deps.bzl' for
    # v21.5. I removed targets we don't need.
    #
    # if not native.existing_rule("bazel_skylib"):
    # if not native.existing_rule("com_google_absl"):
    # if not native.existing_rule("zlib"):
    # if not native.existing_rule("rules_java"):
    # if not native.existing_rule("rules_jvm_external"):
    # if not native.existing_rule("io_bazel_rules_kotlin"):
    # if not native.existing_rule("rules_pkg"):

    maybe_http_archive(
        name = "rules_cc",
        urls = ["https://github.com/bazelbuild/rules_cc/archive/818289e5613731ae410efb54218a4077fb9dbb03.zip"],
        strip_prefix = "rules_cc-818289e5613731ae410efb54218a4077fb9dbb03",
        sha256 = "0adbd6f567291ad526e82c765e15aed33cea5e256eeba129f1501142c2c56610",
    )

    maybe_http_archive(
        name = "rules_proto",
        urls = ["https://github.com/bazelbuild/rules_proto/archive/f7a30f6f80006b591fa7c437fe5a951eb10bcbcf.zip"],
        strip_prefix = "rules_proto-f7a30f6f80006b591fa7c437fe5a951eb10bcbcf",
        sha256 = "a4382f78723af788f0bc19fd4c8411f44ffe0a72723670a34692ffad56ada3ac",
    )

    maybe_http_archive(
        name = "rules_python",
        sha256 = "9fcf91dbcc31fde6d1edb15f117246d912c33c36f44cf681976bd886538deba6",
        strip_prefix = "rules_python-0.8.0",
        url = "https://github.com/bazelbuild/rules_python/archive/refs/tags/0.8.0.tar.gz",
    )

    maybe_http_archive(
        name = "upb",
        urls = ["https://github.com/protocolbuffers/upb/archive/333722e94b35c26b9eb48bd7e471235374ab3737.zip"],
        strip_prefix = "upb-333722e94b35c26b9eb48bd7e471235374ab3737",
        sha256 = "f973aefa29d4191aad76cd1ba74ee3be4d2161b6c95d73c137f82560983912c6",
    )


# We managed to get protobuf working in a single module with the fast cpp proto casters.
# Disable upb experiments for now. See {1fdb0ce4215b}
#
## load("//third_party/proto:setup.bzl", "setup_upb")
## setup_upb()
## load("@upb//bazel:workspace_deps.bzl", "upb_deps")
## upb_deps()


def setup_upb_extras():
    if not native.existing_rule("upb_extras"):
        native.local_repository(
            name = "upb_extras",
            path = "/home/blais/p/upb-extras",
        )

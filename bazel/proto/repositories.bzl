"""Protocol buffer support."""

load("//bazel/build:maybe_archive.bzl", "maybe_http_archive")


def beancount_proto_dependencies():
    maybe_http_archive(
        name = "rules_proto",
        url = "https://github.com/bazelbuild/rules_proto/archive/f7a30f6f80006b591fa7c437fe5a951eb10bcbcf.zip",
        strip_prefix = "rules_proto-f7a30f6f80006b591fa7c437fe5a951eb10bcbcf",
        sha256 = "a4382f78723af788f0bc19fd4c8411f44ffe0a72723670a34692ffad56ada3ac",
    )

    maybe_http_archive(
        name = "com_google_protobuf",
        # v21.5
        url = "https://github.com/protocolbuffers/protobuf/archive/refs/tags/v21.5.zip",
        strip_prefix = "protobuf-21.5",
        sha256 = "468a16f50694822291da57e304197f5322607dbed1a9d93192ff18de642c6cac",
        patches = [
            # Add a publicly visible static library target so that we can
            # link the protobuf module itself into our library.
            "//bazel/proto:protobuf_pyext_target.patch",
        ],
        patch_args = ["-p1"],
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

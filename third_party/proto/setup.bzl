"""Protocol buffer support."""

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")


def _github_archive(repo, commit, **kwargs):
    repo_name = repo.split("/")[-1]
    http_archive(
        urls = [repo + "/archive/" + commit + ".zip"],
        strip_prefix = repo_name + "-" + commit,
        **kwargs
    )


def setup_proto():
    if not native.existing_rule("com_google_protobuf"):
        # v21.5
        http_archive(
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

    if not native.existing_rule("rules_cc"):
        _github_archive(
            name = "rules_cc",
            repo = "https://github.com/bazelbuild/rules_cc",
            commit = "818289e5613731ae410efb54218a4077fb9dbb03",
            sha256 = "0adbd6f567291ad526e82c765e15aed33cea5e256eeba129f1501142c2c56610",
        )


    if not native.existing_rule("rules_proto"):
        _github_archive(
            name = "rules_proto",
            repo = "https://github.com/bazelbuild/rules_proto",
            commit = "f7a30f6f80006b591fa7c437fe5a951eb10bcbcf",
            sha256 = "a4382f78723af788f0bc19fd4c8411f44ffe0a72723670a34692ffad56ada3ac",
        )

    if not native.existing_rule("rules_python"):
        http_archive(
            name = "rules_python",
            sha256 = "9fcf91dbcc31fde6d1edb15f117246d912c33c36f44cf681976bd886538deba6",
            strip_prefix = "rules_python-0.8.0",
            url = "https://github.com/bazelbuild/rules_python/archive/refs/tags/0.8.0.tar.gz",
        )


    if not native.existing_rule("rules_pkg"):
        http_archive(
            name = "rules_pkg",
            urls = [
                "https://mirror.bazel.build/github.com/bazelbuild/rules_pkg/releases/download/0.7.0/rules_pkg-0.7.0.tar.gz",
                "https://github.com/bazelbuild/rules_pkg/releases/download/0.7.0/rules_pkg-0.7.0.tar.gz",
            ],
            sha256 = "8a298e832762eda1830597d64fe7db58178aa84cd5926d76d5b744d6558941c2",
        )

    if not native.existing_rule("upb"):
        _github_archive(
            name = "upb",
            repo = "https://github.com/protocolbuffers/upb",
            commit = "333722e94b35c26b9eb48bd7e471235374ab3737",
            sha256 = "f973aefa29d4191aad76cd1ba74ee3be4d2161b6c95d73c137f82560983912c6",
        )


def setup_upb_extras():
    if not native.existing_rule("upb_extras"):
        native.local_repository(
            name = "upb_extras",
            path = "/home/blais/p/upb-extras",
        )


def setup_riegeli():
    ## # 2020-12-08
    ## http_archive(
    ##     name = "com_google_riegeli",
    ##     sha256 = "b7ba2d3426c9dec898e39c6b79dbd4730a907f0e0978582b63f7caa560def7f0",
    ##     strip_prefix = "riegeli-7f8553a806ed374a6d192b5a25a5f69e46f90108",
    ##     urls = ["https://github.com/google/riegeli/archive/7f8553a806ed374a6d192b5a25a5f69e46f90108.zip"],
    ## )
    # 2022-09-06
    http_archive(
        name = "com_google_riegeli",
        urls = ["https://github.com/google/riegeli/archive/107fbb47e28a9e82830ab20a1fb90b7f1b92dfb5.zip"],
        strip_prefix = "riegeli-107fbb47e28a9e82830ab20a1fb90b7f1b92dfb5",
        sha256 = "e0dbd8e77c4ecd438e98736f00c48cbe5f62742e29576f0fd4c329eaa85eefaf",
    )

    # 2019-02-22
    http_archive(
        name = "highwayhash",
        build_file = "@com_google_riegeli//third_party:highwayhash.BUILD",
        sha256 = "cf891e024699c82aabce528a024adbe16e529f2b4e57f954455e0bf53efae585",
        strip_prefix = "highwayhash-276dd7b4b6d330e4734b756e97ccfb1b69cc2e12",
        urls = ["https://github.com/google/highwayhash/archive/276dd7b4b6d330e4734b756e97ccfb1b69cc2e12.zip"],
    )

    if not native.existing_rule("org_brotli"):
        # 2022-09-05
        http_archive(
            name = "org_brotli",
            urls = ["https://github.com/google/brotli/archive/9801a2c5d6c67c467ffad676ac301379bb877fc3.zip"],
            strip_prefix = "brotli-9801a2c5d6c67c467ffad676ac301379bb877fc3",
            sha256 = "79edf11c219ee05fa57f5ec7b2a224d1d945679c457f4585bb834a6e2c321b8f",
        )

    http_archive(
        name = "net_zstd",
        build_file = "@com_google_riegeli//third_party:net_zstd.BUILD",
        sha256 = "b6c537b53356a3af3ca3e621457751fa9a6ba96daf3aebb3526ae0f610863532",
        strip_prefix = "zstd-1.4.5/lib",
        urls = [
            "https://github.com/facebook/zstd/archive/v1.4.5.zip",  # 2020-05-22
        ],
    )

    http_archive(
        name = "snappy",
        build_file = "@com_google_riegeli//third_party:snappy.BUILD",
        sha256 = "38b4aabf88eb480131ed45bfb89c19ca3e2a62daeb081bdf001cfb17ec4cd303",
        strip_prefix = "snappy-1.1.8",
        urls = [
            "https://github.com/google/snappy/archive/1.1.8.zip",  # 2020-01-14
        ],
    )

    http_archive(
        name = "crc32c",
        build_file = "//third_party:crc32.BUILD",
        sha256 = "338f1d9d95753dc3cdd882dfb6e176bbb4b18353c29c411ebcb7b890f361722e",
        strip_prefix = "crc32c-1.1.0",
        urls = [
            "https://github.com/google/crc32c/archive/1.1.0.zip",  # 2019-05-24
        ],
    )

    # http_archive(
    #     name = "zlib",
    #     build_file = "//third_party:zlib.BUILD",
    #     sha256 = "c3e5e9fdd5004dcb542feda5ee4f0ff0744628baf8ed2dd5d66f8ca1197cb1a1",
    #     strip_prefix = "zlib-1.2.11",
    #     urls = [
    #         "http://zlib.net/fossils/zlib-1.2.11.tar.gz",  # 2017-01-15
    #     ],
    # )

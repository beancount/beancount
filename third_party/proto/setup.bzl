"""Protocol buffer support."""


load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")


def setup_proto():
    # Protobuf
    if not native.existing_rule("com_google_protobuf"):
        http_archive(
            name = "com_google_protobuf",
            urls = ["https://github.com/protocolbuffers/protobuf/archive/v3.11.4.tar.gz"],
            sha256 = "a79d19dcdf9139fa4b81206e318e33d245c4c9da1ffed21c87288ed4380426f9",
            strip_prefix = "protobuf-3.11.4",
        )

    # Rules for building protos.
    if not native.existing_rule("rules_proto"):
        http_archive(
            name = "rules_proto",
            sha256 = "602e7161d9195e50246177e7c55b2f39950a9cf7366f74ed5f22fd45750cd208",
            strip_prefix = "rules_proto-97d8af4dc474595af3900dd85cb3a29ad28cc313",
            urls = [
                "https://mirror.bazel.build/github.com/bazelbuild/rules_proto/archive/97d8af4dc474595af3900dd85cb3a29ad28cc313.tar.gz",
                "https://github.com/bazelbuild/rules_proto/archive/97d8af4dc474595af3900dd85cb3a29ad28cc313.tar.gz",
            ],
        )

    # cc_proto_library
    if not native.existing_rule("rules_cc"):
        http_archive(
            name = "rules_cc",
            sha256 = "29daf0159f0cf552fcff60b49d8bcd4f08f08506d2da6e41b07058ec50cfeaec",
            strip_prefix = "rules_cc-b7fe9697c0c76ab2fd431a891dbb9a6a32ed7c3e",
            urls = ["https://github.com/bazelbuild/rules_cc/archive/b7fe9697c0c76ab2fd431a891dbb9a6a32ed7c3e.tar.gz"],
        )


def setup_riegeli():
    # 2020-05-23
    http_archive(
        name = "com_google_riegeli",
        #sha256 = "94257912684f999ce7f4272878826f3194301a10",
        strip_prefix = "riegeli-94257912684f999ce7f4272878826f3194301a10",
        urls = [
            "https://github.com/google/riegeli/archive/94257912684f999ce7f4272878826f3194301a10.zip",
        ],
    )

    http_archive(
        name = "highwayhash",
        build_file = "@com_google_riegeli//third_party:highwayhash.BUILD",
        sha256 = "cf891e024699c82aabce528a024adbe16e529f2b4e57f954455e0bf53efae585",
        strip_prefix = "highwayhash-276dd7b4b6d330e4734b756e97ccfb1b69cc2e12",
        urls = [
            "https://mirror.bazel.build/github.com/google/highwayhash/archive/276dd7b4b6d330e4734b756e97ccfb1b69cc2e12.zip",
            "https://github.com/google/highwayhash/archive/276dd7b4b6d330e4734b756e97ccfb1b69cc2e12.zip",  # 2019-02-22
        ],
    )

    http_archive(
        name = "org_brotli",
        sha256 = "6e69be238ff61cef589a3fa88da11b649c7ff7a5932cb12d1e6251c8c2e17a2f",
        strip_prefix = "brotli-1.0.7",
        urls = [
            #"https://mirror.bazel.build/github.com/google/brotli/archive/v1.0.7.zip",
            "https://github.com/google/brotli/archive/v1.0.7.zip",  # 2018-10-23
        ],
    )

    http_archive(
        name = "net_zstd",
        build_file = "@com_google_riegeli//third_party:net_zstd.BUILD",
        sha256 = "5a874ba43d1ec6d1c03f070f5fa820ff834ef85d5525b03effa7508c9087ba55",
        strip_prefix = "zstd-1.4.4/lib",
        urls = [
            #"https://mirror.bazel.build/github.com/facebook/zstd/archive/v1.4.4.zip",
            "https://github.com/facebook/zstd/archive/v1.4.4.zip",  # 2019-11-04
        ],
    )

    http_archive(
        name = "snappy",
        build_file = "@com_google_riegeli//third_party:snappy.BUILD",
        sha256 = "38b4aabf88eb480131ed45bfb89c19ca3e2a62daeb081bdf001cfb17ec4cd303",
        strip_prefix = "snappy-1.1.8",
        urls = [
            #"https://mirror.bazel.build/github.com/google/snappy/archive/1.1.8.zip",
            "https://github.com/google/snappy/archive/1.1.8.zip",  # 2020-01-14
        ],
    )

    http_archive(
        name = "crc32c",
        build_file = "//third_party:crc32.BUILD",
        sha256 = "338f1d9d95753dc3cdd882dfb6e176bbb4b18353c29c411ebcb7b890f361722e",
        strip_prefix = "crc32c-1.1.0",
        urls = [
            "https://mirror.bazel.build/github.com/google/crc32c/archive/1.1.0.zip",
            "https://github.com/google/crc32c/archive/1.1.0.zip",  # 2019-05-24
        ],
    )

    # http_archive(
    #     name = "zlib",
    #     build_file = "//third_party:zlib.BUILD",
    #     sha256 = "c3e5e9fdd5004dcb542feda5ee4f0ff0744628baf8ed2dd5d66f8ca1197cb1a1",
    #     strip_prefix = "zlib-1.2.11",
    #     urls = [
    #         "http://mirror.bazel.build/zlib.net/fossils/zlib-1.2.11.tar.gz",
    #         "http://zlib.net/fossils/zlib-1.2.11.tar.gz",  # 2017-01-15
    #     ],
    # )

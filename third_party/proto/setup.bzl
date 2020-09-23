"""Protocol buffer support."""


load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")


def setup_proto():
    # Protobuf
    if not native.existing_rule("com_google_protobuf"):
        http_archive(
            name = "com_google_protobuf",
            urls = ["https://github.com/protocolbuffers/protobuf/archive/v4.0.0-rc2.tar.gz"],
            sha256 = "cd26c9011e065b4eb95c79a74bb4f882f3b0beb6629a9c50312e387775c681c9",
            strip_prefix = "protobuf-4.0.0-rc2",
        )

    # Rules for building protos.
    if not native.existing_rule("rules_proto"):
        http_archive(
            name = "rules_proto",
            sha256 = "602e7161d9195e50246177e7c55b2f39950a9cf7366f74ed5f22fd45750cd208",
            strip_prefix = "rules_proto-97d8af4dc474595af3900dd85cb3a29ad28cc313",
            urls = [
                #"https://mirror.bazel.build/github.com/bazelbuild/rules_proto/archive/97d8af4dc474595af3900dd85cb3a29ad28cc313.tar.gz",
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
    # 2020-09-20
    http_archive(
        name = "com_google_riegeli",
        sha256 = "2cc898b39eeedca79d12d9e83f39c81a6049bb8be57e6a864993df6e5ac857cb",
        strip_prefix = "riegeli-f7a7fbf586cf108c3603857a0d8978a724deb352",
        urls = ["https://github.com/google/riegeli/archive/f7a7fbf586cf108c3603857a0d8978a724deb352.zip"],
    )

    http_archive(
        name = "highwayhash",
        build_file = "@com_google_riegeli//third_party:highwayhash.BUILD",
        sha256 = "cf891e024699c82aabce528a024adbe16e529f2b4e57f954455e0bf53efae585",
        strip_prefix = "highwayhash-276dd7b4b6d330e4734b756e97ccfb1b69cc2e12",
        urls = [
            #"https://mirror.bazel.build/github.com/google/highwayhash/archive/276dd7b4b6d330e4734b756e97ccfb1b69cc2e12.zip",
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
        sha256 = "b6c537b53356a3af3ca3e621457751fa9a6ba96daf3aebb3526ae0f610863532",
        strip_prefix = "zstd-1.4.5/lib",
        urls = [
            #"https://mirror.bazel.build/github.com/facebook/zstd/archive/v1.4.5.zip",
            "https://github.com/facebook/zstd/archive/v1.4.5.zip",  # 2020-05-22
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
            #"https://mirror.bazel.build/github.com/google/crc32c/archive/1.1.0.zip",
            "https://github.com/google/crc32c/archive/1.1.0.zip",  # 2019-05-24
        ],
    )

    # http_archive(
    #     name = "zlib",
    #     build_file = "//third_party:zlib.BUILD",
    #     sha256 = "c3e5e9fdd5004dcb542feda5ee4f0ff0744628baf8ed2dd5d66f8ca1197cb1a1",
    #     strip_prefix = "zlib-1.2.11",
    #     urls = [
    #         #"http://mirror.bazel.build/zlib.net/fossils/zlib-1.2.11.tar.gz",
    #         "http://zlib.net/fossils/zlib-1.2.11.tar.gz",  # 2017-01-15
    #     ],
    # )


def setup_flatbuffers():
    # Same as from https://github.com/tensorflow/io/blob/master/WORKSPACE.
    http_archive(
        name = "com_github_google_flatbuffers",
        sha256 = "12a13686cab7ffaf8ea01711b8f55e1dbd3bf059b7c46a25fefa1250bdd9dd23",
        strip_prefix = "flatbuffers-b99332efd732e6faf60bb7ce1ce5902ed65d5ba3",
        urls = [
            "https://github.com/google/flatbuffers/archive/b99332efd732e6faf60bb7ce1ce5902ed65d5ba3.tar.gz",
        ],
    )


def setup_arrow():
    # These versions were pulled from https://github.com/tensorflow/io/tree/master/WORKSPACE for testing.

    # 2020-09-23
    http_archive(
        name = "org_tensorflow_io",
        sha256 = "203f5e907a52cbee8e01bb66dc0289eec660dbd11fc6789beb68485668570c81",
        strip_prefix = "io-38e2a6840c8fac7e3082aa1d4bfc6f49ec003f8e",
        urls = [
            "https://github.com/tensorflow/io/archive/38e2a6840c8fac7e3082aa1d4bfc6f49ec003f8e.tar.gz",
        ],
    )

    http_archive(
        name = "boost",
        build_file = "@org_tensorflow_io//third_party:boost.BUILD",
        sha256 = "c66e88d5786f2ca4dbebb14e06b566fb642a1a6947ad8cc9091f9f445134143f",
        strip_prefix = "boost_1_72_0",
        urls = [
            "https://dl.bintray.com/boostorg/release/1.72.0/source/boost_1_72_0.tar.gz",
            "https://downloads.sourceforge.net/project/boost/boost/1.72.0/boost_1_72_0.tar.gz",
        ],
    )

    http_archive(
        name = "rapidjson",
        build_file = "@org_tensorflow_io//third_party:rapidjson.BUILD",
        sha256 = "30bd2c428216e50400d493b38ca33a25efb1dd65f79dfc614ab0c957a3ac2c28",
        strip_prefix = "rapidjson-418331e99f859f00bdc8306f69eba67e8693c55e",
        urls = [
            "https://github.com/miloyip/rapidjson/archive/418331e99f859f00bdc8306f69eba67e8693c55e.tar.gz",
        ],
    )

    http_archive(
        name = "zstd",
        build_file = "@org_tensorflow_io//third_party:zstd.BUILD",
        sha256 = "a364f5162c7d1a455cc915e8e3cf5f4bd8b75d09bc0f53965b0c9ca1383c52c8",
        strip_prefix = "zstd-1.4.4",
        urls = [
            "https://github.com/facebook/zstd/archive/v1.4.4.tar.gz",
        ],
    )

    http_archive(
        name = "lz4",
        build_file = "@org_tensorflow_io//third_party:lz4.BUILD",
        sha256 = "658ba6191fa44c92280d4aa2c271b0f4fbc0e34d249578dd05e50e76d0e5efcc",
        strip_prefix = "lz4-1.9.2",
        urls = [
            "https://github.com/lz4/lz4/archive/v1.9.2.tar.gz",
        ],
    )

    http_archive(
        name = "brotli",
        build_file = "@org_tensorflow_io//third_party:brotli.BUILD",
        sha256 = "4c61bfb0faca87219ea587326c467b95acb25555b53d1a421ffa3c8a9296ee2c",
        strip_prefix = "brotli-1.0.7",
        urls = [
            "https://github.com/google/brotli/archive/v1.0.7.tar.gz",
        ],
    )

    http_archive(
        name = "snappy",
        build_file = "@org_tensorflow_io//third_party:snappy.BUILD",
        sha256 = "16b677f07832a612b0836178db7f374e414f94657c138e6993cbfc5dcc58651f",
        strip_prefix = "snappy-1.1.8",
        urls = [
            "https://github.com/google/snappy/archive/1.1.8.tar.gz",
        ],
    )

    http_archive(
        name = "double-conversion",
        sha256 = "a63ecb93182134ba4293fd5f22d6e08ca417caafa244afaa751cbfddf6415b13",
        strip_prefix = "double-conversion-3.1.5",
        urls = [
            "https://github.com/google/double-conversion/archive/v3.1.5.tar.gz",
        ],
    )

    http_archive(
        name = "thrift",
        build_file = "@org_tensorflow_io//third_party:thrift.BUILD",
        sha256 = "b7452d1873c6c43a580d2b4ae38cfaf8fa098ee6dc2925bae98dce0c010b1366",
        strip_prefix = "thrift-0.12.0",
        urls = [
            "https://github.com/apache/thrift/archive/0.12.0.tar.gz",
        ],
    )

    http_archive(
        name = "arrow",
        build_file = "@org_tensorflow_io//third_party:arrow.BUILD",
        sha256 = "d7b3838758a365c8c47d55ab0df1006a70db951c6964440ba354f81f518b8d8d",
        strip_prefix = "arrow-apache-arrow-0.16.0",
        urls = [
            "https://github.com/apache/arrow/archive/apache-arrow-0.16.0.tar.gz",
        ],
    )
    # http_archive(
    #     name = "arrow",
    #     build_file = "@org_tensorflow_io//third_party:arrow.BUILD",
    #     sha256 = "1be2c945c5cf014aee1435f2240cd2bb98f5b6c6625abd7063334c7ed09047f5",
    #     strip_prefix = "arrow-8563b422bdbaf4fdc48d9691b1f8433ad65c8734",
    #     urls = [
    #         "https://github.com/apache/arrow/archive/8563b422bdbaf4fdc48d9691b1f8433ad65c8734.tar.gz",
    #     ],
    # )

    http_archive(
        name = "boringssl",
        sha256 = "1188e29000013ed6517168600fc35a010d58c5d321846d6a6dfee74e4c788b45",
        strip_prefix = "boringssl-7f634429a04abc48e2eb041c81c5235816c96514",
        urls = [
            "https://github.com/google/boringssl/archive/7f634429a04abc48e2eb041c81c5235816c96514.tar.gz",
        ],
    )

    http_archive(
        name = "xz",
        build_file = "@org_tensorflow_io//third_party:xz.BUILD",
        sha256 = "b512f3b726d3b37b6dc4c8570e137b9311e7552e8ccbab4d39d47ce5f4177145",
        strip_prefix = "xz-5.2.4",
        urls = [
            "https://tukaani.org/xz/xz-5.2.4.tar.gz",
        ],
    )

    http_archive(
        name = "bzip2",
        build_file = "@org_tensorflow_io//third_party:bzip2.BUILD",
        sha256 = "ab5a03176ee106d3f0fa90e381da478ddae405918153cca248e682cd0c4a2269",
        strip_prefix = "bzip2-1.0.8",
        urls = [
            "https://sourceware.org/pub/bzip2/bzip2-1.0.8.tar.gz",
        ],
    )

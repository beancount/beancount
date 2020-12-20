"""Protocol buffer support."""


load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")


def setup_proto():
    # Protobuf
    if not native.existing_rule("com_google_protobuf"):
        use_unreleased_version = True
        use_local_copy = True
        if use_unreleased_version:
            if use_local_copy:
                native.local_repository(
                    name = "com_google_protobuf",
                    path = "/home/blais/src/google/third_party/protobuf/testing/opensource",
                )
            else:
                http_archive(
                    name = "com_google_protobuf",
                    urls = ["file:///home/blais/src/google/third_party.tar.gz"],
                    sha256 = "cd26c9011e065b4eb95c79a74bb4f882f3b0beb6629a9c50312e387775c681c9",
                    strip_prefix = "third_party/protobuf/testing/opensource",
                    patches = [
                        ":protobuf_message_module_override.patch",
                        ":protobuf_pyext_target.patch",
                        ":protobuf_tp_print_to_vectorcall_offset.patch"],
                )
        else:
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
            sha256 = "3bce0e2fcf502619119c7cac03613fb52ce3034b2159dd3ae9d35f7339558aa3",
            strip_prefix = "rules_proto-84ba6ec814eebbf5312b2cc029256097ae0042c3",
            urls = [
                "https://github.com/bazelbuild/rules_proto/archive/84ba6ec814eebbf5312b2cc029256097ae0042c3.tar.gz",
            ],
            repo_mapping = {"@com_github_protocolbuffers_protobuf": "@com_google_protobuf",
                            "@com_google_protobuf_protoc_linux_x86_64": "@com_google_protobuf"}
        )

    # cc_proto_library
    if not native.existing_rule("rules_cc"):
        http_archive(
            name = "rules_cc",
            sha256 = "29daf0159f0cf552fcff60b49d8bcd4f08f08506d2da6e41b07058ec50cfeaec",
            strip_prefix = "rules_cc-b7fe9697c0c76ab2fd431a891dbb9a6a32ed7c3e",
            urls = ["https://github.com/bazelbuild/rules_cc/archive/b7fe9697c0c76ab2fd431a891dbb9a6a32ed7c3e.tar.gz"],
        )


def setup_upb():
    # upb, a smaller alternative to protobuf.
    if not native.existing_rule("upb"):
        if True:
            native.local_repository(
                name = "upb",
                path = "/home/blais/src/github/protocolbuffers/upb",
            )
        else:
            http_archive(
                name = "upb",
                sha256 = "602e7530c975d6a5731fff06132ae615100a28058044dd508746f1b63c0a0eae",
                strip_prefix = "upb-b10b02f66f0dfa055b676770ff394bad9f4d9df0",
                urls = ["https://github.com/protocolbuffers/upb/archive/b10b02f66f0dfa055b676770ff394bad9f4d9df0.tar.gz"],
            )

    if not native.existing_rule("upb_extras"):
        native.local_repository(
            name = "upb_extras",
            path = "/home/blais/p/upb-extras",
        )


def setup_riegeli():
    # 2020-12-08
    http_archive(
        name = "com_google_riegeli",
        sha256 = "b7ba2d3426c9dec898e39c6b79dbd4730a907f0e0978582b63f7caa560def7f0",
        strip_prefix = "riegeli-7f8553a806ed374a6d192b5a25a5f69e46f90108",
        urls = ["https://github.com/google/riegeli/archive/7f8553a806ed374a6d192b5a25a5f69e46f90108.zip"],
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
        # 2020-08-27
        http_archive(
            name = "org_brotli",
            sha256 = "fe20057c1e5c4d0b4bd318732c0bcf330b4326b486419caf1b91c351a53c5599",
            strip_prefix = "brotli-1.0.9",
            urls = ["https://github.com/google/brotli/archive/v1.0.9.zip"],
            patches = ["//third_party/proto:brotli.patch"],
            patch_args = ["-p1"],
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


def setup_flatbuffers():
    http_archive(
        name = "com_github_google_flatbuffers",
        sha256 = "62f2223fb9181d1d6338451375628975775f7522185266cd5296571ac152bc45",
        strip_prefix = "flatbuffers-1.12.0",
        urls = ["https://github.com/google/flatbuffers/archive/v1.12.0.tar.gz"],
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

    # Local archive.

    local_arrow = "/home/blais/src/github/apache/arrow"
    if False:
        native.new_local_repository(
            name = "arrow",
            path = local_arrow,
            build_file = "//third_party/proto:arrow.BUILD",
        )
    else:
        # Recent commit on master.
        http_archive(
            name = "arrow",
            build_file = "//third_party/proto:arrow.BUILD",
            sha256 = "8b231e1eda84aebe6a7ec452f0111256fee8b950db707532965cc6d295406ed1",
            strip_prefix = "arrow-66aad9db7eea4c69aa97e5c8f5a43da5db2d0d04",
            patches = ["//third_party/proto:arrow.patch"],
            patch_args = ["-p1"],
            urls = [
                "https://github.com/apache/arrow/archive/66aad9db7eea4c69aa97e5c8f5a43da5db2d0d04.tar.gz",
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

    if not native.existing_rule("org_brotli"):
        # 2020-08-27
        http_archive(
            name = "org_brotli",
            sha256 = "fe20057c1e5c4d0b4bd318732c0bcf330b4326b486419caf1b91c351a53c5599",
            strip_prefix = "brotli-1.0.9",
            urls = ["https://github.com/google/brotli/archive/v1.0.9.zip"],
            patches = ["//third_party/proto:brotli.patch"],
            patch_args = ["-p1"],
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

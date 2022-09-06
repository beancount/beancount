"""Protocol buffer support."""


load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")


def setup_proto():
    # Protobuf
    if not native.existing_rule("com_google_protobuf"):
        # 2021-06-05.
        #
        # Note: This unreleased version on github has the bleeding edge changes
        # we need in order to use fast proto casters and the native library
        # bindings.
        http_archive(
            name = "com_google_protobuf",
            urls = ["https://github.com/protocolbuffers/protobuf/archive/a1d8f8f96900468f09ced2ce9d23dea5a1bc070c.tar.gz"],
            sha256 = "a21eec7f54ca8f7579ba57ec980ee152bc00fcafb101430090b73b7adfe5aecc",
            strip_prefix = "protobuf-a1d8f8f96900468f09ced2ce9d23dea5a1bc070c",
            patches = [
                # Add a publicly visible static library target so that we can
                # link the protobuf module itself into our library.
                "//third_party/proto:protobuf_pyext_target.patch",
            ],
            patch_args = ["-p1"],
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
        if False:
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

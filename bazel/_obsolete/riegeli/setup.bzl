def beancount_riegeli():
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
        build_file = "@com_google_riegeli//bazel:highwayhash.BUILD",
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
        build_file = "@com_google_riegeli//bazel:net_zstd.BUILD",
        sha256 = "b6c537b53356a3af3ca3e621457751fa9a6ba96daf3aebb3526ae0f610863532",
        strip_prefix = "zstd-1.4.5/lib",
        urls = [
            "https://github.com/facebook/zstd/archive/v1.4.5.zip",  # 2020-05-22
        ],
    )

    http_archive(
        name = "snappy",
        build_file = "@com_google_riegeli//bazel:snappy.BUILD",
        sha256 = "38b4aabf88eb480131ed45bfb89c19ca3e2a62daeb081bdf001cfb17ec4cd303",
        strip_prefix = "snappy-1.1.8",
        urls = [
            "https://github.com/google/snappy/archive/1.1.8.zip",  # 2020-01-14
        ],
    )

    http_archive(
        name = "crc32c",
        build_file = "//bazel:crc32.BUILD",
        sha256 = "338f1d9d95753dc3cdd882dfb6e176bbb4b18353c29c411ebcb7b890f361722e",
        strip_prefix = "crc32c-1.1.0",
        urls = [
            "https://github.com/google/crc32c/archive/1.1.0.zip",  # 2019-05-24
        ],
    )

    # http_archive(
    #     name = "zlib",
    #     build_file = "//bazel:zlib.BUILD",
    #     sha256 = "c3e5e9fdd5004dcb542feda5ee4f0ff0744628baf8ed2dd5d66f8ca1197cb1a1",
    #     strip_prefix = "zlib-1.2.11",
    #     urls = [
    #         "http://zlib.net/fossils/zlib-1.2.11.tar.gz",  # 2017-01-15
    #     ],
    # )

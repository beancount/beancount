# Copied and updated from github.com/tensorflow/io/third_party:arrow.BUILD for
# more recent versions of Arrow. Bubble up changes eventually.

# Description:
#   Apache Arrow library

package(default_visibility = ["//visibility:public"])

licenses(["notice"])  # Apache 2.0

#exports_files(["LICENSE.txt"])

load("@com_github_google_flatbuffers//:build_defs.bzl", "flatbuffer_cc_library")

flatbuffer_cc_library(
    name = "arrow_format",
    srcs = [
        "cpp/src/arrow/ipc/feather.fbs",
        "format/File.fbs",
        "format/Message.fbs",
        "format/Schema.fbs",
        "format/SparseTensor.fbs",
        "format/Tensor.fbs",
    ],
    flatc_args = [
        "--scoped-enums",
        "--gen-object-api",
    ],
    out_prefix = "cpp/src/generated/",
)

genrule(
    name = "arrow_util_config",
    srcs = ["cpp/src/arrow/util/config.h.cmake"],
    outs = ["cpp/src/arrow/util/config.h"],
    cmd = ("sed " +
           "-e 's/@ARROW_VERSION_MAJOR@/0/g' " +
           "-e 's/@ARROW_VERSION_MINOR@/16/g' " +
           "-e 's/@ARROW_VERSION_PATCH@/0/g' " +
           "-e 's/cmakedefine/define/g' " +
           "$< >$@"),
)

genrule(
    name = "parquet_version_h",
    srcs = ["cpp/src/parquet/parquet_version.h.in"],
    outs = ["cpp/src/parquet/parquet_version.h"],
    cmd = ("sed " +
           "-e 's/@PARQUET_VERSION_MAJOR@/1/g' " +
           "-e 's/@PARQUET_VERSION_MINOR@/5/g' " +
           "-e 's/@PARQUET_VERSION_PATCH@/1/g' " +
           "$< >$@"),
)

cc_library(
    name = "arrow",
    srcs = glob(
        [
            "cpp/src/arrow/*.cc",
            "cpp/src/arrow/array/*.cc",
            "cpp/src/arrow/csv/*.cc",
            "cpp/src/arrow/io/*.cc",
            "cpp/src/arrow/ipc/*.cc",
            "cpp/src/arrow/json/*.cc",
            "cpp/src/arrow/util/*.cc",
            "cpp/src/arrow/tensor/*.cc",
            "cpp/src/arrow/vendored/optional.hpp",
            "cpp/src/arrow/vendored/string_view.hpp",
            "cpp/src/arrow/vendored/variant.hpp",
            "cpp/src/arrow/**/*.h",
            "cpp/src/parquet/**/*.h",
            "cpp/src/parquet/**/*.cc",
            "cpp/src/generated/parquet_types.h",
        ],
        exclude = [
            "cpp/src/arrow/util/bpacking_avx512.cc",  # Remove conditionally.
            "cpp/src/arrow/util/bpacking_avx2.cc",    # Remove conditionally.
            "cpp/src/**/*_benchmark.cc",
            "cpp/src/**/*_main.cc",
            "cpp/src/**/*_nossl.cc",
            "cpp/src/**/*_test.cc",
            "cpp/src/**/test_*.cc",
            "cpp/src/**/*hdfs*.cc",
            "cpp/src/**/*fuzz*.cc",
            "cpp/src/**/file_to_stream.cc",
            "cpp/src/**/stream_to_file.cc",
        ],
    ) + [
        "@org_tensorflow_io//third_party:parquet/parquet_types.cpp",
    ],
    hdrs = [
        # declare header from above genrule
        "cpp/src/arrow/util/config.h",
        "cpp/src/parquet/parquet_version.h",
    ],
    copts = ["-std=c++11"],
    defines = [
        "ARROW_WITH_BROTLI",
        "ARROW_WITH_SNAPPY",
        "ARROW_WITH_LZ4",
        "ARROW_WITH_ZLIB",
        "ARROW_WITH_ZSTD",
        "ARROW_WITH_BZ2",
        "ARROW_STATIC",
        "PARQUET_STATIC",
        "WIN32_LEAN_AND_MEAN",
    ],
    includes = [
        "cpp/src",
        "cpp/src/arrow/vendored/xxhash",
    ],
    textual_hdrs = [
        "cpp/src/arrow/vendored/xxhash/xxhash.c",
    ],
    deps = [
        ":arrow_format",
        "@boringssl//:crypto",
        "@brotli",
        "@bzip2",
        "@double-conversion",
        "@lz4",
        "@org_tensorflow_io//third_party:parquet",
        "@rapidjson",
        "@snappy",
        "@thrift",
        "@zlib",
        "@zstd",
    ],
)

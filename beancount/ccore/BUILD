load("//third_party/python:py_extension.bzl", "py_extension")
load("@rules_cc//cc:defs.bzl", "cc_proto_library")
load("@rules_proto//proto:defs.bzl", "proto_library")
load("@com_google_protobuf//:protobuf.bzl", "py_proto_library")

# This basically excludes //beancount/utils.
package(default_visibility = [
    "//beancount:__subpackages__",
])

cc_library(
    name = "account",
    hdrs = ["account.h"],
    srcs = ["account.cc"],
    deps = [
        "//beancount:defs",
        "@com_google_re2//:re2",
        "@com_google_absl//absl/strings:str_format",
        "@com_google_absl//absl/strings",
    ],
)

cc_test(
    name = "account_cc_test",
    srcs = ["account_test.cc"],
    deps = [
        "//beancount:defs",
        ":account",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "account_mod",
    hdrs = ["account_mod.h"],
    srcs = ["account_mod.cc"],
    deps = [
        ":account",
        "@pybind11",
    ],
)

py_test(
    name = "account_test",
    srcs = ["//beancount/core:account_test.py"],
    deps = [
        ":_core",
        "//beancount/utils:test_utils",
    ],
)

proto_library(
    name = "data_proto",
    srcs = ["data.proto"],
    deps = [
        "//experiments/v3/decimal:number_proto",
    ],
)

cc_proto_library(
    name = "data_cc_proto",
    deps = [
      ":data_proto",
    ],
)

py_proto_library(
    name = "data_py_proto",
    srcs = ["data.proto"],
    deps = [
        "//experiments/v3/decimal:number_py_proto",
    ],
)

py_extension(
    name = "_core",
    srcs = ["_core.cc"],
    deps = [
        "@local_config_python//:python_headers",
        "@pybind11",
        ":account_mod",
    ],
)

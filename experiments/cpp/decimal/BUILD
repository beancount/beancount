load("@rules_cc//cc:defs.bzl", "cc_proto_library")
load("@rules_proto//proto:defs.bzl", "proto_library")
load("@com_google_protobuf//:protobuf.bzl", "py_proto_library")
load("//bazel/python:py_extension.bzl", "py_extension")

package(default_visibility=["//visibility:public"])

cc_binary(
    name = "testdec",
    srcs = ["testdec.cc"],
    deps = [
        "@mpdecimal//:mpdecimal"
    ],
)

cc_binary(
    name = "explain_decimal",
    srcs = ["explain_decimal.cc"],
    deps = [
        "@mpdecimal//:mpdecimal"
    ],
)

cc_test(
    name = "test_serialize_decimal",
    srcs = ["test_serialize_decimal.cc"],
    deps = [
        "//beancount/ccore:number_cc_proto",
        "//beancount/ccore:number",
        "@mpdecimal//:mpdecimal",
        "@com_google_googletest//:gtest_main",
    ],
)

py_extension(
    name = "decimal_func",
    srcs = ["decimal_func.cc"],
    deps = [
        "@local_config_python//:python_headers",
        "@pybind11",
        "@mpdecimal//:mpdecimal"
    ],
)

py_test(
    name = "decimal_func_test",
    srcs = ["decimal_func_test.py"],
    deps = [
        ":decimal_func",
    ],
)

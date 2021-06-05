load("@rules_cc//cc:defs.bzl", "cc_proto_library")
load("@rules_proto//proto:defs.bzl", "proto_library")
load("@com_google_protobuf//:protobuf.bzl", "py_proto_library")

package(default_visibility=["//visibility:public"])

cc_binary(
    name = "testdec",
    srcs = ["testdec.cc"],
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

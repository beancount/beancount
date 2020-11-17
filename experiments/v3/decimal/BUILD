load("@rules_cc//cc:defs.bzl", "cc_proto_library")
load("@rules_proto//proto:defs.bzl", "proto_library")

package(default_visibility=["//visibility:public"])

proto_library(
    name = "number_proto",
    srcs = ["number.proto"],
    features = ["proto3_optional"],
)

cc_proto_library(
    name = "number_cc_proto",
    deps = [":number_proto"],
)

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
        ":number_cc_proto",
        "@mpdecimal//:mpdecimal",
        "@com_google_googletest//:gtest_main",
    ],
)

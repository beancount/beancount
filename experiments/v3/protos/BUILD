package(default_visibility=["//visibility:public"])

load("@rules_cc//cc:defs.bzl", "cc_proto_library")
load("@rules_proto//proto:defs.bzl", "proto_library")
load("@com_google_protobuf//:protobuf.bzl", "py_proto_library")

py_binary(
    name = "export_as_protos",
    srcs = ["export_as_protos.py"],
    deps = [
        "//beancount/ccore:data_py_proto",
        "//beancount:loader",
        "@com_google_riegeli//python/riegeli",
    ],
)

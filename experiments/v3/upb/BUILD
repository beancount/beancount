load("//third_party/python:py_extension.bzl", "py_extension")
load("@upb//bazel:upb_proto_library.bzl", "upb_proto_reflection_library")
load("@upb_extras//upbcpp:upbcpp_proto_library.bzl", "upbcpp_proto_library")

package(default_visibility=["//visibility:public"])

#load("//third_party/python:py_extension.bzl", "py_extension")
#load("@pybind11_bazel//:build_defs.bzl", "pybind_extension")

upb_proto_reflection_library(
    name = "data_upb_proto",
    deps = ["//beancount/ccore:data_proto"],
)

upbcpp_proto_library(
    name = "data_upbcpp_proto",
    deps = [
        #":data_upb_proto",
        "//beancount/ccore:data_proto",
    ],
)

py_extension(
    name = "extmodule",
    srcs = ["extmodule.cc"],
    deps = [
        ":data_upb_proto",
        ":data_upbcpp_proto",
        "//beancount/cparser:parser",
        "//beancount/cparser:ledger",
        "@local_config_python//:python_headers",
        "@com_google_protobuf//:protobuf",
        "@com_google_protobuf//:proto_api",
        "@pybind11",
    ],
)

py_test(
    name = "extmodule_test",
    srcs = ["extmodule_test.py"],
    deps = [
        ":extmodule",
    ],
)

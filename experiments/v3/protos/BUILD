package(default_visibility=["//visibility:public"])

load("//third_party/python:py_extension.bzl", "py_extension")
#load("@pybind11_bazel//:build_defs.bzl", "pybind_extension")

py_binary(
    name = "export_as_protos",
    srcs = ["export_as_protos.py"],
    deps = [
        "//beancount/ccore:data_py_proto",
        "//beancount/ccore:date_py_proto",
        "//beancount/ccore:number_py_proto",
        "//beancount/core:amount",
        "//beancount/core:data",
        "//beancount/core:position",
        "//beancount/cparser:extmodule",
        "//beancount/cparser:grammar",
        "//beancount/cparser:parser_py_proto",
        "//beancount/parser:printer",
        "//beancount:loader",
        "@com_google_riegeli//python/riegeli",
    ],
)

py_extension(
    name = "expose_protos",
    srcs = ["expose_protos.cc"],
    deps = [
        "//beancount/ccore:data_cc_proto",
        "//beancount/ccore:number_cc_proto",
        "@local_config_python//:python_headers",
        "@pybind11",
        "@pybind11_protobuf//:proto_casters",
    ],
)

py_test(
    name = "expose_protos_test",
    srcs = ["expose_protos_test.py"],
    deps = [
        ":expose_protos",
    ],
)

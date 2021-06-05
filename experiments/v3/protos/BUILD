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
        "//beancount/cparser:parser_py_proto",
        "//beancount/cparser:options_py_proto",
        #
        "//beancount/core:amount",
        "//beancount/core:data",
        "//beancount/core:position",
        "//beancount/cparser:extmodule",
        "//beancount/parser:printer",
        "//beancount:loader",
        "@com_google_riegeli//python/riegeli",
    ],
)

py_extension(
    name = "expose_protos",
    srcs = ["expose_protos.cc"],
    deps = [
        # Link the C++ protos. I'm not sure they should; perhaps the best way is
        # to have the Python API register them to that factory and use
        # exclusively the Python factory to create the C++ messages.
        "//beancount/ccore:data_cc_proto",
        "//beancount/ccore:date_cc_proto",
        "//beancount/ccore:number_cc_proto",
        "//beancount/cparser:parser_cc_proto",
        "//beancount/cparser:options_cc_proto",
        #
        "//beancount/cparser:parser",
        "//beancount/cparser:ledger",
        #
        "@com_google_absl//absl/time",
        #
        "@local_config_python//:python_headers",
        "@com_google_protobuf//:protobuf",
        "@com_google_protobuf//:proto_api",
        "@com_google_protobuf//:pyext",

        "@pybind11",
    ],
)

py_test(
    name = "expose_protos_test",
    srcs = ["expose_protos_test.py"],
    deps = [
        # TODO(blais): Review whether linking these is desirable or not.
        #"//beancount/ccore:data_py_proto",
        ":expose_protos",
    ],
)

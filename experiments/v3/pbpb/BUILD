# Note: pybind11_bazel is pulling its py_proto_library from grpc, not protobuf,
# but it's also behind a lot in proto version. I'm pulling it from protobuf.
# Arbitrary choice I guess. I should compare them.
load("@pybind11_bazel//:build_defs.bzl", "pybind_extension")

package(default_visibility=["//visibility:public"])

#load("//bazel/python:py_extension.bzl", "py_extension")
#load("@pybind11_bazel//:build_defs.bzl", "pybind_extension")

# py_binary(
#     name = "export_as_protos",
#     srcs = ["export_as_protos.py"],
#     deps = [
#         "//beancount/ccore:data_py_proto",
#         "//beancount/ccore:date_py_proto",
#         "//beancount/ccore:number_py_proto",
#         "//beancount/cparser:inter_py_proto",
#         "//beancount/cparser:options_py_proto",
#         #
#         "//beancount/core:amount",
#         "//beancount/core:data",
#         "//beancount/core:position",
#         "//beancount/parser:printer",
#         "//beancount:loader",
#         #
#         #"//beancount/cparser:extmodule",
#         ":pbpb_protos"
#    ],
# )

pybind_extension(
    name = "pbpb_protos",
    srcs = ["pbpb_protos.cc"],
    deps = [
        # Link the C++ protos. I'm not sure they should; perhaps the best way is
        # to have the Python API register them to that factory and use
        # exclusively the Python factory to create the C++ messages.
        "//beancount/ccore:data_cc_proto",
        "//beancount/ccore:date_cc_proto",
        "//beancount/ccore:number_cc_proto",
        "//beancount/cparser:inter_cc_proto",
        "//beancount/cparser:options_cc_proto",
        #
        "//beancount/cparser:parser",
        "//beancount/cparser:ledger",
        #
        "@com_google_absl//absl/time",
        #
        "@pybind11_protobuf//pybind11_protobuf:native_proto_caster",
        "@com_google_protobuf//:protobuf",
        # "@local_config_python//:python_headers",
        # "@pybind11",
    ],
)

py_test(
    name = "pbpb_protos_test",
    srcs = ["pbpb_protos_test.py"],
    data = [
        ":pbpb_protos.so",
        "//examples:example"
    ],
    python_version = "PY3",
    srcs_version = "PY3",
    deps = [
        "@com_google_absl_py//absl/testing:absltest",
        "@com_google_protobuf//:protobuf_python",
        #"//beancount/ccore:data_py_proto",
    ],
)

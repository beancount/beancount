package(default_visibility=["//visibility:public"])

load("@com_google_protobuf//:protobuf.bzl", "py_proto_library")

config_setting(
    name = "use_fast_cpp_protos",
    values = {
        "define": "use_fast_cpp_protos=true",
    },
)

proto_library(
    name = "beancount_proto",
    srcs = ["beancount.proto"],
    deps = [
      ## "@com_google_protobuf//src/google/protobuf/any.proto",
    ],
)

cc_proto_library(
    name = "beancount_cc_proto",
    deps = [
      ":beancount_proto",
      # "@com_google_protobuf//:well_known_protos",
    ],
    # imports = [
    #     "external/com_google_protobuf/src/",
    # ],
)


py_proto_library(
    name = "beancount_py_proto",
    #deps = [":beancount_proto"],
    srcs = ["beancount.proto"],
)

py_binary(
    name = "export_as_protos",
    srcs = ["export_as_protos.py"],
    deps = [
        ":beancount_py_proto",
        "@com_google_riegeli//python/riegeli",
    ],
)

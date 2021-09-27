load("//third_party/python:py_extension.bzl", "py_extension")
load("@rules_cc//cc:defs.bzl", "cc_proto_library")
load("@rules_proto//proto:defs.bzl", "proto_library")
load("@com_google_protobuf//:protobuf.bzl", "py_proto_library")

# This basically excludes //beancount/utils.
package(default_visibility = [
    "//beancount:__subpackages__",
    "//experiments/v3:__subpackages__",
])

cc_library(
    name = "std_utils",
    hdrs = ["std_utils.h"],
    srcs = ["std_utils.cc"],
    deps = [
        "@com_google_absl//absl/strings",
    ],
)

cc_test(
    name = "std_utils_test",
    srcs = ["std_utils_test.cc"],
    deps = [
        ":std_utils",
        "@com_google_googletest//:gtest_main",
    ],
)

#----------------------------------------

proto_library(
    name = "number_proto",
    srcs = ["number.proto"],
    features = ["proto3_optional"],
)

cc_proto_library(
    name = "number_cc_proto",
    deps = [":number_proto"],
)

py_proto_library(
    name = "number_py_proto",
    srcs = ["number.proto"],
)

cc_library(
    name = "number",
    hdrs = ["number.h"],
    srcs = ["number.cc"],
    deps = [
        "@com_google_absl//absl/hash",
        "@mpdecimal//:mpdecimal",
        "//beancount/ccore:number_cc_proto",
    ],
)

cc_test(
    name = "number_cc_test",
    srcs = ["number_test.cc"],
    deps = [
        ":number",
        "@com_google_googletest//:gtest_main",
    ],
)

#----------------------------------------

proto_library(
    name = "date_proto",
    srcs = ["date.proto"],
)

cc_proto_library(
    name = "date_cc_proto",
    deps = [":date_proto"],
)

py_proto_library(
    name = "date_py_proto",
    srcs = ["date.proto"],
)

cc_library(
    name = "date",
    srcs = ["date.cc"],
    hdrs = ["date.h"],
    deps = [
        "@com_google_absl//absl/time",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/status:statusor",
        "//beancount/ccore:date_cc_proto",
    ],
)

cc_test(
    name = "date_test",
    srcs = ["date_test.cc"],
    deps = [
        ":date",
        "@com_google_googletest//:gtest_main",
    ],
)

#----------------------------------------

proto_library(
    name = "data_proto",
    srcs = ["data.proto"],
    deps = [
        "//beancount/ccore:number_proto",
        "//beancount/ccore:date_proto",
        "//beancount/cparser:parser_proto",
        "//beancount/cparser:options_proto",
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
        "//beancount/ccore:number_py_proto",
        "//beancount/ccore:date_py_proto",
        "//beancount/cparser:inter_py_proto",
        "//beancount/cparser:options_py_proto",
    ],
)


cc_library(
    name = "data",
    hdrs = ["data.h"],
    srcs = ["data.cc"],
    deps = [
        ":number",
        ":number_cc_proto",
        ":data_cc_proto",
        #"@com_google_absl//absl/hash",
        "@com_google_protobuf//:protobuf",
    ],
)

cc_library(
    name = "datapy",
    hdrs = ["datapy.h"],
    srcs = ["datapy.cc"],
    deps = [
        "//beancount/ccore:data_cc_proto",
        "//beancount/cparser:options_cc_proto",
        "@local_config_python//:python_headers",
        "@pybind11",
    ],
)

#----------------------------------------

cc_library(
    name = "currency",
    hdrs = ["currency.h"],
    srcs = ["currency.cc"],
    deps = [
        "@com_google_absl//absl/strings",
        "@com_google_re2//:re2",
    ],
)

# TODO(blais):
# cc_test(
#     name = "currency_cc_test",
#     srcs = ["currency_test.cc"],
#     deps = [
#         ":currency",
#         "@com_google_googletest//:gtest_main",
#     ],
# )


#----------------------------------------

cc_library(
    name = "account",
    hdrs = ["account.h"],
    srcs = ["account.cc"],
    deps = [
        "//beancount/ccore:data_cc_proto",
        "@com_google_re2//:re2",
        "@com_google_absl//absl/strings:str_format",
        "@com_google_absl//absl/strings",
    ],
)

cc_test(
    name = "account_cc_test",
    srcs = ["account_test.cc"],
    deps = [
        ":account",
        "@com_google_googletest//:gtest_main",
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

#----------------------------------------

cc_library(
    name = "account_types",
    hdrs = ["account_types.h"],
    srcs = ["account_types.cc"],
    deps = [
        ":account",
        "@com_google_absl//absl/strings",
    ],
)

py_test(
    name = "account_types_test",
    srcs = ["//beancount/core:account_types_test.py"],
    deps = [
        ":_core",
        "//beancount/utils:test_utils",
    ],
)

#----------------------------------------

cc_library(
    name = "inventory",
    hdrs = ["inventory.h"],
    srcs = ["inventory.cc"],
    deps = [
        ":std_utils",
        ":number",
        ":data",
        ":data_cc_proto",
        ":account",
        "@com_google_absl//absl/strings",
    ],
)

py_test(
    name = "inventory_test",
    srcs = ["//beancount/core:inventory_test.py"],
    deps = [
        ":_core",
        "//beancount/core:number",
        "//beancount/core:amount",
        "//beancount/core:position",
        "//beancount/core:convert",
        "//beancount/utils:invariants",
    ],
)

#----------------------------------------

proto_library(
    name = "precision_proto",
    srcs = ["precision.proto"],
    features = ["proto3_optional"],
)

cc_proto_library(
    name = "precision_cc_proto",
    deps = [":precision_proto"],
)

py_proto_library(
    name = "precision_py_proto",
    srcs = ["precision.proto"],
)

cc_library(
    name = "precision",
    hdrs = ["precision.h"],
    srcs = ["precision.cc"],
    deps = [
        ":precision_cc_proto",
        ":data_cc_proto",
        ":number",
        ":currency",
        "@com_google_absl//absl/hash",
        "@com_google_absl//absl/strings",
        "@mpdecimal//:mpdecimal",
    ],
)

cc_test(
    name = "precision_test",
    srcs = ["precision_test.cc"],
    deps = [
        ":precision",
        ":precision_cc_proto",
        "//beancount/cparser:test_utils",
        "@com_google_googletest//:gtest_main",
        "@com_google_protobuf//:protobuf",
        "@com_google_re2//:re2",
    ],
)

#----------------------------------------

py_extension(
    name = "_core",
    srcs = ["_core.cc"],
    deps = [
        ":number",
        ":data",
        ":account",
        ":account_types",
        ":inventory",
        "@local_config_python//:python_headers",
        "@pybind11",
        "@mpdecimal//:mpdecimal",
    ],
)

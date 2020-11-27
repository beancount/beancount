load("//third_party/python:py_extension.bzl", "py_extension")
#load("@pybind11_bazel//:build_defs.bzl", "pybind_extension")
load("//third_party/parser:bison.bzl", "genyacc")
load("//third_party/parser:reflex.bzl", "genreflex")

package(default_visibility = ["//visibility:public"])

#-------------------------------------------------------------------------------
# Scanner and grammar rules.

genreflex(
    name = "scanner",
    src = "scanner.lxx",
    out = "scanner.cc",
    # Set this to true to debug the scanner.
    debug = False,
    header_out = "scanner.h",
    includes = [
        "parser.h",
    ],
)

genyacc(
    name = "parser_generated",
    src = "parser.yxx",
    location_out = "location.h",
    header_out = "parser.h",
    source_out = "parser.cc",
)

cc_library(
    name = "parser_headers",
    hdrs = [
        "scanner.h",
        "parser.h",
        "location.h",
    ],
)

cc_library(
    name = "parser",
    srcs = [
        "scanner.cc",
        "parser.cc",
    ],
    copts = [
        "-Wno-implicit-fallthrough",
        "-O2",
        "-Wno-sign-compare",
    ],
    # Generated and real files are interdependent.
    tags = ["nofixdeps"],
    deps = [
        ":parser_cc_proto",
        "//beancount/ccore:number",
        "//beancount/ccore:data_cc_proto",
        "//beancount/parser:tokens_c",
        "//third_party/parser:reflex",
        ":parser_headers",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/time",
        "@mpdecimal//:mpdecimal",
    ],
)

# cc_test(
#     name = "scanner_test",
#     srcs = ["scanner_test.cc"],
#     deps = [
#         # ":test_utils",
#         ":parser",
#         "@com_google_googletest//:gtest_main",
#     ],
# )

# cc_test(
#     name = "parser_test",
#     srcs = ["parser_test.cc"],
#     deps = [
#         # ":test_utils",
#         # ":data_cc_pb2",
#         # ":compile",
#         ":parser",
#         "@com_google_googletest//:gtest_main",
#         "@com_google_absl//absl/strings",
#     ],
# )

# TODO(blais): Our goal in rewriting is to remove this by chipping away at it.
py_library(
    name = "grammar",
    srcs = ["grammar.py"],
    deps = [
        "//beancount/parser:lexer",
        "//beancount/parser:options",
        "//beancount/core:number",
        "//beancount/core:display_context",
        "//beancount/core:data",
        "//beancount/core:position",
        "//beancount/core:account",
    ],
)

#-------------------------------------------------------------------------------
# Intermediate representation.

proto_library(
    name = "parser_proto",
    srcs = ["parser.proto"],
    deps = [
        "//beancount/ccore:number_proto",
        "//beancount/ccore:data_proto",
    ],
)

cc_proto_library(
    name = "parser_cc_proto",
    deps = [
      ":parser_proto",
    ],
)

proto_library(
    name = "options_proto",
    srcs = ["options.proto"],
    deps = [
        "//beancount/ccore:data_proto",
    ],
)

cc_proto_library(
    name = "options_cc_proto",
    deps = [
      ":options_proto",
    ],
)

#-------------------------------------------------------------------------------
# Extension module.

# TODO(blais): Let the pybind11 authors know py_extension works better.
# pybind_extension(
#     name = "extmodule",
#     srcs = ["extmodule.cc"],
#     # copts = ["-DPYTHON_EXECUTABLE=/usr/bin/python3"],
#     deps = [
#         ":parser",
#     ],
# )

py_extension(
    name = "extmodule",
    srcs = ["extmodule.cc"],
    deps = [
        ":parser_cc_proto",
        ":parser",
        "//beancount:defs",
        "//beancount/parser:tokens_c",
        "@local_config_python//:python_headers",
        "@pybind11",
        #"@com_google_absl//absl/strings",
    ],
)

py_test(
    name = "extmodule_test",
    srcs = ["extmodule_test.py"],
    deps = [
        ":extmodule",
        "//beancount/cparser:grammar",
        "//beancount/parser:printer",
        "//beancount/core:number",
    ],
)

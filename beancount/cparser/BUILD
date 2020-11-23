#load("//third_party/python:py_extension.bzl", "py_extension")
load("//third_party/parser:bison.bzl", "genyacc")
load("//third_party/parser:reflex.bzl", "genreflex")

package(default_visibility = ["//visibility:public"])

#-------------------------------------------------------------------------------
# Scanner and grammar rules.

genreflex(
    name = "example_scanner_generated",
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
        "//beancount/parser:tokens_c",
        "//third_party/parser:reflex",
        # ":data_cc_pb2",
        # ":data",
        ":parser_headers",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/strings",
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

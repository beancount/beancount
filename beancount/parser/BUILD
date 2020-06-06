load("//third_party/python:py_extension.bzl", "py_extension")

package(default_visibility = ["//visibility:public"])

py_library(
    name = "booking_full",
    srcs = ["booking_full.py"],
    deps = [
        ":booking_method",
        "//beancount/core:number",
        "//beancount/core:data",
        "//beancount/core:amount",
        "//beancount/core:position",
        "//beancount/core:inventory",
        "//beancount/core:interpolate",
    ],
)

py_test(
    name = "booking_full_test",
    srcs = ["booking_full_test.py"],
    deps = [
        "//beancount/core:number",
        "//beancount/core:amount",
        "//beancount/core:position",
        "//beancount/core:inventory",
        "//beancount/core:data",
        "//beancount/core:interpolate",
        "//beancount/utils:test_utils",
        "//beancount/ops:documents",
        "//beancount/ops:balance",
        "//beancount/ops:pad",
        ":parser",
        ":printer",
        ":booking_full",
        ":booking_method",
        ":booking_test_lib",
        ":options",
        ":cmptest",
        "//beancount:loader",
    ],
)

py_library(
    name = "booking_method",
    srcs = ["booking_method.py"],
    deps = [
        "//beancount/core:number",
        "//beancount/core:data",
        "//beancount/core:amount",
        "//beancount/core:position",
        "//beancount/core:flags",
        "//beancount/core:inventory",
        "//beancount/core:convert",
    ],
)

py_test(
    name = "booking_method_test",
    srcs = ["booking_method_test.py"],
    deps = [
        ":booking_method",
    ],
)

py_library(
    name = "booking",
    srcs = ["booking.py"],
    deps = [
        ":booking_full",
        "//beancount/core:number",
        "//beancount/core:data",
        "//beancount/core:inventory",
    ],
)

py_test(
    name = "booking_test",
    srcs = ["booking_test.py"],
    deps = [
        ":booking_test_lib",
    ],
)

# TODO(blais): Remove this target by removing the dependency.
py_library(
    name = "booking_test_lib",
    srcs = ["booking_test.py"],
    deps = [
        "//beancount/core:number",
        "//beancount/core:amount",
        "//beancount/core:data",
        "//beancount/core:position",
        ":parser",
        ":cmptest",
        ":booking",
    ],
)

py_library(
    name = "cmptest",
    srcs = ["cmptest.py"],
    deps = [
        "//beancount/core:amount",
        "//beancount/core:compare",
        "//beancount/core:data",
        "//beancount/core:position",
        "//beancount/core:number",
        "//beancount/utils:test_utils",
        ":booking",
        ":parser",
        ":printer",
    ],
)

py_test(
    name = "cmptest_test",
    srcs = ["cmptest_test.py"],
    deps = [
        "//beancount/core:amount",
        "//beancount/core:data",
        "//beancount/core:position",
        "//beancount/core:number",
        ":booking",
        ":cmptest",
        ":parser",
    ],
)

cc_library(
    name = "grammar_c",
    srcs = ["grammar.c"],
    deps = [
        ":parser_hdr",
        ":lexer_hdr",
        "@local_config_python//:python_headers",
    ],
)

cc_library(
    name = "grammar_hdr",
    hdrs = ["grammar.h"],
    deps = [
        "@local_config_python//:python_headers",
    ],
)

py_library(
    name = "grammar",
    srcs = ["grammar.py"],
    deps = [
        ":lexer",
        ":options",
        "//beancount/core:number",
        "//beancount/core:display_context",
        "//beancount/core:data",
        "//beancount/core:position",
        "//beancount/core:account",
    ],
)

py_test(
    name = "grammar_test",
    srcs = ["grammar_test.py"],
    deps = [
        "//beancount/core:number",
        "//beancount/core:amount",
        "//beancount/core:position",
        "//beancount/core:data",
        "//beancount/utils:test_utils",
        "//beancount/parser:cmptest",
        ":parser",
        ":lexer",
    ],
)

filegroup(
    name = "parser_source_files",
    srcs = [
        "lexer.l",
        "grammar.y",
        "parser.h",
        "parser.c",
    ],
)

genrule(
    name = "parser_source_hash",
    srcs = [],
    tools = ["//beancount/parser:hashsrc"],
    cmd = "$(location //beancount/parser:hashsrc) > $@",
    outs = ["parser_source_hash.h"],
)

py_library(
    name = "hashsrc_lib",
    srcs = ["hashsrc.py"],
    data = [":parser_source_files"],
    deps = [
        "//beancount/core:number",
    ],
)

py_binary(
    name = "hashsrc",
    srcs = ["hashsrc.py"],
    deps = [
        ":hashsrc_lib",
    ],
)

py_test(
    name = "hashsrc_test",
    srcs = ["hashsrc_test.py"],
    deps = [
        ":hashsrc_lib",
        ":_parser",
    ],
)

cc_library(
    name = "lexer_c",
    hdrs = ["lexer.h"],
    srcs = ["lexer.c"],
    deps = [
        ":parser_hdr",
        ":grammar_hdr",
        "@local_config_python//:python_headers",
    ],
)

cc_library(
    name = "lexer_hdr",
    hdrs = ["lexer.h"],
    deps = [
        ":parser_hdr",
        ":grammar_hdr",
        "@local_config_python//:python_headers",
    ],
)

# lexer.l
# lexer.patch

py_library(
    name = "lexer",
    srcs = ["lexer.py"],
    deps = [
        ":_parser",
        "//beancount/core:data",
        "//beancount/core:account",
        "//beancount/core:number",
    ],
)

py_test(
    name = "lexer_test",
    srcs = ["lexer_test.py"],
    deps = [
        ":lexer",
        "//beancount/core:number",
    ],
)

py_library(
    name = "options",
    srcs = ["options.py"],
    deps = [
        "//beancount/core:number",
        "//beancount/core:data",
        "//beancount/core:display_context",
        "//beancount/core:account",
        "//beancount/core:account_types",
    ],
)

py_test(
    name = "options_test",
    srcs = ["options_test.py"],
    deps = [
        ":options",
        ":parser",
        "//beancount/core:account_types",
    ],
)

# TODO(blais): Compute VC_TIMESTAMP properly. See tools/*.bzl
py_extension(
    name = "_parser",
    srcs = [
        "parser.c",
        ":parser_source_hash"
    ],
    # TODO(blais): Compute this value properly.
    local_defines = ["VC_CHANGESET", "VC_TIMESTAMP", "RELEASE_VERSION"],
    deps = [
        ":grammar_c",
        ":lexer_c",
        "@local_config_python//:python_headers",
    ],
)

cc_library(
    name = "parser_hdr",
    hdrs = ["parser.h"],
    deps = [
        "@local_config_python//:python_headers",
    ],
)

py_library(
    name = "parser",
    srcs = ["parser.py"],
    deps = [
        ":_parser",
        ":grammar",
        ":printer",
        ":hashsrc_lib",
        "//beancount/core:data",
        "//beancount/core:number",
    ],
)

py_test(
    name = "parser_test",
    srcs = ["parser_test.py"],
    deps = [
        ":parser",
        "//beancount/core:data",
        "//beancount/core:number",
        "//beancount/utils:test_utils",
    ],
)

py_library(
    name = "printer",
    srcs = ["printer.py"],
    deps = [
        "//beancount/core:number",
        "//beancount/core:position",
        "//beancount/core:convert",
        "//beancount/core:inventory",
        "//beancount/core:amount",
        "//beancount/core:account",
        "//beancount/core:data",
        "//beancount/core:display_context",
        "//beancount/core:interpolate",
        "//beancount/utils:misc_utils",
    ],
)

py_test(
    name = "printer_test",
    srcs = ["printer_test.py"],
    deps = [
        ":printer",
        ":cmptest",
        "//beancount/core:data",
        "//beancount/core:interpolate",
        "//beancount/ops:pad",
        "//beancount/ops:balance",
        "//beancount/ops:documents",
        "//beancount/utils:test_utils",
        "//beancount:loader",
    ],
)

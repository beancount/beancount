package(default_visibility = ["//visibility:public"])

py_library(
    name = "balance",
    srcs = ["balance.py"],
    deps = [
        "//beancount/core:number",
        "//beancount/core:data",
        "//beancount/core:amount",
        "//beancount/core:account",
        "//beancount/core:inventory",
        "//beancount/core:realization",
        "//beancount/core:getters",
    ],
)

py_test(
    name = "balance_test",
    srcs = ["balance_test.py"],
    deps = [
        "//beancount/core:number",
        "//beancount/core:amount",
        "//beancount:loader",
        "//beancount:plugins_for_tests",
    ],
)

py_library(
    name = "basicops",
    srcs = ["basicops.py"],
    deps = [
        "//beancount/core:data",
    ],
)

py_test(
    name = "basicops_test",
    srcs = ["basicops_test.py"],
    deps = [
        "//beancount/core:data",
        "//beancount:loader",
        ":basicops",
        "//beancount:plugins_for_tests",
    ],
)

py_library(
    name = "compress",
    srcs = ["compress.py"],
    deps = [
        "//beancount/core:number",
        "//beancount/core:amount",
        "//beancount/core:data",
    ],
)

py_test(
    name = "compress_test",
    srcs = ["compress_test.py"],
    deps = [
        "//beancount:loader",
        "//beancount/parser:cmptest",
        "//beancount/core:data",
        ":compress",
        "//beancount:plugins_for_tests",
    ],
)

py_library(
    name = "documents",
    srcs = ["documents.py"],
    deps = [
        "//beancount/core:account",
        "//beancount/core:data",
        "//beancount/core:getters",
    ],
)

py_test(
    name = "documents_test",
    srcs = ["documents_test.py"],
    deps = [
        "//beancount/utils:test_utils",
        "//beancount/core:data",
        "//beancount/parser:cmptest",
        ":documents",
        "//beancount:loader",
    ],
)

py_library(
    name = "holdings",
    srcs = ["holdings.py"],
    deps = [
        "//beancount/core:number",
        "//beancount/core:account",
        "//beancount/core:amount",
        "//beancount/core:position",
        "//beancount/core:realization",
        "//beancount/core:account_types",
        "//beancount/core:data",
        "//beancount/core:flags",
        "//beancount/core:getters",
        "//beancount/core:prices",
        "//beancount/ops:summarize",
        "//beancount/utils:misc_utils",
    ],
)

py_test(
    name = "holdings_test",
    srcs = ["holdings_test.py"],
    deps = [
        "//beancount/core:number",
        "//beancount/core:position",
        "//beancount/core:data",
        "//beancount/core:prices",
        ":holdings",
        "//beancount:loader",
        "//beancount:plugins_for_tests",
        "//beancount/plugins:unrealized",
    ],
)

py_library(
    name = "lifetimes",
    srcs = ["lifetimes.py"],
    deps = [
        "//beancount/core:inventory",
        "//beancount/core:data",
    ],
)

py_test(
    name = "lifetimes_test",
    srcs = ["lifetimes_test.py"],
    deps = [
        ":lifetimes",
        "//beancount:loader",
        "//beancount:plugins_for_tests",
        "//beancount/utils:test_utils",
    ],
)

py_library(
    name = "pad",
    srcs = ["pad.py"],
    deps = [
        "//beancount/core:account",
        "//beancount/core:amount",
        "//beancount/core:inventory",
        "//beancount/core:data",
        "//beancount/core:position",
        "//beancount/core:flags",
        "//beancount/core:realization",
        "//beancount/utils:misc_utils",
        ":balance",
    ],
)

py_test(
    name = "pad_test",
    srcs = ["pad_test.py"],
    deps = [
        "//beancount/core:amount",
        "//beancount/core:inventory",
        "//beancount/core:data",
        "//beancount/core:realization",
        ":pad",
        "//beancount/ops:balance",
        "//beancount/parser:cmptest",
        "//beancount:loader",
        "//beancount:plugins_for_tests",
    ],
)

py_library(
    name = "summarize",
    srcs = ["summarize.py"],
    deps = [
        "//beancount/core:number",
        "//beancount/core:data",
        "//beancount/core:account_types",
        "//beancount/core:amount",
        "//beancount/core:inventory",
        "//beancount/core:flags",
        "//beancount/core:interpolate",
        "//beancount/core:convert",
        "//beancount/core:prices",
        ":balance",
        "//beancount/utils:bisect_key",
        "//beancount/parser:options",
    ],
)

py_test(
    name = "summarize_test",
    srcs = ["summarize_test.py"],
    deps = [
        "//beancount/core:inventory",
        "//beancount/core:data",
        "//beancount/core:flags",
        "//beancount/core:interpolate",
        "//beancount/core:convert",
        ":summarize",
        "//beancount/parser:printer",
        "//beancount/parser:options",
        "//beancount/parser:cmptest",
        "//beancount/utils:misc_utils",
        "//beancount:loader",
        "//beancount:plugins_for_tests",
    ],
)

py_library(
    name = "validation",
    srcs = ["validation.py"],
    deps = [
        "//beancount/core:data",
        "//beancount/core:getters",
        "//beancount/core:interpolate",
        "//beancount/utils:misc_utils",
    ],
)

py_test(
    name = "validation_test",
    srcs = ["validation_test.py"],
    deps = [
        "//beancount/core:data",
        "//beancount/parser:cmptest",
        ":validation",
        "//beancount:loader",
        "//beancount:plugins_for_tests",
    ],
)

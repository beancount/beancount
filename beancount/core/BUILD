# This basically excludes //beancount/utils.
package(default_visibility = [
    "//beancount:__pkg__",
    "//beancount/ingest:__subpackages__",
    "//beancount/ops:__pkg__",
    "//beancount/parser:__pkg__",
    "//beancount/plugins:__pkg__",
    "//beancount/prices:__subpackages__",
    "//beancount/projects:__pkg__",
    "//beancount/scripts:__pkg__",
    "//examples:__subpackages__",
    "//experiments/plugins:__pkg__",
    "//experiments/sharing:__pkg__",
    "//experiments/v3/protos:__pkg__",
    "//experiments/sql:__pkg__",

    # Temporary access from C core and parser.
    "//beancount/ccore:__pkg__",
    "//beancount/cparser:__pkg__",
])

py_library(
    name = "account",
    srcs = ["account.py"],
)

py_test(
    name = "account_test",
    srcs = ["account_test.py"],
    deps = [
        ":account",
        "//beancount/utils:test_utils",
    ],
)

py_library(
    name = "account_types",
    srcs = ["account_types.py"],
    deps = [
        ":account",
    ],
)

py_test(
    name = "account_types_test",
    srcs = ["account_types_test.py"],
    deps = [
        ":account_types",
    ],
)

py_library(
    name = "amount",
    srcs = ["amount.py"],
    deps = [
        ":number",
        ":display_context",
    ],
)

py_test(
    name = "amount_test",
    srcs = ["amount_test.py"],
    deps = [
        ":amount",
    ],
)

py_library(
    name = "compare",
    srcs = ["compare.py"],
    deps = [
        ":data",
    ],
)

py_test(
    name = "compare_test",
    srcs = ["compare_test.py"],
    deps = [
        ":data",
        ":compare",
        "//beancount:loader",
    ],
)

py_library(
    name = "convert",
    srcs = ["convert.py"],
    deps = [
        ":number",
        ":amount",
        ":position",
        ":prices",
    ],
)

py_test(
    name = "convert_test",
    srcs = ["convert_test.py"],
    deps = [
        ":number",
        ":amount",
        ":data",
        ":position",
        ":convert",
        ":inventory",
        ":prices",
        "//beancount:loader",
        "//beancount/ops:pad",
        "//beancount/ops:documents",
        "//beancount/ops:balance",
    ],
)

py_library(
    name = "data",
    srcs = ["data.py"],
    deps = [
        ":number",
        ":amount",
        ":position",
        ":account",
        "//beancount/utils:bisect_key",
    ],
)

py_test(
    name = "data_test",
    srcs = ["data_test.py"],
    deps = [
        ":data",
        ":amount",
    ],
)

py_library(
    name = "display_context",
    srcs = ["display_context.py"],
    deps = [
        ":number",
        ":distribution",
    ],
)

py_test(
    name = "display_context_test",
    srcs = ["display_context_test.py"],
    deps = [
        ":display_context",
        ":number",
    ],
)

py_library(
    name = "distribution",
    srcs = ["distribution.py"],
    deps = [
    ],
)

py_test(
    name = "distribution_test",
    srcs = ["distribution_test.py"],
    deps = [
        ":distribution",
    ],
)

py_library(
    name = "flags",
    srcs = ["flags.py"],
)

py_test(
    name = "flags_test",
    srcs = ["flags_test.py"],
    deps = [
        ":flags",
    ],
)

py_library(
    name = "getters",
    srcs = ["getters.py"],
    deps = [
        ":data",
        ":account",
    ],
)

py_test(
    name = "getters_test",
    srcs = ["getters_test.py"],
    deps = [
        ":getters",
        ":data",
        "//beancount:loader",
    ],
)


py_library(
    name = "interpolate",
    srcs = ["interpolate.py"],
    deps = [
        ":number",
        ":amount",
        ":position",
        ":inventory",
        ":convert",
        ":data",
        ":getters",
        "//beancount/utils:defdict",
    ],
)

py_test(
    name = "interpolate_test",
    srcs = ["interpolate_test.py"],
    deps = [
        ":number",
        ":amount",
        ":data",
        ":interpolate",
        ":inventory",
        ":position",
        ":convert",
        "//beancount/parser:parser",
        "//beancount/parser:cmptest",
        "//beancount/utils:defdict",
        "//beancount:loader",
        "//beancount/ops:pad",
        "//beancount/ops:documents",
        "//beancount/ops:balance",
    ],
)


py_library(
    name = "inventory",
    srcs = ["inventory.py"],
    deps = [
        ":number",
        ":amount",
        ":position",
        ":convert",
        ":display_context",
    ],
)

py_test(
    name = "inventory_test",
    srcs = ["inventory_test.py"],
    deps = [
        ":number",
        ":amount",
        ":position",
        ":inventory",
        ":convert",
        "//beancount/utils:invariants",
    ],
)

py_library(
    name = "number",
    srcs = ["number.py"],
)

py_test(
    name = "number_test",
    srcs = ["number_test.py"],
    deps = [
        ":number",
    ],
)


py_library(
    name = "position",
    srcs = ["position.py"],
    deps = [
        ":number",
        ":display_context",
    ],
)

py_test(
    name = "position_test",
    srcs = ["position_test.py"],
    deps = [
        ":position",
        ":convert",
        ":number",
        ":amount",
        ":display_context",
    ],
)

py_library(
    name = "prices",
    srcs = ["prices.py"],
    deps = [
        ":number",
        ":data",
        "//beancount/utils:misc_utils",
        "//beancount/utils:bisect_key",
    ],
)

py_test(
    name = "prices_test",
    srcs = ["prices_test.py"],
    deps = [
        ":number",
        ":prices",
        "//beancount/parser:cmptest",
        "//beancount:loader",
        "//beancount/ops:pad",
        "//beancount/ops:documents",
        "//beancount/ops:balance",
    ],
)

py_library(
    name = "realization",
    srcs = ["realization.py"],
    deps = [
        ":inventory",
        ":amount",
        ":data",
        ":account",
        ":flags",
        ":convert",
    ],
)

py_test(
    name = "realization_test",
    srcs = ["realization_test.py"],
    deps = [
        ":number",
        ":amount",
        ":realization",
        ":data",
        ":inventory",
        ":position",
        ":account_types",
        ":display_context",
        "//beancount/utils:test_utils",
        "//beancount:loader",
        "//beancount/ops:pad",
        "//beancount/ops:documents",
        "//beancount/ops:balance",
    ],
)

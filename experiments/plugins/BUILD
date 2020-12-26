package(default_visibility = ["//visibility:public"])

py_library(
    name = "all_plugins",
    srcs = [
        ":__init__.py",
    ],
    deps = [
        ":book_conversions",
        ":divert_expenses",
        ":exclude_tag",
        ":fill_account",
        ":fix_payees",
        ":forecast",
        ":ira_contribs",
        ":mark_unverified",
        ":merge_meta",
        ":tag_pending",
        #":unrealized",
    ],
)

py_library(
    name = "book_conversions",
    srcs = ["book_conversions.py"],
    deps = [
        "//beancount/core:number",
        "//beancount/core:amount",
        "//beancount/core:position",
        "//beancount/core:inventory",
        "//beancount/core:account",
        "//beancount/core:data",
        "//beancount/utils:table",
        "//beancount/parser:version",
        "//beancount:loader",
    ],
)

py_binary(
    name = "book_conversions_main",
    srcs = ["book_conversions.py"],
    main = "book_conversions.py",
    deps = [
        ":book_conversions",
    ],
)

py_test(
    name = "book_conversions_test",
    srcs = ["book_conversions_test.py"],
    deps = [
        "//beancount/parser:cmptest",
        ":book_conversions",
        "//beancount:loader",
        "//beancount/utils:test_utils",
        "//beancount:plugins_for_tests",
    ],
)

py_library(
    name = "divert_expenses",
    srcs = ["divert_expenses.py"],
    deps = [
        "//beancount/core:account_types",
        "//beancount/core:data",
        "//beancount/parser:options",
    ],
)

py_test(
    name = "divert_expenses_test",
    srcs = ["divert_expenses_test.py"],
    deps = [
        ":divert_expenses",
        "//beancount:loader",
        "//beancount:plugins_for_tests",
        "//beancount/parser:cmptest",
    ],
)

py_library(
    name = "exclude_tag",
    srcs = ["exclude_tag.py"],
    deps = [
        "//beancount/core:data",
    ],
)

py_test(
    name = "exclude_tag_test",
    srcs = ["exclude_tag_test.py"],
    deps = [
        ":exclude_tag",
        "//beancount:loader",
        "//beancount/parser:cmptest",
        "//beancount:plugins_for_tests",
    ],
)

py_library(
    name = "fill_account",
    srcs = ["fill_account.py"],
    deps = [
        "//beancount/core:account",
        "//beancount/core:convert",
        "//beancount/core:data",
        "//beancount/core:inventory",
    ],
)

py_test(
    name = "fill_account_test",
    srcs = ["fill_account_test.py"],
    deps = [
        ":fill_account",
        "//beancount:loader",
        "//beancount/parser:cmptest",
        "//beancount:plugins_for_tests",
    ],
)


py_library(
    name = "fix_payees",
    srcs = ["fix_payees.py"],
    deps = [
        "//beancount/core:data",
    ],
)

py_test(
    name = "fix_payees_test",
    srcs = ["fix_payees_test.py"],
    deps = [
        ":fix_payees",
        "//beancount:loader",
        "//beancount/parser:cmptest",
        "//beancount:plugins_for_tests",
    ],
)

py_library(
    name = "forecast",
    srcs = ["forecast.py"],
    deps = [
        "//beancount/core:data",
    ],
)

py_test(
    name = "forecast_test",
    srcs = ["forecast_test.py"],
    deps = [
        ":forecast",
        "//beancount:loader",
        "//beancount/parser:cmptest",
        "//beancount:plugins_for_tests",
    ],
)


py_library(
    name = "ira_contribs",
    srcs = ["ira_contribs.py"],
    deps = [
        "//beancount/core:account_types",
        "//beancount/core:amount",
        "//beancount/core:data",
        "//beancount/core:number",
        "//beancount/parser:printer",
    ],
)

py_test(
    name = "ira_contribs_test",
    srcs = ["ira_contribs_test.py"],
    deps = [
        ":ira_contribs",
        "//beancount/plugins:implicit_prices",
        "//beancount:loader",
        "//beancount/parser:cmptest",
        "//beancount:plugins_for_tests",
    ],
)

py_library(
    name = "mark_unverified",
    srcs = ["mark_unverified.py"],
    deps = [
        "//beancount/core:data",
    ],
)

py_test(
    name = "mark_unverified_test",
    srcs = ["mark_unverified_test.py"],
    deps = [
        ":mark_unverified",
        "//beancount/core:data",
        "//beancount:loader",
        "//beancount/parser:cmptest",
        "//beancount/parser:parser",
        "//beancount:plugins_for_tests",
    ],
)

py_library(
    name = "merge_meta",
    srcs = ["merge_meta.py"],
    deps = [
        "//beancount/core:getters",
        "//beancount:loader",
    ],
)

py_test(
    name = "merge_meta_test",
    srcs = ["merge_meta_test.py"],
    deps = [
        ":merge_meta",
        "//beancount:loader",
        "//beancount/parser:cmptest",
        "//beancount:plugins_for_tests",
    ],
)


py_library(
    name = "tag_pending",
    srcs = ["tag_pending.py"],
    deps = [
        "//beancount/core:inventory",
        "//beancount/ops:basicops",
    ],
)

py_test(
    name = "tag_pending_test",
    srcs = ["tag_pending_test.py"],
    deps = [
        ":tag_pending",
        "//beancount/core:data",
        "//beancount:loader",
        "//beancount/utils:test_utils",
        "//beancount:plugins_for_tests",
    ],
)


py_library(
    name = "transfer_lots",
    srcs = ["transfer_lots.py"],
    deps = [
        "//beancount/core:inventory",
        "//beancount/ops:basicops",
    ],
)

py_test(
    name = "transfer_lots_test",
    srcs = ["transfer_lots_test.py"],
    deps = [
        ":transfer_lots",
        "//beancount/parser:cmptest",
        "//beancount/core:data",
        "//beancount:loader",
        "//beancount/utils:test_utils",
        "//beancount:plugins_for_tests",
    ],
)


# NOTE: Disabled because dependent beancount.ops.holdings has been deprecated.
# And we will not port this--realizing unrealized gains is the wrong approach anyway.
#
# py_library(
#     name = "unrealized",
#     srcs = ["unrealized.py"],
#     deps = [
#         "//beancount/core:account",
#         "//beancount/core:amount",
#         "//beancount/core:data",
#         "//beancount/core:flags",
#         "//beancount/core:getters",
#         "//beancount/core:number",
#         "//beancount/core:prices",
#         "//beancount/ops:holdings",
#         "//beancount/parser:options",
#     ],
# )
#
# py_test(
#     name = "unrealized_test",
#     srcs = ["unrealized_test.py"],
#     deps = [
#         ":unrealized",
#         "//beancount/core:data",
#         "//beancount/core:number",
#         "//beancount:loader",
#         "//beancount/ops:validation",
#         "//beancount/parser:options",
#         "//beancount:plugins_for_tests",
#     ],
# )

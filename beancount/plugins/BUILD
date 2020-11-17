package(default_visibility = ["//visibility:public"])

py_library(
    name = "all_plugins",
    srcs = [
        ":__init__.py",
    ],
    deps = [
        ":auto_accounts",
        ":auto",
        ":book_conversions",
        ":check_average_cost",
        ":check_closing",
        ":check_commodity",
        ":coherent_cost",
        ":commodity_attr",
        ":currency_accounts",
        ":divert_expenses",
        ":exclude_tag",
        ":fill_account",
        ":fix_payees",
        ":forecast",
        ":implicit_prices",
        ":ira_contribs",
        ":leafonly",
        ":mark_unverified",
        ":merge_meta",
        ":noduplicates",
        ":nounused",
        ":onecommodity",
        ":pedantic",
        ":sellgains",
        ":split_expenses",
        ":tag_pending",
        ":unique_prices",
        ":unrealized",
    ],
)

py_library(
    name = "auto_accounts",
    srcs = ["auto_accounts.py"],
    deps = [
        "//beancount/core:data",
        "//beancount/core:getters",
    ],
)

py_test(
    name = "auto_accounts_test",
    srcs = ["auto_accounts_test.py"],
    deps = [
        "//beancount/parser:cmptest",
        ":auto_accounts",
        "//beancount:loader",
    ],
)

py_library(
    name = "auto",
    srcs = ["auto.py"],
    deps = [
        "//beancount/plugins:auto_accounts",
        "//beancount/plugins:implicit_prices",
        "//beancount:loader",
    ],
)

py_test(
    name = "auto_test",
    srcs = ["auto_test.py"],
    deps = [
        ":auto",
        "//beancount:loader",
        "//beancount:plugins_for_tests",
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
    name = "check_average_cost",
    srcs = ["check_average_cost.py"],
    deps = [
        "//beancount/core:data",
        "//beancount/core:getters",
        "//beancount/core:inventory",
        "//beancount/core:number",
    ],
)

py_test(
    name = "check_average_cost_test",
    srcs = ["check_average_cost_test.py"],
    deps = [
        ":check_average_cost",
        "//beancount:loader",
        "//beancount:plugins_for_tests",
    ],
)

py_library(
    name = "check_closing",
    srcs = ["check_closing.py"],
    deps = [
        "//beancount/core:amount",
        "//beancount/core:data",
        "//beancount/core:number",
    ],
)

py_test(
    name = "check_closing_test",
    srcs = ["check_closing_test.py"],
    deps = [
        ":check_closing",
        "//beancount:loader",
        "//beancount/parser:cmptest",
        "//beancount/plugins:auto_accounts",
    ],
)

py_library(
    name = "check_commodity",
    srcs = ["check_commodity.py"],
    deps = [
        "//beancount/core:data",
        "//beancount/core:getters",
    ],
)

py_test(
    name = "check_commodity_test",
    srcs = ["check_commodity_test.py"],
    deps = [
        "//beancount:loader",
        ":check_commodity",
        "//beancount:plugins_for_tests",
    ],
)

py_library(
    name = "coherent_cost",
    srcs = ["coherent_cost.py"],
    deps = [
        "//beancount/core:data",
    ],
)

py_test(
    name = "coherent_cost_test",
    srcs = ["coherent_cost_test.py"],
    deps = [
        "//beancount:loader",
        "//beancount/parser:cmptest",
        ":coherent_cost",
        "//beancount:plugins_for_tests",
    ],
)

py_library(
    name = "commodity_attr",
    srcs = ["commodity_attr.py"],
    deps = [
        "//beancount/core:data",
    ],
)

py_test(
    name = "commodity_attr_test",
    srcs = ["commodity_attr_test.py"],
    deps = [
        "//beancount:loader",
        ":commodity_attr",
        "//beancount:plugins_for_tests",
    ],
)

py_library(
    name = "currency_accounts",
    srcs = ["currency_accounts.py"],
    deps = [
        "//beancount/core:account",
        "//beancount/core:convert",
        "//beancount/core:data",
        "//beancount/core:inventory",
    ],
)

py_test(
    name = "currency_accounts_test",
    srcs = ["currency_accounts_test.py"],
    deps = [
        "//beancount/core:data",
        "//beancount:loader",
        "//beancount/parser:cmptest",
        ":currency_accounts",
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
    name = "implicit_prices",
    srcs = ["implicit_prices.py"],
    deps = [
        "//beancount/core:amount",
        "//beancount/core:data",
        "//beancount/core:inventory",
    ],
)

py_test(
    name = "implicit_prices_test",
    srcs = ["implicit_prices_test.py"],
    deps = [
        ":implicit_prices",
        "//beancount/core:data",
        "//beancount/core:number",
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
        ":implicit_prices",
        "//beancount:loader",
        "//beancount/parser:cmptest",
        "//beancount:plugins_for_tests",
    ],
)

py_library(
    name = "leafonly",
    srcs = ["leafonly.py"],
    deps = [
        "//beancount/core:data",
        "//beancount/core:getters",
        "//beancount/core:realization",
    ],
)

py_test(
    name = "leafonly_test",
    srcs = ["leafonly_test.py"],
    deps = [
        ":leafonly",
        "//beancount:loader",
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
    name = "noduplicates",
    srcs = ["noduplicates.py"],
    deps = [
        "//beancount/core:compare",
    ],
)

py_test(
    name = "noduplicates_test",
    srcs = ["noduplicates_test.py"],
    deps = [
        ":noduplicates",
        "//beancount/core:compare",
        "//beancount:loader",
        "//beancount/parser:cmptest",
        "//beancount:plugins_for_tests",
    ],
)

py_library(
    name = "nounused",
    srcs = ["nounused.py"],
    deps = [
        "//beancount/core:data",
        "//beancount/core:getters",
    ],
)

py_test(
    name = "nounused_test",
    srcs = ["nounused_test.py"],
    deps = [
        ":nounused",
        "//beancount:loader",
        "//beancount/parser:cmptest",
        "//beancount:plugins_for_tests",
    ],
)

py_library(
    name = "onecommodity",
    srcs = ["onecommodity.py"],
    deps = [
        "//beancount/core:data",
    ],
)

py_test(
    name = "onecommodity_test",
    srcs = ["onecommodity_test.py"],
    deps = [
        "//beancount:loader",
        ":onecommodity",
        "//beancount:plugins_for_tests",
    ],
)

py_library(
    name = "pedantic",
    srcs = ["pedantic.py"],
    deps = [
        "//beancount:loader",
        "//beancount/plugins:check_commodity",
        "//beancount/plugins:coherent_cost",
        "//beancount/plugins:leafonly",
        "//beancount/plugins:noduplicates",
        "//beancount/plugins:nounused",
        "//beancount/plugins:onecommodity",
        "//beancount/plugins:sellgains",
        "//beancount/plugins:unique_prices",
    ],
)

py_test(
    name = "pedantic_test",
    srcs = ["pedantic_test.py"],
    deps = [
        ":pedantic",
        "//beancount:loader",
        "//beancount:plugins_for_tests",
    ],
)

py_library(
    name = "sellgains",
    srcs = ["sellgains.py"],
    deps = [
        "//beancount/core:account_types",
        "//beancount/core:amount",
        "//beancount/core:convert",
        "//beancount/core:data",
        "//beancount/core:interpolate",
        "//beancount/core:inventory",
        "//beancount/core:number",
        "//beancount/parser:options",
    ],
)

py_test(
    name = "sellgains_test",
    srcs = ["sellgains_test.py"],
    deps = [
        ":sellgains",
        "//beancount:loader",
        "//beancount/ops:validation",
        "//beancount/parser:printer",
        "//beancount:plugins_for_tests",
    ],
)

py_library(
    name = "split_expenses",
    srcs = ["split_expenses.py"],
    deps = [
        "//beancount/core:account",
        "//beancount/core:account_types",
        "//beancount/core:amount",
        "//beancount/core:data",
        "//beancount/core:getters",
        "//beancount/core:interpolate",
        "//beancount:loader",
        "//beancount/parser:options",
        "//beancount/query:query",
        "//beancount/query:query_render",
        "//beancount/parser:version",
    ],
)

py_test(
    name = "split_expenses_test",
    srcs = ["split_expenses_test.py"],
    data = [
        "//examples:sharing"
    ],
    deps = [
        ":split_expenses",
        "//beancount:loader",
        "//beancount/parser:cmptest",
        "//beancount/utils:test_utils",
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
    name = "unique_prices",
    srcs = ["unique_prices.py"],
    deps = [
        "//beancount/core:data",
    ],
)

py_test(
    name = "unique_prices_test",
    srcs = ["unique_prices_test.py"],
    deps = [
        ":unique_prices",
        "//beancount:loader",
        "//beancount/parser:cmptest",
        "//beancount/plugins:implicit_prices",
        "//beancount:plugins_for_tests",
    ],
)

py_library(
    name = "unrealized",
    srcs = ["unrealized.py"],
    deps = [
        "//beancount/core:account",
        "//beancount/core:amount",
        "//beancount/core:data",
        "//beancount/core:flags",
        "//beancount/core:getters",
        "//beancount/core:number",
        "//beancount/core:prices",
        "//beancount/ops:holdings",
        "//beancount/parser:options",
    ],
)

py_test(
    name = "unrealized_test",
    srcs = ["unrealized_test.py"],
    deps = [
        ":unrealized",
        "//beancount/core:data",
        "//beancount/core:number",
        "//beancount:loader",
        "//beancount/ops:validation",
        "//beancount/parser:options",
        "//beancount:plugins_for_tests",
    ],
)

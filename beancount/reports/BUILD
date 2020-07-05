package(default_visibility = ["//beancount/web:__pkg__"])

py_library(
    name = "balance_reports",
    srcs = ["balance_reports.py"],
    deps = [
        "//beancount/core:convert",
        "//beancount/core:display_context",
        "//beancount/core:realization",
        "//beancount/reports:base",
        "//beancount/reports:tree_table",
    ],
)

py_test(
    name = "balance_reports_test",
    srcs = ["balance_reports_test.py"],
    deps = [
        "//beancount/parser:options",
        "//beancount/reports:balance_reports",
        "//beancount/reports:base_test",
    ],
)

py_library(
    name = "base",
    srcs = ["base.py"],
    data = ["template.html"],
    deps = [
        "//beancount/core:display_context",
        "//beancount/core:prices",
        "//beancount/core:realization",
        "//beancount/parser:options",
        "//beancount/reports:html_formatter",
        "//beancount/utils:table",
        "//beancount/parser:version",
    ],
)

py_test(
    name = "base_test",
    srcs = ["base_test.py"],
    deps = [
        "//beancount/core:display_context",
        "//beancount/core:number",
        "//beancount/core:realization",
        "//beancount:loader",
        "//beancount/parser:options",
        "//beancount/reports:base",
        "//beancount/utils:table",
        "//beancount:plugins_for_tests",
    ],
)

py_library(
    name = "context",
    srcs = ["context.py"],
    visibility = [
        "//beancount/scripts:__pkg__",
        "//beancount/web:__pkg__",
    ],
    deps = [
        "//beancount/core:compare",
        "//beancount/core:convert",
        "//beancount/core:data",
        "//beancount/core:getters",
        "//beancount/core:interpolate",
        "//beancount/core:inventory",
        "//beancount/parser:printer",
    ],
)

py_test(
    name = "context_test",
    srcs = ["context_test.py"],
    deps = [
        "//beancount:loader",
        "//beancount/reports:context",
        "//beancount/utils:test_utils",
        "//beancount:plugins_for_tests",
        "//beancount/plugins:implicit_prices",
    ],
)

py_library(
    name = "convert_reports",
    srcs = ["convert_reports.py"],
    deps = [
        "//beancount/core:amount",
        "//beancount/core:convert",
        "//beancount/core:data",
        "//beancount/core:display_context",
        "//beancount/core:interpolate",
        "//beancount/core:position",
        "//beancount/reports:base",
    ],
)

py_test(
    name = "convert_reports_test",
    srcs = ["convert_reports_test.py"],
    deps = [
        "//beancount/core:data",
        "//beancount:loader",
        "//beancount/parser:cmptest",
        ":convert_reports",
        "//beancount/reports:report",
        "//beancount/scripts:example",
        "//beancount/utils:test_utils",
        "//beancount:plugins_for_tests",
        "//beancount/plugins:implicit_prices",
    ],
)

py_library(
    name = "export_reports",
    srcs = ["export_reports.py"],
    deps = [
        "//beancount/core:amount",
        "//beancount/core:getters",
        "//beancount/core:number",
        "//beancount/core:prices",
        "//beancount/ops:holdings",
        "//beancount/reports:base",
        "//beancount/reports:holdings_reports",
    ],
)

py_test(
    name = "export_reports_test",
    srcs = ["export_reports_test.py"],
    deps = [
        "//beancount/core:getters",
        "//beancount/core:number",
        "//beancount:loader",
        "//beancount/reports:export_reports",
        "//beancount/reports:holdings_reports",
        "//beancount:plugins_for_tests",
    ],
)

py_library(
    name = "gviz",
    srcs = ["gviz.py"],
    deps = [
    ],
)

py_test(
    name = "gviz_test",
    srcs = ["gviz_test.py"],
    deps = [
        "//beancount/reports:gviz",
    ],
)

py_library(
    name = "holdings_reports",
    srcs = ["holdings_reports.py"],
    deps = [
        "//beancount/core:account",
        "//beancount/core:convert",
        "//beancount/core:data",
        "//beancount/core:flags",
        "//beancount/core:number",
        "//beancount/core:prices",
        "//beancount/ops:holdings",
        "//beancount/ops:summarize",
        "//beancount/parser:options",
        "//beancount/parser:printer",
        "//beancount/reports:base",
        "//beancount/utils:table",
    ],
)

py_test(
    name = "holdings_reports_test",
    srcs = ["holdings_reports_test.py"],
    deps = [
        "//beancount:loader",
        "//beancount/ops:holdings",
        "//beancount/reports:holdings_reports",
        "//beancount/utils:table",
        "//beancount:plugins_for_tests",
    ],
)

py_library(
    name = "html_formatter",
    srcs = ["html_formatter.py"],
    deps = [
        "//beancount/core:display_context",
        "//beancount/parser:printer",
    ],
)

py_test(
    name = "html_formatter_test",
    srcs = ["html_formatter_test.py"],
    deps = [
        "//beancount/core:data",
        "//beancount/core:display_context",
        "//beancount/core:inventory",
        "//beancount/reports:html_formatter",
    ],
)

py_library(
    name = "journal_html",
    srcs = ["journal_html.py"],
    deps = [
        "//beancount/core:convert",
        "//beancount/core:data",
        "//beancount/core:flags",
        "//beancount/core:position",
        "//beancount/core:realization",
    ],
)

py_test(
    name = "journal_html_test",
    srcs = ["journal_html_test.py"],
    deps = [
        "//beancount/core:data",
        "//beancount/core:display_context",
        "//beancount/core:realization",
        "//beancount:loader",
        "//beancount/reports:html_formatter",
        "//beancount/reports:journal_html",
        "//beancount:plugins_for_tests",
    ],
)

py_library(
    name = "journal_reports",
    srcs = ["journal_reports.py"],
    deps = [
        "//beancount/core:data",
        "//beancount/core:realization",
        "//beancount/reports:base",
        "//beancount/reports:journal_html",
        "//beancount/reports:journal_text",
        "//beancount/utils:misc_utils",
    ],
)

py_test(
    name = "journal_reports_test",
    srcs = ["journal_reports_test.py"],
    deps = [
        "//beancount/parser:options",
        "//beancount/reports:base_test",
        "//beancount/reports:journal_reports",
    ],
)

py_library(
    name = "journal_text",
    srcs = ["journal_text.py"],
    deps = [
        "//beancount/core:convert",
        "//beancount/core:data",
        "//beancount/core:number",
        "//beancount/core:realization",
    ],
)

py_test(
    name = "journal_text_test",
    srcs = ["journal_text_test.py"],
    deps = [
        "//beancount/core:amount",
        "//beancount/core:data",
        "//beancount/core:number",
        "//beancount/core:position",
        "//beancount/core:realization",
        "//beancount:loader",
        "//beancount/reports:journal_text",
        "//beancount:plugins_for_tests",
        "//beancount/plugins:implicit_prices",
    ],
)

py_library(
    name = "misc_reports",
    srcs = ["misc_reports.py"],
    deps = [
        "//beancount/core:account_types",
        "//beancount/core:data",
        "//beancount/core:display_context",
        "//beancount/core:getters",
        "//beancount/core:realization",
        "//beancount/parser:options",
        "//beancount/parser:printer",
        "//beancount/reports:base",
        "//beancount/utils:table",
        "//beancount/reports:tree_table",
        "//beancount/utils:date_utils",
        "//beancount/utils:misc_utils",
    ],
)

py_test(
    name = "misc_reports_test",
    srcs = ["misc_reports_test.py"],
    deps = [
        "//beancount:loader",
        "//beancount/parser:options",
        "//beancount/reports:base_test",
        "//beancount/reports:misc_reports",
    ],
)

py_library(
    name = "price_reports",
    srcs = ["price_reports.py"],
    deps = [
        "//beancount/core:amount",
        "//beancount/core:data",
        "//beancount/core:getters",
        "//beancount/core:prices",
        "//beancount/ops:lifetimes",
        "//beancount/parser:printer",
        "//beancount/reports:base",
        "//beancount/reports:gviz",
        "//beancount/utils:table",
    ],
)

py_test(
    name = "price_reports_test",
    srcs = ["price_reports_test.py"],
    deps = [
        "//beancount/parser:options",
        "//beancount/reports:base_test",
        "//beancount/reports:misc_reports",
    ],
)

py_library(
    name = "report",
    srcs = ["report.py"],
    visibility = [
        "//bin:__pkg__",
        "//beancount/scripts:__pkg__",
    ],
    deps = [
        "//beancount:loader_with_plugins",
        "//beancount/ops:validation",
        "//beancount/reports:balance_reports",
        "//beancount/reports:base",
        "//beancount/reports:convert_reports",
        "//beancount/reports:export_reports",
        "//beancount/reports:holdings_reports",
        "//beancount/reports:journal_reports",
        "//beancount/reports:misc_reports",
        "//beancount/reports:price_reports",
        "//beancount/utils:table",
        "//beancount/utils:file_utils",
        "//beancount/utils:misc_utils",
        "//beancount/parser:version",
    ],
)

py_test(
    name = "report_test",
    srcs = ["report_test.py"],
    data = [
        "//examples:example",
    ],
    deps = [
        "//beancount/reports:base",
        "//beancount/reports:report",
        "//beancount/utils:test_utils",
        "//beancount:plugins_for_tests",
        "//beancount/plugins:implicit_prices",
    ],
)

py_library(
    name = "tree_table",
    srcs = ["tree_table.py"],
    deps = [
        "//beancount/core:account_types",
        "//beancount/core:convert",
        "//beancount/core:data",
        "//beancount/core:inventory",
        "//beancount/core:number",
        "//beancount/core:realization",
    ],
)

py_test(
    name = "tree_table_test",
    srcs = ["tree_table_test.py"],
    deps = [
        "//beancount/core:display_context",
        "//beancount/core:realization",
        "//beancount:loader",
        "//beancount/reports:html_formatter",
        "//beancount/reports:tree_table",
        "//beancount:plugins_for_tests",
    ],
)

py_library(
    name = "bottle_utils",
    srcs = ["bottle_utils.py"],
    deps = [
        # Insert a dep to bottle here?
    ],
)

py_test(
    name = "bottle_utils_test",
    srcs = ["bottle_utils_test.py"],
    deps = [
        ":bottle_utils",
    ],
)

py_library(
    name = "views",
    srcs = ["views.py"],
    deps = [
        "//beancount/core:data",
        "//beancount/ops:summarize",
        "//beancount/core:realization",
        "//beancount/parser:options",
        "//beancount/utils:misc_utils",
        "//beancount/utils:date_utils",
    ],
)

py_test(
    name = "views_test",
    srcs = ["views_test.py"],
    deps = [
        ":views",
        "//beancount:loader",
        "//beancount/parser:options",
        "//beancount/core:realization",
        "//beancount/ops:pad",
        "//beancount/ops:documents",
        "//beancount/ops:balance",
    ],
)

py_library(
    name = "scrape",
    srcs = ["scrape.py"],
    visibility = [
        "//beancount/scripts:__pkg__",
    ],
    deps = [
        # Add lxml here?
    ],
)

py_test(
    name = "scrape_test",
    srcs = ["scrape_test.py"],
    deps = [
        "//beancount/utils:test_utils",
        "//beancount/web:scrape",
    ],
)


py_library(
    name = "web",
    srcs = ["web.py"],
    data = [
        "web.html",
        "web.css",
        "favicon.ico",
        "third_party/sorttable.js"
    ],
    visibility = [
        "//bin:__pkg__",
        "//beancount/scripts:__pkg__",
    ],
    deps = [
        "//beancount/core:account",
        "//beancount/core:account_types",
        "//beancount/core:compare",
        "//beancount/core:convert",
        "//beancount/core:data",
        "//beancount/core:getters",
        "//beancount/core:prices",
        "//beancount/ops:balance",
        "//beancount/ops:basicops",
        "//beancount/ops:documents",
        "//beancount/ops:pad",
        "//beancount/parser:options",
        "//beancount/parser:printer",
        "//beancount/reports:balance_reports",
        "//beancount/reports:context",
        "//beancount/reports:holdings_reports",
        "//beancount/reports:html_formatter",
        "//beancount/reports:journal_html",
        "//beancount/reports:journal_reports",
        "//beancount/reports:misc_reports",
        "//beancount/reports:price_reports",
        "//beancount/utils:misc_utils",
        "//beancount/utils:text_utils",
        "//beancount/utils:version",
        "//beancount/web:bottle_utils",
        "//beancount/web:scrape",
        "//beancount/web:views",
        "//beancount:loader",
    ],
)

py_test(
    name = "web_test",
    srcs = ["web_test.py"],
    data = ["//examples:simple"],
    deps = [
        ":web",
        "//beancount/utils:test_utils",
        "//beancount/utils:version",
    ],
)

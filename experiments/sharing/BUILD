filegroup(
    name = "sharing",
    srcs = [
        "cozumel2015.beancount",
        "duxbury2015.beancount",
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
        ":sharing"
    ],
    deps = [
        ":split_expenses",
        "//beancount:loader",
        "//beancount/parser:cmptest",
        "//beancount/utils:test_utils",
        "//beancount:plugins_for_tests",
    ],
)

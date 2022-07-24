py_library(
    name = "sql",
    srcs = ["sql.py"],
    deps = [
        "//beancount/core:data",
        "//beancount:loader",
        "//beancount/utils:misc_utils",
        "//beancount/parser:version",
    ],
)

py_test(
    name = "sql_test",
    srcs = ["sql_test.py"],
    deps = [
        ":sql",
        "//beancount/utils:test_utils",
    ],
)

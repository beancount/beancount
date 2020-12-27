package(default_visibility = ["//visibility:public"])

py_library(
    name = "export",
    srcs = ["export.py"],
    data = [
        "//examples:example",
    ],
    deps = [
        "//beancount/core:account",
        "//beancount/core:account_types",
        "//beancount/core:data",
        "//beancount/core:flags",
        "//beancount/core:getters",
        "//beancount/core:number",
        "//beancount/core:prices",
        "//beancount:loader",
        "//beancount/ops:summarize",
        "//beancount/parser:options",
    ],
)

py_test(
    name = "export_test",
    srcs = ["export_test.py"],
    deps = [
        "//beancount/projects:export",
        "//beancount/utils:test_utils",
    ],
)

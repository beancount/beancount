package(default_visibility = ["//visibility:public"])

py_library(
    name = "will",
    srcs = ["will.py"],
    deps = [
        "//beancount/core:account",
        "//beancount/core:account_types",
        "//beancount/core:convert",
        "//beancount/core:data",
        "//beancount/core:getters",
        "//beancount/core:realization",
        "//beancount:loader",
        "//beancount/parser:options",
        "//beancount/parser:version",
    ],
)

py_test(
    name = "will_test",
    srcs = ["will_test.py"],
    deps = [
        "//beancount/core:getters",
        "//beancount:loader",
        "//beancount/projects:will",
        "//beancount/utils:test_utils",
        "//beancount:plugins_for_tests",
    ],
)

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

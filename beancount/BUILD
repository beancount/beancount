package(default_visibility = ["//visibility:public"])

py_library(
    name = "__init__",
    srcs = ["__init__.py"],
    deps = [
    ],
)

py_library(
    name = "loader",
    srcs = ["loader.py"],
    deps = [
        "//beancount/utils:misc_utils",
        "//beancount/core:data",
        "//beancount/parser:parser",
        "//beancount/parser:booking",
        "//beancount/parser:options",
        "//beancount/parser:printer",
        "//beancount/ops:validation",
        "//beancount/utils:encryption",
        "//beancount/utils:file_utils",
    ],
)

py_test(
    name = "loader_test",
    srcs = ["loader_test.py"],
    deps = [
        ":loader",
        ":plugins_for_tests",
        "//beancount/parser:parser",
        "//beancount/utils:test_utils",
        "//beancount/utils:encryption_test",
    ],
)

# TODO(blais): Remove all test targets and use this. From core, parser, utils
py_library(
    name = "plugins_for_tests",
    deps = [
        "//beancount/ops:pad",
        "//beancount/ops:documents",
        "//beancount/ops:balance",
        "//beancount/plugins:auto_accounts",
    ],
)

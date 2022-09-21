package(default_visibility = ["//visibility:public"])

genrule(
    name = "version_header",
    srcs = [":VERSION"],
    tools = ["//tools:gen_version_header"],
    cmd = "$(location //tools:gen_version_header) $(location :VERSION) > $@",
    outs = ["version.h"],
)

py_library(
    name = "__init__",
    srcs = ["__init__.py"],
    data = ["VERSION"],
    deps = [
        "//beancount/cparser:extmodule",
        "//beancount:api",
    ],
)

py_library(
    name = "api",
    srcs = ["api.py"],
    deps = [
        "//beancount/core:account",
        "//beancount/core:account_types",
        "//beancount/core:amount",
        "//beancount/core:convert",
        "//beancount/core:data",
        "//beancount/core:flags",
        "//beancount/core:getters",
        "//beancount/core:inventory",
        "//beancount/core:number",
        "//beancount/core:position",
        "//beancount/core:prices",
        "//beancount/core:realization",
        "//beancount:loader",
    ],
)

# Note: We don't include the required plugins in order to be conservative on
# dependencies for those tests which bypass it. We can probably simplify the
# deps later.
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

py_library(
    name = "loader_with_plugins",
    deps = [
        ":loader",
        "//beancount/ops:pad",
        "//beancount/ops:documents",
        "//beancount/ops:balance",
        "//beancount/plugins:all_plugins",
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

# A group of plugins widely assumed during testing.
py_library(
    name = "plugins_for_tests",
    deps = [
        "//beancount/ops:pad",
        "//beancount/ops:documents",
        "//beancount/ops:balance",
        "//beancount/plugins:auto_accounts",
    ],
)

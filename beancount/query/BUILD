package(default_visibility = ["//visibility:public"])

py_library(
    name = "numberify",
    srcs = ["numberify.py"],
    deps = [
        "//beancount/core:amount",
        "//beancount/core:inventory",
        "//beancount/core:number",
        "//beancount/core:position",
    ],
)

py_test(
    name = "numberify_test",
    srcs = ["numberify_test.py"],
    deps = [
        "//beancount/core:amount",
        "//beancount/core:display_context",
        "//beancount/core:inventory",
        "//beancount/core:number",
        "//beancount/core:position",
        "//beancount/query:numberify",
    ],
)

py_library(
    name = "query_compile",
    srcs = ["query_compile.py"],
    deps = [
        "//beancount/core:inventory",
        "//beancount/core:number",
        "//beancount/query:query_parser",
    ],
)

py_test(
    name = "query_compile_test",
    srcs = ["query_compile_test.py"],
    deps = [
        "//beancount/core:number",
        "//beancount/query:query_compile",
        "//beancount/query:query_env",
        "//beancount/query:query_parser",
    ],
)

py_library(
    name = "query_env",
    srcs = ["query_env.py"],
    deps = [
        "//beancount/core:account",
        "//beancount/core:account_types",
        "//beancount/core:amount",
        "//beancount/core:compare",
        "//beancount/core:convert",
        "//beancount/core:data",
        "//beancount/core:getters",
        "//beancount/core:inventory",
        "//beancount/core:number",
        "//beancount/core:position",
        "//beancount/core:prices",
        "//beancount/query:query_compile",
        "//beancount/utils:date_utils",
    ],
)

py_test(
    name = "query_env_test",
    srcs = ["query_env_test.py"],
    deps = [
        "//beancount/core:amount",
        "//beancount/core:inventory",
        "//beancount/core:number",
        "//beancount/core:position",
        "//beancount/parser:parser",
        "//beancount/query:query",
        "//beancount/query:query_compile",
        "//beancount/query:query_env",
    ],
)

py_library(
    name = "query_execute",
    srcs = ["query_execute.py"],
    deps = [
        "//beancount/core:data",
        "//beancount/core:display_context",
        "//beancount/core:getters",
        "//beancount/core:inventory",
        "//beancount/core:number",
        "//beancount/core:position",
        "//beancount/core:prices",
        "//beancount/ops:summarize",
        "//beancount/parser:options",
        "//beancount/parser:printer",
        "//beancount/query:query_compile",
        "//beancount/query:query_env",
        "//beancount/utils:misc_utils",
    ],
)

py_test(
    name = "query_execute_test",
    srcs = ["query_execute_test.py"],
    deps = [
        "//beancount/core:inventory",
        "//beancount/core:number",
        "//beancount:loader",
        "//beancount/parser:cmptest",
        "//beancount/query:query_compile",
        "//beancount/query:query_env",
        "//beancount/query:query_execute",
        "//beancount/query:query_parser",
        "//beancount/utils:misc_utils",
    ],
)

py_library(
    name = "query_parser",
    srcs = ["query_parser.py"],
    deps = [
        "//beancount/core:number",
        "//beancount/utils:misc_utils",
    ],
)

py_test(
    name = "query_parser_test",
    srcs = ["query_parser_test.py"],
    deps = [
        "//beancount/core:number",
        "//beancount/query:query_parser",
    ],
)

py_library(
    name = "query_render",
    srcs = ["query_render.py"],
    deps = [
        "//beancount/core:amount",
        "//beancount/core:distribution",
        "//beancount/core:inventory",
        "//beancount/core:number",
        "//beancount/core:position",
    ],
)

py_test(
    name = "query_render_test",
    srcs = ["query_render_test.py"],
    deps = [
        "//beancount/core:amount",
        "//beancount/core:display_context",
        "//beancount/core:inventory",
        "//beancount/core:number",
        "//beancount/core:position",
        "//beancount/query:query_render",
    ],
)

py_library(
    name = "query",
    srcs = ["query.py"],
    deps = [
        "//beancount/query:numberify",
        "//beancount/query:query_compile",
        "//beancount/query:query_env",
        "//beancount/query:query_execute",
        "//beancount/query:query_parser",
    ],
)

py_test(
    name = "query_test",
    srcs = ["query_test.py"],
    data = [
        "//examples:example"
    ],
    deps = [
        "//beancount:loader",
        "//beancount/query:query",
        "//beancount/utils:test_utils",
        "//beancount/plugins:auto_accounts",
        "//beancount:plugins_for_tests",
    ],
)

py_library(
    name = "shell",
    srcs = ["shell.py"],
    deps = [
        "//beancount/core:data",
        "//beancount:loader",
        "//beancount/parser:printer",
        "//beancount/query:numberify",
        "//beancount/query:query_compile",
        "//beancount/query:query_env",
        "//beancount/query:query_execute",
        "//beancount/query:query_parser",
        "//beancount/query:query_render",
        "//beancount/utils:misc_utils",
        "//beancount/utils:pager",
        "//beancount/parser:version",
    ],
)

py_test(
    name = "shell_test",
    srcs = ["shell_test.py"],
    data = [
        "//examples:example"
    ],
    deps = [
        "//beancount:loader",
        "//beancount/query:shell",
        "//beancount/utils:test_utils",
        "//beancount:plugins_for_tests",
    ],
)

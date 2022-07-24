package(default_visibility = ["//visibility:public"])

py_test(
    name = "check_examples_test",
    srcs = ["check_examples_test.py"],
    deps = [
        "//beancount/scripts:check",
        "//beancount/utils:test_utils",
    ],
)

py_library(
    name = "check",
    srcs = ["check.py"],
    deps = [
        "//beancount:loader",
        "//beancount/ops:validation",
        "//beancount/utils:misc_utils",
        "//beancount/parser:version",
    ],
)

py_test(
    name = "check_test",
    srcs = ["check_test.py"],
    deps = [
        "//beancount/scripts:check",
        "//beancount/utils:test_utils",
        "//beancount:plugins_for_tests",
    ],
)

py_library(
    name = "deps",
    srcs = ["deps.py"],
    deps = [
    ],
)

py_test(
    name = "deps_test",
    srcs = ["deps_test.py"],
    deps = [
        "//beancount/scripts:deps",
    ],
)

py_library(
    name = "directories",
    srcs = ["directories.py"],
    deps = [
        "//beancount/core:account",
        "//beancount/core:getters",
    ],
)

py_test(
    name = "directories_test",
    srcs = ["directories_test.py"],
    deps = [
        "//beancount/scripts:directories",
    ],
)

py_library(
    name = "doctor",
    srcs = ["doctor.py"],
    deps = [
        "//beancount/core:compare",
        "//beancount/core:realization",
        "//beancount/core:display_context",
        "//beancount/parser:printer",
        "//beancount/parser:context",
        "//beancount/scripts:deps",
        "//beancount/utils:misc_utils",
        "//beancount/parser:version",
        "//beancount:loader",
    ],
)

py_test(
    name = "doctor_test",
    srcs = ["doctor_test.py"],
    deps = [
        "//beancount/parser:cmptest",
        "//beancount/scripts:directories_test",
        "//beancount/scripts:doctor",
        "//beancount/utils:test_utils",
    ],
)

py_library(
    name = "example",
    srcs = ["example.py"],
    deps = [
        "//beancount/core:account",
        "//beancount/core:amount",
        "//beancount/core:convert",
        "//beancount/core:data",
        "//beancount/core:display_context",
        "//beancount/core:getters",
        "//beancount/core:inventory",
        "//beancount/core:number",
        "//beancount/core:prices",
        "//beancount/core:realization",
        "//beancount:loader",
        "//beancount/ops:validation",
        "//beancount/parser:booking",
        "//beancount/parser:parser",
        "//beancount/parser:printer",
        "//beancount/scripts:format",
        "//beancount/utils:date_utils",
        "//beancount/utils:misc_utils",
        "//beancount/parser:version",
    ],
)

py_test(
    name = "example_test",
    srcs = ["example_test.py"],
    deps = [
        "//beancount:loader",
        "//beancount/ops:validation",
        "//beancount/scripts:example",
        "//beancount/utils:test_utils",
        "//beancount:plugins_for_tests",
    ],
)

py_library(
    name = "format",
    srcs = ["format.py"],
    deps = [
        "//beancount/core:account",
        "//beancount/core:amount",
        "//beancount/parser:version",
    ],
)

py_test(
    name = "format_test",
    srcs = ["format_test.py"],
    deps = [
        "//beancount/scripts:format",
        "//beancount/utils:test_utils",
    ],
)

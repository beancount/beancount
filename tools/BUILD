package(default_visibility = ["//visibility:public"])

py_binary(
    name = "gen_version_header",
    srcs = ["gen_version_header.py"],
)

py_binary(
    name = "update_options",
    srcs = ["update_options.py"],
    deps = [
        "//beancount/parser:options",
        "//beancount/utils:test_utils",
    ],
)

py_test(
    name = "update_options_test",
    srcs = ["update_options_test.py"],
    data = [
        "//:nginx.conf",
    ],
    deps = [
        ":update_options",
    ],
)

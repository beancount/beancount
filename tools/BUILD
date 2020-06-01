package(default_visibility = ["//visibility:public"])

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
        "//:htaccess",
    ],
    deps = [
        ":update_options",
    ],
)

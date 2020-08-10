# This basically excludes //beancount/utils.
package(default_visibility = [
    "//beancount:__subpackages__",
])

cc_library(
    name = "account",
    hdrs = ["account.h"],
    srcs = ["account.cc"],
    deps = [
        "//beancount:defs",
        "@com_google_re2//:re2",
        "@com_google_absl//absl/strings:str_format",
        "@com_google_absl//absl/strings",
    ],
)

cc_test(
    name = "account_test",
    srcs = ["account_test.cc"],
    deps = [
        "//beancount:defs",
        ":account",
        "@com_google_googletest//:gtest_main",
    ],
)

package(default_visibility=["//visibility:public"])

load("@rules_cc//cc:defs.bzl", "cc_binary")

# Requires latest version:
cc_binary(
    name = "example",
    srcs = ["example.cc"],
    deps = [
        "@arrow//:arrow",
        "@arrow//:arrow_format",
    ],
)

cc_binary(
    name = "row-wise-conversion-example",
    srcs = ["row-wise-conversion-example.cc"],
    deps = [
        "@arrow//:arrow",
        "@arrow//:arrow_format",
    ],
)

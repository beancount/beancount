load("@rules_foreign_cc//foreign_cc:defs.bzl", "configure_make")

filegroup(name = "all", srcs = glob(["**"]), visibility = ["//visibility:public"])

configure_make(
    name = "m4",
    env = select({
        "@platforms//os:macos": {"AR": ""},
        "//conditions:default": {},
    }),
    lib_source = "@m4//:all",
    out_binaries = ["m4"],
)

filegroup(
    name = "m4_bin",
    srcs = [":m4"],
    output_group = "m4",
    visibility = ["//visibility:public"],
)

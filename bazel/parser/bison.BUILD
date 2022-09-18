load("@rules_foreign_cc//foreign_cc:defs.bzl", "configure_make")

filegroup(name = "all", srcs = glob(["**"]), visibility = ["//visibility:public"])

filegroup(
    name = "bison_runtime_data",
    srcs = glob(["data/**/*"]),
    output_licenses = ["unencumbered"],
    path = "data",
    visibility = ["//visibility:public"],

)
exports_files(["data"])

configure_make(
    name = "bison",
    build_data = ["@m4//:m4_bin"],
    # Without specifying m4 directly, cc_rules_foreign may fallback
    # on the system m4, which might be too old.
    env = select({
        "@platforms//os:macos": {
            "AR": "",
            "M4": "$(execpath @m4//:m4_bin)",
        },
        "//conditions:default": {
            "M4": "$(execpath @m4//:m4_bin)"
        },
    }),
    lib_source = "@bison//:all",
    out_binaries = [
        "bison",
        "yacc",
    ],
)

filegroup(
    name = "bison_bin",
    srcs = [":bison"],
    output_group = "bison",
    visibility = ["//visibility:public"],
)

"""
Rules for adding './configure && make' style dependencies.
"""

load("@rules_foreign_cc//foreign_cc:defs.bzl", "configure_make")

licenses(["notice"])  # Apache v2.0

package(
    default_visibility = ["//:__subpackages__"],
)

exports_files([
    "bison.bzl",
    "reflex.bzl",
])

#------------------------------------------------------------------------------
# M4

configure_make(
    name = "m4",
    env = select({
        "@platforms//os:macos": {"AR": ""},
        "//conditions:default": {},
    }),
    lib_source = "@m4//:all",
    out_binaries = [
        "m4",
    ],
)

filegroup(
    name = "m4_bin",
    srcs = [":m4"],
    output_group = "m4",
)

#------------------------------------------------------------------------------
# Bison

configure_make(
    name = "bison",
    build_data = [":m4_bin"],
    # Without specifying m4 directly, cc_rules_foreign may fallback
    # on the system m4, which might be too old.
    env = select({
        "@platforms//os:macos": {
            "AR": "",
            "M4": "$(execpath :m4_bin)",
        },
        "//conditions:default": {
            "M4": "$(execpath :m4_bin)"
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
)

#------------------------------------------------------------------------------
# ReFlex

configure_make(
    name = "reflex",
    out_binaries = [
        "reflex",
    ],
    out_static_libs = ["libreflex.a"],
    env = {
        # Use PIC so we can link Python extension modules to this.
        "CXXFLAGS": "-fPIC",
    },
    lib_source = "@reflex//:all",
)

filegroup(
    name = "reflex_bin",
    srcs = [":reflex"],
    output_group = "reflex",
)

cc_library(
    name = "reflex_headers",
    hdrs = ["@reflex//:headers"],
)

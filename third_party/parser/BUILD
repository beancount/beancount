"""
Rules for adding './configure && make' style dependencies.
"""

load("@rules_foreign_cc//tools/build_defs:configure.bzl", "configure_make")
load("//third_party/foreign:ar_wrapper.bzl", "ar_wrapper")

licenses(["notice"])  # Apache v2.0

package(
    default_visibility = ["//:__subpackages__"],
)

exports_files([
    "bison.bzl",
    "flex.bzl",
])

ar_wrapper(
    name = "ar_wrapper",
    visibility = ["//visibility:public"],
)

configure_make(
    name = "bison",
    binaries = [
        "bison",
        "yacc",
    ],
    configure_env_vars = {
        "CC_FOR_BUILD": "$$CC$$",
        "AR": "ar_wrapper",
    },
    lib_source = "@bison//:all",
    static_libraries = ["liby.a"],
    tools_deps = [":ar_wrapper"],
)

filegroup(
    name = "bison_bin",
    srcs = [":bison"],
    output_group = "bison",
)

configure_make(
    name = "reflex",
    binaries = [
        "reflex",
    ],
    static_libraries = ["libreflex.a"],
    configure_env_vars = {
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
    strip_include_prefix = "//external/reflex/include",
)

# configure_make(
#     name = "flex",
#     binaries = [
#         "flex",
#     ],
#     # This seems to be necessary (using tools_dep and weird path) because unlike
#     # bison, flex needs to invoke m4 during build (whereas bison needs it only
#     # during `configure`).
#     configure_env_vars = {
#         "M4": "$$EXT_BUILD_DEPS$$/m4/bin/m4",
#         "AR": "ar_wrapper",
#     },
#     lib_source = "@flex//:all",
#     tools_deps = [":ar_wrapper"],
#     deps = [":m4"],
# )

# filegroup(
#     name = "flex_bin",
#     srcs = [":flex"],
#     output_group = "flex",
# )

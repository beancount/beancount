load("@rules_foreign_cc//foreign_cc:defs.bzl", "configure_make")

package(
    default_visibility = ["//visibility:public"],
)

filegroup(name = "all", srcs = glob(["**"]))
filegroup(name = "headers", srcs = glob(["include/reflex/*.h"]))

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

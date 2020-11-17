"""Build rules for International Components for Unicode library."""

load("@rules_foreign_cc//tools/build_defs:configure.bzl", "configure_make")

licenses(["notice"])  # Apache v2.0

package(
    default_visibility = ["//visibility:public"],
)

# We need to label this for configure_make.
filegroup(
    name = "all",
    srcs = glob(["**"]),
)

configure_make(
    name = "icu",
    configure_command = "source/configure",
    configure_env_vars = {
        "CXXFLAGS": "-fPIC",  # For JNI
        "CFLAGS": "-fPIC",  # For JNI
        "LIBS": "$$LDFLAGS$$",
        "AR": "ar_wrapper",
    },
    configure_options = [
        "--enable-option-checking",
        "--enable-static",
        "--enable-tools",  # needed to build data
        "--disable-shared",
        "--disable-dyload",
        "--disable-extras",
        "--disable-plugins",
        "--disable-tests",
        "--disable-samples",
        "--with-data-packaging=static",
    ],
    lib_source = "@icu//:all",
    static_libraries = [
        "libicui18n.a",
        "libicuio.a",
        "libicuuc.a",
        "libicudata.a",
    ],
    tools_deps = ["//third_party/foreign:ar_wrapper"],
)

cc_library(
    name = "common",
    deps = [
        "icu",
    ],
)

cc_library(
    name = "headers",
    deps = [
        "icu",
    ],
)

cc_library(
    name = "unicode",
    deps = [
        "icu",
    ],
)

exports_files([
    "icu4c/LICENSE",
])

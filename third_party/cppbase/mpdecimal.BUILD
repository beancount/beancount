load("@rules_foreign_cc//tools/build_defs:configure.bzl", "configure_make")

package(default_visibility=["//visibility:public"])

licenses(["notice"])  # BSD

filegroup(
  name = "allsrcs",
  srcs = glob(["**"]),
  visibility = ["//visibility:public"]
)

configure_make(
    name = "mpdecimal",
    static_libraries = ["libmpdec.a", "libmpdec++.a"],
    # Use PIC so we can link Python extension modules to this.
    configure_env_vars = {"CFLAGS": "-fPIC", "CXXFLAGS": "-fPIC"},
    configure_in_place = True,
    lib_source = "//:allsrcs",
)

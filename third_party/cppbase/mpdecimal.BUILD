load("@rules_foreign_cc//foreign_cc:defs.bzl", "configure_make")

package(default_visibility=["//visibility:public"])

licenses(["notice"])  # BSD

filegroup(
  name = "allsrcs",
  srcs = glob(["**"]),
  visibility = ["//visibility:public"]
)

configure_make(
    name = "mpdecimal",
    out_static_libs = ["libmpdec.a", "libmpdec++.a"],
    # Use PIC so we can link Python extension modules to this.
    env = {"CFLAGS": "-fPIC", "CXXFLAGS": "-fPIC"},
    configure_in_place = True,
    lib_source = "//:allsrcs",
)

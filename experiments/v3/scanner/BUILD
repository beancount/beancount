load("//bazel/parser:reflex.bzl", "genreflex")

genreflex(
    name = "indents_gen",
    src = "indents.lxx",
    out = "indents.cc",
    # Set this to true to debug the scanner.
    debug = False,
    header_out = "indents.h",
)

cc_library(
    name = "indents_hdrs",
    hdrs = [
        "indents.h",
    ],
)

cc_binary(
    name = "indents",
    srcs = ["indents.cc"],
    deps = [
        ":indents_hdrs",
        "//bazel/parser:reflex",
    ],
)

cc_binary(
    name = "matcher",
    srcs = ["matcher.cc",],
    deps = [
        "//bazel/parser:reflex",
    ],
)

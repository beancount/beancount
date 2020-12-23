load("//third_party/parser:reflex.bzl", "genreflex")

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
        "//third_party/parser:reflex",
    ],
)

cc_binary(
    name = "matcher",
    srcs = ["matcher.cc",],
    deps = [
        "//third_party/parser:reflex",
    ],
)

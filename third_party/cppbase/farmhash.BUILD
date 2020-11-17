licenses(["notice"])  # Apache v2.0

cc_library(
    name = "farmhash_fingerprint",
    srcs = ["src/farmhash.cc"],
    hdrs = ["src/farmhash.h"],
    # For some reason, the default is 'util'.
    defines = ["NAMESPACE_FOR_HASH_FUNCTIONS=farmhash"],
    includes = ["src/."],
    visibility = ["//visibility:public"],
)

# Build file for Genivia's RE-Flex (https://github.com/Genivia/RE-flex)

cc_library(
    name = "reflex-lib",
    includes = ["include"],
    hdrs = glob(["include/reflex/*.h"]),
    srcs = glob(["lib/*.cpp", "unicode/*.cpp"]),
    copts = ["-march=native", "-mavx"],
    defines = ["HAVE_AVX"],
)

cc_library(
    name = "reflex-min",
    includes = ["include"],
    hdrs = glob(["include/reflex/*.h"]),
    srcs = ["lib/debug.cpp",
            "lib/error.cpp",
            "lib/input.cpp",
            "lib/matcher.cpp",
            "lib/pattern.cpp",
            "lib/utf8.cpp"],
    copts = ["-march=native", "-mavx"],
    defines = ["HAVE_AVX"],
)

cc_library(
    name = "reflex-main",
    includes = ["include"],
    hdrs = ["src/reflex.h"],
    srcs = ["src/reflex.cpp"],
    deps = [":reflex-lib"],
    copts = ["-march=native", "-mavx"],
    defines = ["HAVE_AVX"],
)

cc_binary(
    name = "reflex",
    deps = [":reflex-main"],
    copts = ["-march=native", "-mavx"],
    defines = ["HAVE_AVX"],
)

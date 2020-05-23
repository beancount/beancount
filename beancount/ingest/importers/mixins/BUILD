package(default_visibility = ["//visibility:public"])

py_library(
    name = "config",
    srcs = [
        "config.py",
    ],
    deps = [
        "//beancount/ingest:importer",
    ],
)

py_library(
    name = "filing",
    srcs = [
        "filing.py",
    ],
    deps = [
        "//beancount/ingest:importer",
        "//beancount/core:account",
    ],
)

py_library(
    name = "identifier",
    srcs = [
        "identifier.py",
    ],
    deps = [
        "//beancount/ingest:cache",
        "//beancount/ingest:importer",
    ],
)

package(default_visibility = ["//visibility:public"])

py_library(
    name = "config",
    srcs = ["config.py"],
)

py_test(
    name = "config_test",
    srcs = ["config_test.py"],
    deps = [
        "//beancount/ingest:importer",
        ":config",
    ],
)

py_library(
    name = "fileonly",
    srcs = ["fileonly.py"],
    deps = [
        "//beancount/ingest/importers/mixins:filing",
        "//beancount/ingest/importers/mixins:identifier",
    ],
)

py_test(
    name = "fileonly_test",
    srcs = ["fileonly_test.py"],
    deps = [
        ":fileonly",
        "//beancount/ingest:cache",
        "//beancount/utils:file_type",
        "//beancount/utils:test_utils",
    ],
)

py_library(
    name = "csv",
    srcs = ["csv.py"],
    deps = [
        "//beancount/core:data",
        "//beancount/core:amount",
        "//beancount/core:number",
        "//beancount/ingest/importers/mixins:filing",
        "//beancount/ingest/importers/mixins:identifier",
        "//beancount/utils:date_utils",
    ],
)

py_test(
    name = "csv_test",
    srcs = ["csv_test.py"],
    deps = [
        ":csv",
        "//beancount/ingest:cache",
        "//beancount/parser:cmptest",
        "//beancount/utils:test_utils",
    ],
)

py_library(
    name = "ofx",
    srcs = ["ofx.py"],
    deps = [
        "//beancount/core:amount",
        "//beancount/core:number",
        "//beancount/core:data",
        "//beancount/ingest:importer",
    ],
)

py_test(
    name = "ofx_test",
    srcs = ["ofx_test.py"],
    deps = [
        ":ofx",
        "//beancount/core:number",
        "//beancount/parser:parser",
        "//beancount/parser:cmptest",
    ],
)

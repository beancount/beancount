package(default_visibility = ["//visibility:public"])

py_library(
    name = "importer",
    srcs = [
        "importer.py",
    ],
    deps = [
        "//beancount/core:flags",
    ],
)

py_test(
    name = "importer_test",
    srcs = [
        "importer_test.py",
    ],
    deps = [
        ":importer",
        ":cache",
    ],
)

py_library(
    name = "cache",
    srcs = [
        "cache.py",
    ],
    deps = [
        "//beancount/utils:file_type",
        "//beancount/utils:defdict",
    ],
)

py_test(
    name = "cache_test",
    srcs = [
        "cache_test.py",
    ],
    deps = [
        ":cache",
        "//beancount/utils:file_type",
    ],
)

py_library(
    name = "scripts_utils",
    srcs = [
        "scripts_utils.py",
    ],
    deps = [
        "//beancount/ingest:importer",
        "//beancount/ingest:cache",
        "//beancount/utils:test_utils",
        "//beancount/utils:version",
        "//beancount/ingest:identify",
        "//beancount/ingest:extract",
        "//beancount/ingest:file",
    ],
)

py_test(
    name = "scripts_utils_test",
    srcs = [
        "scripts_utils_test.py",
    ],
    deps = [
        ":scripts_utils",
        "//beancount/utils:test_utils",
        "//beancount/ingest:extract",
    ],
)

py_library(
    name = "similar",
    srcs = [
        "similar.py",
    ],
    deps = [
        "//beancount/core:number",
        "//beancount/core:data",
        "//beancount/core:amount",
        "//beancount/core:interpolate",
    ],
)

py_test(
    name = "similar_test",
    srcs = [
        "similar_test.py",
    ],
    deps = [
        ":similar",
        "//beancount/core:number",
        "//beancount/core:data",
        "//beancount/parser:cmptest",
        "//beancount/parser:parser",
        "//beancount:loader",
        "//beancount/ops:pad",
        "//beancount/ops:documents",
        "//beancount/plugins:auto_accounts",
        "//beancount/ops:balance",
    ],
)

py_library(
    name = "extract",
    srcs = [
        "extract.py",
    ],
    deps = [
        "//beancount/core:data",
        "//beancount/parser:printer",
        "//beancount/ingest:similar",
        "//beancount/ingest:identify",
        "//beancount/ingest:cache",
        "//beancount:loader",
    ],
)

py_test(
    name = "extract_test",
    srcs = [
        "extract_test.py",
    ],
    data = [
        "//examples:example",
        "//examples/ingest:office_files",
    ],
    deps = [
        ":extract",
        "//examples/ingest:office",
        "//beancount/utils:test_utils",
        "//beancount/utils:misc_utils",
        "//beancount/parser:parser",
        "//beancount:loader",
        "//beancount/ingest:importer",
        "//beancount/ingest:scripts_utils",
    ],
)

py_library(
    name = "file",
    srcs = [
        "file.py",
    ],
    deps = [
        "//beancount/core:account",
        "//beancount/utils:misc_utils",
        "//beancount/ingest:identify",
        "//beancount/ingest:cache",
    ],
)

py_test(
    name = "file_test",
    srcs = [
        "file_test.py",
    ],
    data = [
        "//examples:example",
        "//examples/ingest:office_files",
    ],
    deps = [
        ":file",
        "//examples/ingest:office",
        "//beancount/utils:test_utils",
        "//beancount/utils:file_utils",
        "//beancount/ingest:scripts_utils",
    ],
)

py_library(
    name = "identify",
    srcs = [
        "identify.py",
    ],
    deps = [
        "//beancount/utils:file_utils",
        "//beancount/ingest:cache",
    ],
)

py_test(
    name = "identify_test",
    srcs = [
        "identify_test.py",
    ],
    data = [
        "//examples:example",
        "//examples/ingest:office_files",
    ],
    deps = [
        ":identify",
        "//examples/ingest:office",
        "//beancount/utils:test_utils",
        "//beancount/ingest:importer",
        "//beancount/ingest:scripts_utils",
    ],
)

py_library(
    name = "regression",
    srcs = [
        "regression.py",
    ],
    deps = [
        "//beancount/ingest:importer",
        "//beancount/parser:printer",
        "//beancount/utils:test_utils",
        "//beancount/utils:misc_utils",
        "//beancount/ingest:extract",
        "//beancount/ingest:cache",
    ],
)

py_library(
    name = "regression_pytest",
    srcs = [
        "regression_pytest.py",
    ],
    deps = [
        "//beancount/ingest:cache",
        "//beancount/ingest:extract",
        "//beancount/parser:printer",
    ],
)

py_test(
    name = "regression_test",
    srcs = [
        "regression_test.py",
    ],
    deps = [
        ":regression",
        "//beancount/utils:test_utils",
        "//beancount/parser:parser",
        "//beancount/ingest:importer",
    ],
)

package(default_visibility = ["//visibility:public"])

py_library(
    name = "bisect_key",
    srcs = ["bisect_key.py"],
)

py_test(
    name = "bisect_key_test",
    srcs = ["bisect_key_test.py"],
    deps = [":bisect_key"],
)

py_library(
    name = "date_utils",
    srcs = ["date_utils.py"],
    deps = [],
)

py_test(
    name = "date_utils_test",
    srcs = ["date_utils_test.py"],
    deps = [
        ":date_utils",
    ],
)

py_library(
    name = "defdict",
    srcs = ["defdict.py"],
)

py_test(
    name = "defdict_test",
    srcs = ["defdict_test.py"],
    deps = [
        ":defdict",
    ],
)

py_library(
    name = "encryption",
    srcs = ["encryption.py"],
)

py_test(
    name = "encryption_test",
    srcs = ["encryption_test.py"],
    deps = [
        ":encryption",
        ":test_utils",
    ],
)

py_library(
    name = "file_utils",
    srcs = ["file_utils.py"],
)

py_test(
    name = "file_utils_test",
    srcs = ["file_utils_test.py"],
    deps = [
        ":file_utils",
        ":test_utils",
    ],
)

py_library(
    name = "import_utils",
    srcs = ["import_utils.py"],
    deps = [
    ],
)

py_test(
    name = "import_utils_test",
    srcs = ["import_utils_test.py"],
    deps = [
        ":import_utils",
        ":defdict",
    ],
)

py_library(
    name = "invariants",
    srcs = ["invariants.py"],
)

py_test(
    name = "invariants_test",
    srcs = ["invariants_test.py"],
    deps = [":invariants"],
)

py_library(
    name = "memo",
    srcs = ["memo.py"],
)

py_test(
    name = "memo_test",
    srcs = ["memo_test.py"],
    deps = [
        ":memo",
    ],
)

py_library(
    name = "misc_utils",
    srcs = ["misc_utils.py"],
)

py_test(
    name = "misc_utils_test",
    srcs = ["misc_utils_test.py"],
    deps = [
        ":misc_utils",
        ":test_utils",
    ],
)

py_library(
    name = "pager",
    srcs = ["pager.py"],
)

py_test(
    name = "pager_test",
    srcs = ["pager_test.py"],
    deps = [
        ":pager",
        ":test_utils",
    ],
)

py_library(
    name = "regexp_utils",
    srcs = ["regexp_utils.py"],
)

py_test(
    name = "regexp_utils_test",
    srcs = ["regexp_utils_test.py"],
    deps = [":regexp_utils"],
)

py_library(
    name = "snoop",
    srcs = ["snoop.py"],
)

py_test(
    name = "snoop_test",
    srcs = ["snoop_test.py"],
    deps = [
        ":snoop",
    ],
)

py_library(
    name = "test_utils",
    srcs = ["test_utils.py"],
)

py_test(
    name = "test_utils_test",
    srcs = ["test_utils_test.py"],
    deps = [":test_utils"],
)

py_library(
    name = "text_utils",
    srcs = ["text_utils.py"],
)

py_test(
    name = "text_utils_test",
    srcs = ["text_utils_test.py"],
    deps = [
        ":text_utils",
    ],
)

py_library(
    name = "table",
    srcs = ["table.py"],
)

py_test(
    name = "table_test",
    srcs = ["table_test.py"],
    deps = [
        ":table",
    ],
)

py_library(
    name = "scrape",
    srcs = ["scrape.py"],
    deps = [
        # Add lxml here?
    ],
)

py_test(
    name = "scrape_test",
    srcs = ["scrape_test.py"],
    deps = [
        ":test_utils",
        ":scrape",
    ],
)

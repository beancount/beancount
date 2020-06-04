package(default_visibility = ["//visibility:public"])

filegroup(
    name = "office_files",
    srcs = glob(["**"]),
)

py_library(
    name = "office",
    srcs = [
        "office/importers/utrade/utrade_csv_test.py",
        "office/importers/utrade/utrade_csv.py",
        "office/importers/conftest.py",
        "office/importers/acme/acme_pdf.py",
        "office/importers/acme/acme_pdf_test.py",
    ],
    deps = [
        "//beancount/ingest/importers:ofx"
    ],
)

py_library(
    name = "utrade_csv",
    srcs = [
        "//beancount/core:number",
        "//beancount/core:data",
        "//beancount/core:account",
        "//beancount/core:amount",
        "//beancount/core:position",
        "//beancount/ingest:importer",
    ],
)

py_library(
    name = "acme_pdf",
    srcs = [
        "//beancount/ingest:importer",
    ],
)

py_library(
    name = "example_import",
    srcs = ["office/example_import.py"],
    deps = [
        ":utrade_csv",
        ":acme_pdf",
        "//beancount/ingest:extract",
        "//beancount/ingest/importers:ofx",
    ],
)

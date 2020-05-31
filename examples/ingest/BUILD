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

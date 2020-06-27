# alias(
#     name = "@protobuf_archive",
#     actual = "@com_google_protobuf"
# )

cc_binary(
    name = "riegeli_print",
    srcs = ["riegeli_print.cc"],
    deps = [
        "@com_google_riegeli//riegeli/base",
        "@com_google_riegeli//riegeli/base:options_parser",
        "@com_google_riegeli//riegeli/base:str_error",
        "@com_google_riegeli//riegeli/bytes:fd_reader",
        "@com_google_riegeli//riegeli/bytes:writer_utils",
        "@com_google_riegeli//riegeli/records:chunk_reader",
        "@com_google_riegeli//riegeli/records:record_reader",
        "@com_google_absl//absl/base:core_headers",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/strings:str_format",
        "//experiments/protos:beancount_cc_proto"
    ],
)

# alias(
#     name = "@protobuf_archive",
#     actual = "@com_google_protobuf"
# )

# Broken by changes in riegeli which require a patch to protobuf's own rules. {cc05788f14ff}
## cc_binary(
##     name = "riegeli_print",
##     srcs = ["riegeli_print.cc"],
##     deps = [
##         "@com_google_riegeli//riegeli/base",
##         "@com_google_riegeli//riegeli/bytes:fd_reader",
##         "@com_google_riegeli//riegeli/records:record_reader",
##         "@com_google_absl//absl/base:core_headers",
##         "@com_google_absl//absl/strings",
##         "@com_google_absl//absl/status",
##         "@com_google_absl//absl/strings:str_format",
##         "//beancount/ccore:data_cc_proto"
##     ],
## )

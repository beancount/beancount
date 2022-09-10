filegroup(name = "all", srcs = glob(["**"]), visibility = ["//visibility:public"])
filegroup(
    name = "bison_runtime_data",
    srcs = glob(["data/**/*"]),
    output_licenses = ["unencumbered"],
    path = "data",
    visibility = ["//visibility:public"],

)
exports_files(["data"])

# TODO(blais): This file is incomplete.

package(default_visibility = ["//visibility:public"])

py_library(
    name = "config",
    srcs = ["config.py"],
    deps = [
    ],
)

py_test(
    name = "config_test",
    srcs = ["config_test.py"],
    deps = [
        "//beancount/ingest:importer",
        ":config",
    ],
)

# # TODO(blais): This doesn't work, absolute imports appear not to work properly in Bazel.
# py_library(
#     name = "csv-lib",
#     srcs = ["csv.py"],
#     deps = [
#         "//beancount/core:data",
#         "//beancount/core:amount",
#         "//beancount/core:number",
#         "//beancount/ingest/importers/mixins:filing",
#         "//beancount/ingest/importers/mixins:identifier",
#         "//beancount/utils:date_utils",
#     ],
# )

# py_test(
#     name = "csv_test",
#     srcs = ["csv_test.py"],
#     deps = [
#         ":csv-lib",
#         "//beancount/ingest:cache",
#         "//beancount/parser:cmptest",
#         "//beancount/utils:test_utils",
#     ],
# )



# py_library(
#     name = "importers",
#     srcs = [
#         "fileonly.py",
#         "ofx.py",
#     ],
#     deps = [
#     ],
# )

  # -rw-r--r-- 1 blais blais  2798 May 22 12:47 config.py
  # -rw-r--r-- 1 blais blais  1654 May 23 09:23 config_test.py
  # -rw-r--r-- 1 blais blais 14286 May 23 09:55 csv.py
  # -rw-r--r-- 1 blais blais  9832 May 23 09:23 csv_test.py
  # -rw-r--r-- 1 blais blais   463 May 22 12:47 fileonly.py
  # -rw-r--r-- 1 blais blais  1394 May 23 09:23 fileonly_test.py
  # -rw-r--r-- 1 blais blais     0 May 22 12:47 __init__.py
  # drwxr-xr-x 2 blais blais  4096 May 23 16:51 mixins
  # -rw-r--r-- 1 blais blais 10681 May 23 09:23 ofx.py
  # -rw-r--r-- 1 blais blais 18051 May 23 09:23 ofx_test.py

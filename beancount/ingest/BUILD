# TODO(blais): This file is incomplete.

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

  # -rw-r--r--  1 blais blais  4038 May 22 12:47 cache.py
  # -rw-r--r--  1 blais blais  1691 May 23 09:23 cache_test.py
  # -rw-r--r--  1 blais blais  9947 May 23 09:23 extract.py
  # -rw-r--r--  1 blais blais 14306 May 23 09:23 extract_test.py
  # -rw-r--r--  1 blais blais 12091 May 23 09:23 file.py
  # -rw-r--r--  1 blais blais 17072 May 23 09:23 file_test.py
  # -rw-r--r--  1 blais blais  3757 May 23 09:23 identify.py
  # -rw-r--r--  1 blais blais  5313 May 23 09:23 identify_test.py
  # drwxr-xr-x  3 blais blais  4096 May 23 16:51 importers
  # -rw-r--r--  1 blais blais   655 May 22 12:47 __init__.py
  # -rw-r--r--  1 blais blais  9269 May 22 12:47 regression.py
  # -rw-r--r--  1 blais blais  5957 May 22 12:47 regression_pytest.py
  # -rw-r--r--  1 blais blais  7351 May 23 09:23 regression_test.py
  # -rw-r--r--  1 blais blais 16582 May 22 12:47 scripts_utils.py
  # -rw-r--r--  1 blais blais  4485 May 23 09:23 scripts_utils_test.py
  # -rw-r--r--  1 blais blais  5550 May 22 12:47 similar.py
  # -rw-r--r--  1 blais blais  5796 May 23 09:23 similar_test.py

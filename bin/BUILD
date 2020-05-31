# TODO(blais): This file is incomplete.

package(default_visibility = ["//visibility:public"])

# Note: The genrule targets rename the scripts to .py generated files.
# Note: subpar does not yet support C extensions.

genrule(
    name = "bean_check_py",
    srcs = [
        "bean-check",
    ],
    outs = ["bean_check.py"],
    cmd = "cat $(locations :bean-check) > $@",
)

py_binary(
    name = "bean_check",
    srcs = ["bean_check.py"],
    deps = [
        "//beancount/scripts:check",
        "//beancount:loader_with_plugins",
    ],
)

genrule(
    name = "bean_bake_py",
    srcs = [
        "bean-bake",
    ],
    outs = ["bean_bake.py"],
    cmd = "cat $(locations :bean-bake) > $@",
)

py_binary(
    name = "bean_bake",
    srcs = ["bean_bake.py"],
    deps = [
        "//beancount/scripts:bake",
        "//beancount:loader_with_plugins",
    ],
)

genrule(
    name = "bean_doctor_py",
    srcs = [
        "bean-doctor",
    ],
    outs = ["bean_doctor.py"],
    cmd = "cat $(locations :bean-doctor) > $@",
)

py_binary(
    name = "bean_doctor",
    srcs = ["bean_doctor.py"],
    deps = [
        "//beancount/scripts:doctor",
        "//beancount:loader_with_plugins",
    ],
)

genrule(
    name = "bean_example_py",
    srcs = [
        "bean-example",
    ],
    outs = ["bean_example.py"],
    cmd = "cat $(locations :bean-example) > $@",
)

py_binary(
    name = "bean_example",
    srcs = ["bean_example.py"],
    deps = [
        "//beancount/scripts:example",
        "//beancount:loader_with_plugins",
    ],
)



  # -rwxr-xr-x  1 blais blais  155 May 22 12:47 bean-extract
  # -rwxr-xr-x  1 blais blais  152 May 22 12:47 bean-file
  # -rwxr-xr-x  1 blais blais  155 May 22 12:47 bean-format
  # -rwxr-xr-x  1 blais blais  156 May 22 12:47 bean-identify
  # -rwxr-xr-x  1 blais blais  153 May 22 12:47 bean-price
  # -rwxr-xr-x  1 blais blais  152 May 22 12:47 bean-query
  # -rwxr-xr-x  1 blais blais  155 May 22 12:47 bean-report
  # -rwxr-xr-x  1 blais blais  152 May 22 12:47 bean-sql
  # -rwxr-xr-x  1 blais blais  148 May 22 12:47 bean-web
  # -rw-r--r--  1 blais blais  381 May 30 22:20 BUILD
  # -rwxr-xr-x  1 blais blais  148 May 22 12:47 treeify
  # -rwxr-xr-x  1 blais blais  154 May 22 12:47 upload-to-sheets

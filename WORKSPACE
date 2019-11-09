# Bazel workspace definition.

workspace(name="beancount")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

# For local Python installation / header-files configuration.
load("//tools/build:python_configure.bzl", "python_configure")
python_configure(name = "local_config_python")
register_toolchains("@local_config_python//:toolchain")



# Install python-magic external dependency.
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
http_archive(
    name = "python_magic",
    url = "https://github.com/ahupp/python-magic/archive/8753c3cf284c1b83eaf0dd0f520400ec7652ee81.zip",
    sha256 = "caacf286e21ba3d7228f1b39d39604c75927a00db136df924cd78bfa0d6043a0",

    # FIXME: Report, if you use this there's an error mentioning //external.
    #build_file = "BUILD.python_magic"

    build_file_content = """
py_library(
    name = "magic",
    srcs = ["magic.py"],
    visibility = ["//visibility:public"],
)
    """,
    strip_prefix = "python-magic-8753c3cf284c1b83eaf0dd0f520400ec7652ee81",
)



## See https://github.com/bazelbuild/rules_go/blob/0.19.0/go/workspace.rst#proto-dependencies
##
### load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")
### 
### git_repository(
###     name = "com_google_protobuf",
###     commit = "09745575a923640154bcf307fba8aedff47f240a",
###     remote = "https://github.com/protocolbuffers/protobuf",
###     shallow_since = "1558721209 -0700",
### )
### 
### load("@com_google_protobuf//:protobuf_deps.bzl", "protobuf_deps")
### 
### protobuf_deps()



### # Support pip repositories for requirements file.
### # See https://github.com/bazelbuild/rules_python/
### load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")
### git_repository(
###     name = "rules_python",
###     remote = "https://github.com/bazelbuild/rules_python.git",
###     # NOT VALID: Replace with actual Git commit SHA.
###     commit = "5aa465d5d91f1d9d90cac10624e3d2faf2057bd5",
### )
### load("@rules_python//python:repositories.bzl", "py_repositories")
### py_repositories()
### # Only needed if using the packaging rules.
### load("@rules_python//python:pip.bzl", "pip_repositories")
### pip_repositories()




# # https://stackoverflow.com/questions/45471120/how-do-i-tell-bazel-where-python-h-lives
# new_local_repository(
#     name = "python_linux",
#     path = "/usr/local",
#     build_file_content = """
# cc_library(
#     name = "python37-lib",
#     srcs = ["lib/python3.7/config-3.7m-x86_64-linux-gnu/libpython3.7m.a"],
#     hdrs = glob(["include/python3.7m/*.h"]),
#     includes = ["include/python3.7m"],
#     visibility = ["//visibility:public"]
# )
#     """
# )

# config_setting(
#     name = "linux_x86_64",
#     values = {"cpu": "k8"},
#     visibility = ["//visibility:public"],
# )

## cc_binary(
##     name="python-test",
##     srcs = [
##         "main.c",
##     ],
##     deps = select({
##         "//:linux_x86_64": [
##             "@python_linux//:python37-lib"
##         ],
##     })
## )


## The easy one is to build with --spawn_strategy=standalone (bazel build
## --spawn_strategy=standalone :pybridge). This tells Bazel not to use
## sandboxing for this build. Note that, as far as Bazel knows, nothing has
## changed between the sandboxed and non-sandboxed run, so you'll have to clean
## before re-running without sandboxing, or you'll just get the cached error.



## https://github.com/protocolbuffers/protobuf/blob/master/util/python/BUILD



## https://github.com/tensorflow/tensorflow/blob/ea5477fe59b761605c7f1d29fdedf6b1360fc3a9/tensorflow/workspace.bzl#L1004-L1008
## https://github.com/tensorflow/tensorflow/blob/master/third_party/python_runtime/BUILD



## # We don't really need tensorflow, but we use their trick to find the python
## # headers. (This is unfortunate, I wish there will be a better way at some
## # point).
## http_archive(
##     name = "org_tensorflow",
##     sha256 = "902a6d90bb69549fe241377210aa459773459820da1333b67dcfdef37836f25f",
##     strip_prefix = "tensorflow-1.13.1",
##     urls = [
##         "https://github.com/tensorflow/tensorflow/archive/v1.13.1.zip",
##     ],
## )
##
## load("@org_tensorflow//third_party/py:python_configure.bzl", "python_configure")
## python_configure(name="local_config_python")
##
## # TODO(blais): Replace by alias().
## bind(
##     name = "python_headers",
##     actual = "@org_tensorflow//third_party/python_runtime:headers",
## )
##
##

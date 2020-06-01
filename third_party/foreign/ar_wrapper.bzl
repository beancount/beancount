"""Wraps the command that the C++ toolchain uses for static libraries
with an interface which looks like 'ar'.

This is a helper / workaround for rules_foreign_cc.  By default,
rules_foreign_cc takes whatever tool the Bazel C++ toolchain
configures for linking static libraries, sets it to the AR environment
variable and invokes configure/make (or similar).

This is particularly useful for MacOS with Bazel 1.0+ which use
libtool (see https://github.com/bazelbuild/bazel/pull/9154).
"""

load(
    "@bazel_tools//tools/build_defs/cc:action_names.bzl",
    "CPP_LINK_STATIC_LIBRARY_ACTION_NAME",
)
load("@bazel_tools//tools/cpp:toolchain_utils.bzl", "find_cpp_toolchain")

def _generate_ar_wrapper_impl(ctx):
    output_file = "${OUTPUT}"

    cc_toolchain = find_cpp_toolchain(ctx)

    feature_configuration = cc_common.configure_features(
        ctx = ctx,
        cc_toolchain = cc_toolchain,
        requested_features = ctx.features,
        unsupported_features = ctx.disabled_features,
    )

    variables = cc_common.create_link_variables(
        cc_toolchain = cc_toolchain,
        feature_configuration = feature_configuration,
        is_using_linker = False,
        is_linking_dynamic_library = False,
        output_file = output_file,
    )

    tool = cc_common.get_tool_for_action(
        feature_configuration = feature_configuration,
        action_name = CPP_LINK_STATIC_LIBRARY_ACTION_NAME,
    )

    env = cc_common.get_environment_variables(
        feature_configuration = feature_configuration,
        action_name = CPP_LINK_STATIC_LIBRARY_ACTION_NAME,
        variables = variables,
    )

    env_str = "\n".join([
        "export %s='%s'" % (k, v)
        for (k, v) in env.items()
    ])

    if tool[0] != "/":
        tool = "${EXT_BUILD_ROOT}/" + tool

    args = cc_common.get_memory_inefficient_command_line(
        feature_configuration = feature_configuration,
        action_name = CPP_LINK_STATIC_LIBRARY_ACTION_NAME,
        variables = variables,
    )

    command = " ".join([tool] + args)

    output = ctx.outputs.out

    ctx.actions.write(
        output = output,
        content = """#!/bin/bash
set -euo pipefail

echo "ar_wrapper: $@"
in_flags=1
have_operation=0
while [[ ${in_flags} = 1 ]]; do
  if [[ $# -eq 0 ]]; then
    >&2 echo "missing args (have_operation=${have_operation})"
    exit 1
  fi
  case "${have_operation}:$1" in
    0:-*[dmpxt]* | 0:*[dmpxt]*)
      >&2 echo "delete/move/print/extract/table operations are not supported"
      exit 1
      ;;
    0:-*[qr]* | 0:*[qr]*)
      have_operation=1
      shift
      ;;
    1:-*[dmpxtqr]*)
      >&2 echo "duplicate operation"
      exit 1
      ;;
    [01]:-*)
      # ignore all other flags
      shift
      ;;
    1:*)
      # Done parsing args...
      in_flags=0
      ;;
    *)
      >&2 echo "unexpected arg $1 (have_operation=${have_operation})"
      exit 1
      ;;
  esac
done

OUTPUT="$1"
shift
##env##
##command## "$@"
        """.replace("##command##", command).replace("##env##", env_str),
    )
    return [DefaultInfo(files = depset([output]))]

generate_ar_wrapper = rule(
    implementation = _generate_ar_wrapper_impl,
    attrs = {
        "out": attr.output(mandatory = True),
        "_cc_toolchain": attr.label(
            default = Label("@bazel_tools//tools/cpp:current_cc_toolchain"),
        ),
    },
    toolchains = [
        "@bazel_tools//tools/cpp:toolchain_type",
    ],
    fragments = ["cpp"],
)

def ar_wrapper(name, visibility = None):
    generate_ar_wrapper(
        name = "generate_" + name,
        out = name + ".sh",
    )

    native.sh_binary(
        name = name,
        srcs = [name + ".sh"],
        visibility = visibility,
    )

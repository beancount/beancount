"""Build rule for generating C or C++ sources with RE-Flex.

Examples
--------

This is a simple example.
```
genreflex(
    name = "html_lex_lex",
    src = "html.lex",
    out = "html_lexer.c",
)
```

This example uses a `.tab.hh` file.
```
genreflex(
    name = "rules_l",
    src = "rules.lex",
    includes = [
        "rules.tab.hh",
    ],
    out = "rules.yy.cc",
)
```
"""

load("@rules_m4//m4:m4.bzl", "M4_TOOLCHAIN_TYPE", "m4_toolchain")

def _genreflex_impl(ctx):
    """Implementation for genreflex rule."""

    # Construct the arguments.
    args = ctx.actions.args()
    args.add("-o", ctx.outputs.out)

    if ctx.outputs.header_out:
        args.add("--header-file=%s" % ctx.outputs.header_out.path)
    if ctx.outputs.graphs_out:
        args.add("--graphs-file=%s" % ctx.outputs.graphs_out.path)
    if ctx.outputs.regexp_out:
        args.add("--regexp-file=%s" % ctx.outputs.regexp_out.path)
    if ctx.outputs.tables_out:
        args.add("--tables-file=%s" % ctx.outputs.tables_out.path)

    # Compute the prefix, if not specified.
    if ctx.attr.prefix:
        args.add("-P", ctx.attr.prefix)

    args.add("--bison-complete")
    args.add("--bison-locations")
    args.add_all(ctx.attr.lexopts)
    args.add(ctx.file.src)

    if ctx.attr.debug:
        args.add("-d")

    m4 = m4_toolchain(ctx)
    ctx.actions.run(
        executable = ctx.executable._reflex,
        env = {
            "M4": m4.m4_tool.executable.path,
        },
        arguments = [args],
        inputs = ctx.files.src + ctx.files.includes,
        tools = [m4.m4_tool.executable],
        outputs = [ctx.outputs.out,
                   ctx.outputs.header_out],
        mnemonic = "ReFlex",
        progress_message = "Generating %s from %s" % (
            ctx.outputs.out.short_path,
            ctx.file.src.short_path,
        ),
    )

genreflex = rule(
    implementation = _genreflex_impl,
    doc = "Generate C/C++-language sources from a lex file using RE-Flex.",
    attrs = {
        "src": attr.label(
            mandatory = True,
            allow_single_file = [".l", ".ll", ".lex", ".lpp", ".lxx"],
            doc = "The .lex source file for this rule",
        ),
        "includes": attr.label_list(
            allow_files = True,
            doc = "A list of headers that are included by the .lex file",
        ),

        "debug": attr.bool(
            doc = "A flag to enable debugging output."),

        "out": attr.output(
            mandatory = True,
            doc = "The generated source file"),
        "header_out": attr.output(
            mandatory = False,
            doc = "A C++ header FILE.h in addition to the scanner"),
        "graphs_out": attr.output(
            mandatory = False,
            doc = "The scanner's DFA in Graphviz format to FILE.gv"),
        "regexp_out": attr.output(
            mandatory = False,
            doc = "The scanner's regular expression patterns to FILE.txt"),
        "tables_out": attr.output(
            mandatory = False,
            doc = "The scanner's FSM opcode tables or FSM code to FILE.cpp"),

        "prefix": attr.string(
            doc = "External symbol prefix for Flex. This string is " +
                  "passed to flex as the -P option, causing the resulting C " +
                  "file to define external functions named 'prefix'text, " +
                  "'prefix'in, etc.  The default is the basename of the source" +
                  "file without the .lex extension.",
        ),
        "lexopts": attr.string_list(
            doc = "A list of options to be added to the flex command line.",
        ),
        "_reflex": attr.label(
            default = Label("//third_party/parser:reflex_bin"),
            executable = True,
            cfg = "host",
        ),
    },
    output_to_genfiles = True,
    toolchains = [M4_TOOLCHAIN_TYPE],
)

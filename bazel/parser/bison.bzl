"""Build rule for generating C or C++ sources with Bison.

Notes about compiling libraries using bison: Make sure to use

  cc_library(
    ...
    copts = [
        "-Wno-implicit-fallthrough",
        "-O2",
        "-Wno-sign-compare",
    ],

The Bison skeleton uses implicit fallthrough, so libraries won't compile unless
we turn the 'no-implicit-fallthrough' warning off. The -O2 is required because
the generated bison parser uses a ton of stack space in non-optimized builds.
The entire parser is contained in a single giant function, and non-optimized
compilation does not reuse stack slots for local variables. Without -O2, clients
are likely to fail in fastbuild with default 64KB stack sizes, or to start
failing when the parser is extended further.

"""

def _genyacc_impl(ctx):
    """Implementation for genyacc rule."""

    # Argument list
    args = ctx.actions.args()
    args.add("--defines=%s" % ctx.outputs.header_out.path)
    if ctx.outputs.location_out:
        args.add('--define=api.location.file="%s"' % ctx.outputs.location_out.basename)
        args.add('--define=api.location.include=""%s""' % ctx.outputs.location_out.short_path)
    args.add("--output-file=%s" % ctx.outputs.source_out.path)
    if ctx.attr.prefix:
        args.add("--name-prefix=%s" % ctx.attr.prefix)
    args.add_all([ctx.expand_location(opt) for opt in ctx.attr.extra_options])
    args.add(ctx.file.src.path)

    # Output files
    outputs = ctx.outputs.extra_outs + [
        ctx.outputs.header_out,
        ctx.outputs.source_out,
    ]
    if ctx.outputs.location_out:
        outputs.append(ctx.outputs.location_out)

    ctx.actions.run(
        executable = ctx.executable._bison,
        env = {
            "M4": ctx.executable._m4.path,
            "BISON_PKGDATADIR": ctx.files._bison_data[0].dirname,
        },
        arguments = [args],
        inputs = ctx.files._bison_data + ctx.files.src,
        tools = [ctx.executable._m4],
        outputs = outputs,
        mnemonic = "Yacc",
        progress_message = "Generating %s and %s from %s" %
                           (
                               ctx.outputs.source_out.short_path,
                               ctx.outputs.header_out.short_path,
                               ctx.file.src.short_path,
                           ),
    )

genyacc = rule(
    implementation = _genyacc_impl,
    doc = "Generate C/C++-language sources from a Yacc file using Bison.",
    attrs = {
        "src": attr.label(
            mandatory = True,
            allow_single_file = [".y", ".yy", ".yc", ".ypp", ".yxx"],
            doc = "The .y, .yy, or .yc source file for this rule",
        ),
        "header_out": attr.output(
            mandatory = True,
            doc = "The generated 'defines' header file",
        ),
        "location_out": attr.output(
            mandatory = False,
            doc = "The generated 'location' header file",
        ),
        "source_out": attr.output(mandatory = True, doc = "The generated source file"),
        "prefix": attr.string(
            doc = "External symbol prefix for Bison. This string is " +
                  "passed to bison as the -p option, causing the resulting C " +
                  "file to define external functions named 'prefix'parse, " +
                  "'prefix'lex, etc. instead of yyparse, yylex, etc.",
        ),
        "extra_outs": attr.output_list(doc = "A list of extra generated output files."),
        "extra_options": attr.string_list(
            doc = "A list of extra options to pass to Bison.  These are " +
                  "subject to $(location ...) expansion.",
        ),
        "_bison_data": attr.label(default = "@bison//:bison_runtime_data"),
        "_bison": attr.label(
            default = Label("@bison//:bison_bin"),
            executable = True,
            cfg = "host",
        ),
        "_m4": attr.label(
            default = Label("@m4//:m4_bin"),
            executable = True,
            cfg = "host",
        ),
    },
    output_to_genfiles = True,
)

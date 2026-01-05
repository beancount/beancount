module.exports = grammar({
    /*
     * From beancount grammar.y
     */

    name: "beancount",

    // Ensure we don't extract keywords from tokens
    word: ($) => $.identifier,

    inline: ($) => [
    ],

    conflicts: ($) => [
    ],

    externals: ($) => [
        $._stars,
        $._sectionend,
        $._eof,  // Basically just '\0', but allows multiple to be matched
    ],

    extras: ($) => [
        /( |\r|\t)+/,
    ],

    supertypes: $ => [
        $._entry,
        $._directive,
    ],

    rules: {
        file: $ => repeat(
            choice(
                $.section,
                $._declarations,
                $._nl,
            )
        ),

        _nl: _ => choice('\n', '\r'),
        _eol: $ => choice('\n', '\r', $._eof),
        _any: $ => /[^\r\n]*/,


        /*--------------------------------------------------------------------------------*/
        /* SECTION AND HEADLINE RULES (Org-mode/Markdown support) */
        /*--------------------------------------------------------------------------------*/
        section: $ => seq(
            field('headline', $.headline),
            repeat(choice(
                $._declarations,
                $._nl,
                field('subsection', $.section)
            )),
            $._sectionend
        ),
        _org_stars: $ => seq($._stars, /(\*|#)+/),
        headline: $ => seq(
            $._org_stars,
            token(/[ \t]+/), // tokenized for performance
            optional(field('item', $.item)),
            $._nl,
        ),
        item: $ => token(/[^\r\n]+/),


        /*--------------------------------------------------------------------------------*/
        /* TERMINAL SYMBOLS */
        /*--------------------------------------------------------------------------------*/
        _indent: $ => token(/[ \r\t]+/),
        _eol: $ => token(/\n/),
        atat: $ => token('@@'),
        at: $ => token('@'),
        asterisk: $ => token('*'),
        slash: $ => token('/'),
        plus: $ => token('+'),
        minus: $ => token('-'),
        flag: $ => token(/[!&?%PSTCURM*#]/),
        _none: $ => token('NULL'),
        bool: $ => token(/TRUE|FALSE/),
        date: $ => token(/([12]\d{3}[-\/](0[1-9]|1[0-2])[-\/](0[1-9]|[12]\d|3[01]))/),
        // Account names: Assets|Liabilities|Equity|Income|Expenses followed by colon-separated components
        // Components can contain Unicode letters/numbers including CJK characters
        account: $ =>
            token(
                seq(
                    /Assets|Liabilities|Equity|Income|Expenses/,
                    repeat1(
                        seq(
                            ":",
                            /[\p{Lu}\p{N}\u3040-\u309f\u30a0-\u30ff\u4e00-\u9fff\uac00-\ud7a3][\p{L}\p{N}\u3040-\u309f\u30a0-\u30ff\u4e00-\u9fff\uac00-\ud7a3\-]*/,
                        ),
                    ),
                ),
            ),
        currency: $ => token(/[A-Z]([A-Z0-9\'\._\-]{0,22}[A-Z0-9])?/),
        string: $ => token(/"([^"]|\\")*"/),
        number: $ => token(/([0-9]+|[0-9][0-9,]+[0-9])(\.[0-9]*)?/),
        tag: $ => token(/#[A-Za-z0-9\-_/.]+/),
        link: $ => token(/\^[A-Za-z0-9\-_/.]+/),

        /*--------------------------------------------------------------------------------*/
        /* ARITHMETIC EXPRESSIONS */
        /*--------------------------------------------------------------------------------*/
        // Operator precedence: PLUS/MINUS (left, 1), MULTIPLY/DIVIDE (left, 2), UNARY (3)

        txn: $ => choice(
            "txn",
            $.flag,
            "*",
            "#"
        ),

        _number_expr: $ =>
            choice(
                $.number,
                $._paren__number_expr,
                $.unary_number_expr,
                $.binary_number_expr,
            ),
        _paren__number_expr: $ =>
            seq(
                "(",
                $._number_expr,
                ")",
            ),
        unary_number_expr: $ =>
            prec(3,
                choice(
                    seq(
                        $.minus,
                        $._number_expr,
                    ),
                    seq(
                        $.plus,
                        $._number_expr,
                    ),
                ),
            ),
        binary_number_expr: $ =>
            prec(3,
                choice(
                    prec.left(1,
                        seq($._number_expr, $.plus, $._number_expr),
                    ),
                    prec.left(1,
                        seq($._number_expr, $.minus, $._number_expr),
                    ),
                    prec.left(2,
                        seq($._number_expr, $.asterisk, $._number_expr),
                    ),
                    prec.left(2,
                        seq($._number_expr, $.slash, $._number_expr),
                    ),
                ),
            ),

        // OPTIONAL
        _txn_strings: $ =>
            choice(
                seq(
                    field("payee", alias($.string, $.payee)),
                    field("narration", alias($.string, $.narration)),
                ),
                field("narration", alias($.string, $.narration)),
            ),

        // OPTIONAL
        tags_links: $ =>
            repeat1(
                //seq(
                //   optional($._indent),
                choice(
                    $.link,
                    $.tag,
                ),
                //),
            ),

        transaction: $ =>
            seq(
                field("date", $.date),
                field("txn", $.txn),
                optional($._txn_strings),
                field("tags_links", optional($.tags_links)),
                field("comment", optional($.comment)),
                $._eol,
                optional(
                    repeat1(
                        choice(
                            seq(
                                $._indent,
                                $._eol
                            ),
                            seq(
                                $._indent,
                                $.tags_links,
                                $._eol
                            ),
                            $._key_value_line,
                            $.posting,
                            seq(
                                $._indent,
                                field("comment", $.comment),
                                $._eol
                            )
                        )
                    ),
                ),

            ),

        // OPTIONAL
        optflag: $ =>
            choice(
                "*",
                "#",
                $.flag,
            ),

        price_annotation: $ => $.incomplete_amount,
        //choice(
        //    seq(
        //        $.atat,
        //        $.incomplete_amount
        //    ),
        //    seq(
        //        $.at,
        //        $.incomplete_amount
        //    )
        //),

        posting: $ =>
            seq(
                $._indent,
                field("optflag", optional($.optflag)),
                field("account", $.account),
                field("amount", optional($.incomplete_amount)),
                field("cost_spec", optional($.cost_spec)),
                optional(
                    seq(
                        choice($.at, $.atat),
                        field("price_annotation", optional($.price_annotation))
                    )
                ),
                field("comment", optional($.comment)),
                $._eol
            ),

        key: $ => token(/[a-z][a-zA-Z0-9\-_]+/),

        value: $ =>
            choice(
                $.string,
                $.account,
                $.date,
                $.currency,
                $.tag,
                $.bool,
                $._none,
                $._number_expr,
                $.amount
            ),

        key_value: $ =>
            prec.left(seq(
                $.key,
                ":",
                $.value,
            )),

        _key_value_line: $ => seq(
            $._indent,
            /*prec.left(seq(
                $.key,
                ":",
                $.value,
            )),*/
            $.key_value,
            $._eol
        ),

        // OPTIONAL
        _key_value_list: $ =>
            repeat1(
                choice(
                    seq(
                        $._indent,
                        $._eol
                    ),
                    seq(
                        $._key_value_line
                    ),
                    seq(
                        $._indent,
                        field("comment", $.comment),
                        $._eol
                    ),
                )
            ),

        // OPTIONAL

        pushtag: $ => seq(
            "pushtag",
            $.tag,
            $._eol
        ),

        poptag: $ => seq(
            "poptag",
            $.tag,
            $._eol
        ),

        pushmeta: $ => seq(
            "pushmeta",
            $.key_value,
            $._eol
        ),

        popmeta: $ => seq(
            "popmeta",
            $.key,
            ":",
            $._eol
        ),

        open: $ =>
            seq(
                field("date", $.date),
                "open",
                field("account", $.account),
                field("currencies", repeat(
                    seq(
                        $.currency,
                        repeat(
                            seq(
                                ",",
                                $.currency
                            )
                        )
                    ),
                )),
                field("opt_booking", optional($.opt_booking)),
                field("comment", optional($.comment)),
                $._eol,
                optional($._key_value_list)
            ),

        // OPTIONAL
        opt_booking: $ => $.string,

        close: $ =>
            seq(
                field("date", $.date),
                "close",
                field("account", $.account),
                field("comment", optional($.comment)),
                $._eol,
                optional($._key_value_list)
            ),

        commodity: $ =>
            seq(
                field("date", $.date),
                "commodity",
                field("currency", $.currency),
                field("comment", optional($.comment)),
                $._eol,
                optional($._key_value_list)
            ),

        pad: $ =>
            seq(
                field("date", $.date),
                "pad",
                field("account", $.account),
                field("from_account", $.account),
                field("comment", optional($.comment)),
                $._eol,
                optional($._key_value_list)
            ),

        balance: $ =>
            seq(
                field("date", $.date),
                "balance",
                field("account", $.account),
                field("amount",
                    //choice(
                    //    $.amount,
                    $.amount_tolerance,
                    //)
                ),
                field("comment", optional($.comment)),
                $._eol,
                optional($._key_value_list)
            ),

        amount: $ =>
            seq(
                $._number_expr,
                $.currency
            ),

        amount_tolerance: $ =>
            choice(
                seq(
                    $._number_expr,
                    $.currency
                ),
                seq(
                    $._number_expr,
                    "~",
                    $._number_expr,
                    $.currency
                )
            ),

        // Just Optional number_expr
        //maybe_number:

        // Just Optional currency
        //maybe_currency:

        compound_amount: $ =>
            choice(
                seq(
                    field("per", optional($._number_expr)),
                    field("currency", $.currency)
                ),
                seq(
                    field("per", $._number_expr),
                    field("currency", optional($.currency))
                ),
                seq(
                    field("per", optional($._number_expr)),
                    "#",
                    field("total", optional($._number_expr)),
                    field("currency", $.currency)
                ),
            ),

        // OPTIONAL
        // TODO may need a chice here
        incomplete_amount: $ =>
            seq(
                $._number_expr, // maybe_number
                $.currency      // maybe_currenecy
            ),

        // OPTIONAL
        cost_spec: $ =>
            choice(
                seq(
                    "{",
                    field("cost_comp_list", optional($._cost_comp_list)),
                    "}"
                ),
                seq(
                    "{{",
                    field("cost_comp_list", optional($._cost_comp_list)),
                    "}}"
                ),
            ),

        // OPTIONAL
        _cost_comp_list: $ =>
            seq(
                $.cost_comp,
                repeat(
                    seq(
                        ",",
                        $.cost_comp
                    )
                )
            ),

        cost_comp: $ =>
            choice(
                $.compound_amount,
                $.date,
                $.string,
                "*"
            ),

        price: $ =>
            seq(
                field("date", $.date),
                "price",
                field("currency", $.currency),
                field("amount", $.amount),
                field("comment", optional($.comment)),
                $._eol,
                optional($._key_value_list)
            ),

        event: $ =>
            seq(
                field("date", $.date),
                "event",
                field("type", $.string),
                field("desc", $.string),
                field("comment", optional($.comment)),
                $._eol,
                optional($._key_value_list)
            ),

        query: $ =>
            seq(
                field("date", $.date),
                "query",
                field("name", $.string),
                field("query", $.string),
                field("comment", optional($.comment)),
                $._eol,
                optional($._key_value_list)
            ),

        note: $ =>
            seq(
                field("date", $.date),
                "note",
                field("account", $.account),
                field("note", $.string),
                field("comment", optional($.comment)),
                $._eol,
                optional($._key_value_list)
            ),

        filename: $ => $.string,

        document: $ =>
            seq(
                field("date", $.date),
                "document",
                field("account", $.account),
                field("filename", $.filename),
                field("tags_links", optional($.tags_links)),
                field("comment", optional($.comment)),
                $._eol,
                optional($._key_value_list)
            ),

        custom_value: $ =>
            choice(
                $.string,
                $.date,
                $.bool,
                $.amount,
                $._number_expr,
                $.account
            ),


        custom: $ =>
            seq(
                field("date", $.date),
                "custom",
                field("name", $.string),
                field("custom_value_list", optional(
                    repeat1(
                        $.custom_value
                    ),
                )),
                field("comment", optional($.comment)),
                $._eol,
                optional($._key_value_list)
            ),

        _entry: $ =>
            choice(
                $.transaction,
                $.balance,
                $.open,
                $.close,
                $.pad,
                $.document,
                $.note,
                $.event,
                $.price,
                $.commodity,
                $.query,
                $.custom,
            ),

        option: $ => seq(
            "option",
            field("key", $.string),
            field("value", $.string),
            $._eol,
        ),

        include: $ => seq(
            "include",
            $.string,
            $._eol,
        ),

        plugin: $ =>
            choice(
                seq(
                    "plugin",
                    $.string,
                    $._eol
                ),
                seq(
                    "plugin",
                    $.string,
                    $.string,
                    $._eol
                ),
            ),

        _directive: $ =>
            choice(
                $.pushtag,
                $.poptag,
                $.pushmeta,
                $.popmeta,
                $.option,
                $.include,
                $.plugin
            ),

        _declarations: $ => choice(
            $._directive,
            $._entry,
            $._skipped_lines,
        ),

        /*--------------------------------------------------------------------------------*/
        /* UTILITY AND LEXICAL RULES */
        /*--------------------------------------------------------------------------------*/

        comment: $ => token(seq(';', /[^\r\n]*/)),

        // NOTE: includes reserved identifiers
        // Allow uppercase letters to support incomplete account names for LSP completion
        identifier: $ => /[a-zA-Z]+/,

        _skipped_lines: $ =>
            choice(
                seq(
                    $.flag,
                    /[^\r\n]*/,
                    $._eol
                ),
                seq(
                    ":",
                    /[^\r\n]*/,
                    $._eol
                ),
                $._eol,
                seq(
                    $.comment,
                    $._eol
                ),
            ),

    }
})

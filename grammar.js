/** globals grammar, $, repeat, choice, seq *$ /

/**
 * Resources:
 * * https://github.com/tree-sitter/tree-sitter-c/blob/master/grammar.js
 * * https://github.com/tree-sitter/tree-sitter-javascript/blob/master/grammar.js
 */

/** @type {import("tree-sitter-cli/dsl").grammar} */
module.exports = grammar({
  name: 'vcl',

  extras: ($) => [/\s+/, $.COMMENT],

  precedences: ($) => [['member', 'call']],

  rules: {
    source_file: ($) => repeat($.toplev_declaration),

    // See: https://github.com/tree-sitter/tree-sitter-c/blob/f35789006ccbe5be8db21d1a2dd4cc0b5a1286f2/grammar.js#L1004-L1011
    COMMENT: ($) =>
      token(
        choice(
          seq('#', /.*/),
          seq('//', /.*/),
          seq('/*', /[^*]*\*+([^/*][^*]*\*+)*/, '/')
        )
      ),

    // See: https://github.com/varnishcache/varnish-cache/blob/a3bc025c2df28e4a76e10c2c41217c9864e9963b/lib/libvcc/vcc_parse.c#L353-L358
    toplev_declaration: ($) =>
      choice(
        $.acl_declaration,
        $.sub_declaration,
        $.backend_declaration,
        $.probe_declaration,
        $.import_declaration,
        $.vcl_version_declaration
      ),
    acl_declaration: ($) =>
      seq('acl', field('ident', $.ident), '{', repeat($.acl_entry), '}'),
    sub_declaration: ($) =>
      seq('sub', field('ident', $.ident), '{', repeat($.stmt), '}'),
    backend_declaration: ($) =>
      seq(
        'backend',
        field('ident', $.ident),
        '{',
        repeat($.backend_property),
        '}'
      ), // TODO:
    probe_declaration: ($) =>
      seq('probe', field('ident', $.ident), '{', repeat($.string), '}'),
    import_declaration: ($) =>
      seq(
        'import',
        field('ident', field('ident', $.ident)),
        optional(seq('from', $.string)),
        ';'
      ),
    vcl_version_declaration: ($) => seq('vcl', $.float, ';'),

    backend_property: ($) => seq('.', $.ident, '=', $.expr, ';'),
    acl_entry: ($) => seq($.string, optional(seq('/', $.literal)), ';'),

    stmt: ($) =>
      choice(
        $.if_stmt,
        $.set_stmt,
        $.new_stmt,
        $.unset_stmt,
        $.call_stmt,
        $.ident_call_stmt,
        $.ret_stmt
      ),

    if_stmt: ($) =>
      seq(
        'if',
        field('condition', $.parenthesized_expression),
        '{',
        field('consequence', repeat($.stmt)),
        '}',
        optional(choice($.elsif_stmt, $.else_stmt))
      ),
    elsif_stmt: ($) =>
      seq(
        choice('else if', 'elsif'),
        $.parenthesized_expression,
        '{',
        repeat($.stmt),
        '}'
      ),
    else_stmt: ($) => seq('else', '{', repeat($.stmt), '}'),
    call_stmt: ($) => seq('call', field('ident', $.ident), ';'), // subroutine call expr (e.g. «call strip_query_params;»)
    idnt_call_expr: ($) =>
      prec(
        'call',
        seq(
          field('ident', $.nested_ident),
          '(',
          repeat(seq($.expr, repeat(seq(',', $.expr)))),
          ')'
        )
      ), // function call expr (e.g. «if (querystring.get("")) {}»)
    ident_call_stmt: ($) => seq($.ident_call_expr, ';'), // function call statement (e.g. «var.global_set("a", "b");» )

    ret_stmt: ($) =>
      seq('return', '(', $.varnish_internal_return_methods, ')', ';'),
    new_stmt: ($) => seq('new', field('ident', $.ident), '=', $.expr, ';'),
    set_stmt: ($) => seq('set', $.nested_ident, '=', $.expr, ';'),
    unset_stmt: ($) => seq('unset', $.nested_ident, ';'),
    expr: ($) =>
      choice(
        $.literal,
        $.ident_call_expr,
        prec('member', $.nested_ident),
        // $.boolean_exprs,
        $.parenthesized_expression,
        $.binary_expression
      ),
    parenthesized_expression: ($) => seq('(', $.expr, ')'),

    binary_expression: ($) =>
      choice(
        prec.left(
          seq(
            field('left', $.expr),
            field('operator', $.comp_op),
            field('right', $.expr)
          )
        )
      ),

    bool: ($) => choice('true', 'false'),
    eq: ($) => '==',
    ne: ($) => '!=',
    _match: ($) => '~',
    _nmatch: ($) => '!~',
    g: ($) => '>',
    l: ($) => '<',
    ge: ($) => '>=',
    le: ($) => '<=',
    not: ($) => '!',
    or: ($) => '||',
    and: ($) => '&&',

    comp_op: ($) =>
      choice(
        $.and,
        $.or,
        $.eq,
        $.ne,
        $._match,
        $._nmatch,
        $.not,
        $.g,
        $.l,
        $.ge,
        $.le
      ),

    literal: ($) => choice($.string, $.number),
    string: ($) => seq('"', /[^"]*/, '"'),
    number: ($) => /\d+/,
    float: ($) => /\d+\.\d+/,

    ident: ($) => /\w+/,
    nested_ident: ($) => seq($.ident, repeat(seq('.', $.ident))),
    varnish_internal_return_methods: ($) =>
      seq(
        choice('hit', 'pass', 'retry', 'restart', 'fail', 'synth', 'hash'),
        optional(seq('(', repeat(seq($.expr, repeat(seq(',', $.expr)))), ')'))
      ),
  },
});

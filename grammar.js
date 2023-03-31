/** globals grammar, $, repeat, choice, seq *$ /

/**
 * Resources:
 * * https://github.com/tree-sitter/tree-sitter-c/blob/master/grammar.js
 * * https://github.com/tree-sitter/tree-sitter-javascript/blob/master/grammar.js
 */

/** @type {import("tree-sitter-cli/dsl").grammar} */
module.exports = grammar({
  name: 'vcl',

  extras: $ => [/\s+/, $.COMMENT],
  word: $ => $.ident,

  precedences: () => [['member', 'call']],

  rules: {
    source_file: $ => repeat($.toplev_declaration),

    // See: https://github.com/tree-sitter/tree-sitter-c/blob/f35789006ccbe5be8db21d1a2dd4cc0b5a1286f2/grammar.js#L1004-L1011
    COMMENT: () =>
      token(
        choice(
          seq('#', /.*/),
          seq('//', /.*/),
          seq('/*', /[^*]*\*+([^/*][^*]*\*+)*/, '/'),
        ),
      ),

    // See: https://github.com/varnishcache/varnish-cache/blob/a3bc025c2df28e4a76e10c2c41217c9864e9963b/lib/libvcc/vcc_parse.c#L353-L358
    toplev_declaration: $ =>
      choice(
        $.acl_declaration,
        $.sub_declaration,
        $.backend_declaration,
        $.probe_declaration,
        $.import_declaration,
        $.vcl_version_declaration,
      ),
    acl_declaration: $ =>
      seq('acl', field('ident', $.ident), '{', repeat($.acl_entry), '}'),
    sub_declaration: $ =>
      seq('sub', field('ident', $.ident), '{', repeat($.stmt), '}'),
    backend_declaration: $ =>
      seq(
        'backend',
        field('ident', $.ident),
        choice(seq('{', repeat($.backend_property), '}'), seq('none', ';')),
      ),
    probe_declaration: $ =>
      seq(
        'probe',
        field('ident', $.ident),
        '{',
        repeat($.backend_property),
        '}',
      ),
    import_declaration: $ =>
      seq(
        'import',
        field('ident', field('ident', $.ident)),
        optional(seq('from', $.string)),
        ';',
      ),
    vcl_version_declaration: $ => seq('vcl', $.float, ';'),

    backend_property: $ => seq('.', $.ident, '=', $.expr, ';'),
    acl_entry: $ => seq($.string, optional(seq('/', $.literal)), ';'),

    stmt: $ =>
      choice(
        $.if_stmt,
        $.set_stmt,
        $.new_stmt,
        $.unset_stmt,
        $.call_stmt,
        $.ident_call_stmt,
        $.ret_stmt,
      ),

    if_stmt: $ =>
      seq(
        'if',
        field('condition', $.parenthesized_expression),
        '{',
        field('consequence', repeat($.stmt)),
        '}',
        repeat($.elsif_stmt),
        optional($.else_stmt),
      ),
    elsif_stmt: $ =>
      seq(
        choice('else if', 'elsif', 'elseif'),
        $.parenthesized_expression,
        '{',
        repeat($.stmt),
        '}',
      ),
    else_stmt: $ => seq('else', '{', repeat($.stmt), '}'),
    call_stmt: $ => seq('call', field('ident', $.ident), ';'), // subroutine call expr (e.g. «call strip_query_params;»)
    ident_call_expr: $ =>
      prec('call', seq(field('ident', $.nested_ident), $.func_call_args)), // function call expr (e.g. «if (querystring.get("")) {}»)
    ident_call_stmt: $ => seq($.ident_call_expr, ';'), // function call statement (e.g. «var.global_set("a", "b");» )

    ret_stmt: $ =>
      seq('return', '(', $.varnish_internal_return_methods, ')', ';'),
    new_stmt: $ => seq('new', field('ident', $.ident), '=', $.expr, ';'),
    set_stmt: $ => seq('set', $.nested_ident, '=', $.expr, ';'),
    unset_stmt: $ => seq('unset', $.nested_ident, ';'),
    expr: $ =>
      choice(
        $.literal,
        $.ident_call_expr,
        prec('member', $.nested_ident),
        $.parenthesized_expression,
        $.binary_expression,
        $.neg_expr,
      ),
    parenthesized_expression: $ => seq('(', $.expr, ')'),

    binary_expression: $ =>
      choice(
        prec.left(
          seq(
            field('left', $.expr),
            field('operator', $.comp_op),
            field('right', $.expr),
          ),
        ),
      ),

    neg_expr: $ => prec.left(seq('!', $.expr)),

    bool: () => choice('true', 'false'),
    add: () => choice('+', '-'),
    multiply: () => choice('*', '/'),

    eq: () => '==',
    ne: () => '!=',
    _match: () => '~',
    _nmatch: () => '!~',
    g: () => '>',
    l: () => '<',
    ge: () => '>=',
    le: () => '<=',
    // not: ($) => '!',
    or: () => '||',
    and: () => '&&',

    comp_op: $ =>
      choice(
        $.add,
        $.multiply,
        $.and,
        $.or,
        $.eq,
        $.ne,
        $._match,
        $._nmatch,
        // $.not,
        $.g,
        $.l,
        $.ge,
        $.le,
      ),

    literal: $ => choice($.string, $.number, $.duration, $.bytes),
    string: () =>
      choice(seq('"', /[^"]*/, '"'), seq('{"', /[^"]*"+([^}"][^"]*"+)*/, '}')),
    number: () => /\d+/,
    float: () => /\d+\.\d+/,
    // https://github.com/varnishcache/varnish-cache/blob/a3bc025c2df28e4a76e10c2c41217c9864e9963b/lib/libvcc/vcc_utils.c#L300
    duration: $ =>
      seq(
        choice($.number, $.float),
        choice('ms', 's', 'm', 'h', 'd', 'w', 'y'),
      ),
    bytes: $ =>
      seq(choice($.number, $.float), choice('B', 'KB', 'MB', 'GB', 'TB')),

    ident: () => /[\w\-]+/,
    enum_ident: () => /[A-Z_]+/,
    // optional due to autocomplete
    nested_ident: $ => seq($.ident, repeat(seq('.', optional($.ident)))),
    /*
    partial_nested_ident: ($) =>
      seq($.ident, repeat(seq('.', optional($.ident)))),
      */
    varnish_internal_return_methods: $ =>
      seq(
        choice(
          'hit',
          'pass',
          'pipe',
          'retry',
          'restart',
          'fail',
          'synth',
          'hash',
        ),
        optional(seq('(', optional(commaSep($.expr)), ')')),
      ),
    func_call_args: $ =>
      seq(
        '(',
        optional(
          commaSep(
            choice(
              // named parameter
              seq($.ident, '=', choice($.expr, $.enum_ident)),
              // anonymous parameter
              choice($.expr, $.enum_ident),
            ),
          ),
        ),
        ')',
      ),
  },
});

function commaSep(rule) {
  return seq(rule, repeat(seq(',', rule)));
}

/** globals grammar, $, repeat, choice, seq *$ /

/**
 * Resources:
 * * https://github.com/tree-sitter/tree-sitter-c/blob/master/grammar.js
 * * https://github.com/tree-sitter/tree-sitter-javascript/blob/master/grammar.js
 */

/** @type {import("tree-sitter-cli/dsl").grammar} */
module.exports = grammar({
  name: 'vcl',

  word: $ => $._word,
  extras: $ => [/\s+/, $.COMMENT, $.inline_c],
  inline: $ => [$.expr, $.inline_c],

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
        $.include_declaration_with_semi,
      ),
    acl_declaration: $ =>
      seq(
        'acl',
        field('ident', $.ident),
        field('body', seq('{', repeat($.acl_entry), '}')),
      ),
    sub_declaration: $ =>
      seq(
        'sub',
        field('ident', $.ident),
        field('body', $.block),
      ),
    backend_declaration: $ =>
      seq(
        'backend',
        field('ident', $.ident),
        choice(
          field('body', seq('{', repeat($.backend_property), '}')),
          seq('none', ';'),
        ),
      ),
    probe_declaration: $ =>
      seq(
        'probe',
        field('ident', $.ident),
        field('body', seq('{', repeat($.backend_property), '}')),
      ),
    import_declaration: $ =>
      seq(
        'import',
        field('ident', field('ident', $.ident)),
        optional(seq('from', $.string)),
        ';',
      ),
    vcl_version_declaration: $ => seq('vcl', $.number, ';'),
    include_declaration: $ =>
      seq(
        'include',
        optional('+glob'), // added in 7.0
        $.string,
      ),
    include_declaration_with_semi: $ => seq($.include_declaration, ';'),

    // properties for both backends and probes
    // quirk, probe .request can have a list of strings
    backend_property: $ =>
      seq(
        '.',
        optional(
          seq(
            field('left', $.ident),
            optional(
              seq(
                '=',
                choice(
                  seq(
                    field(
                      'right',
                      optional(
                        choice(
                          $.expr,
                          $.string_list, // strings list is only for probe .request
                        ),
                      ),
                    ),
                    ';',
                  ),
                  field('right', $.inline_probe), // inline probe has cannot have a appending «;»
                ),
              ),
            ),
          ),
        ),
      ),
    acl_entry: $ =>
      seq(
        optional('!'),
        $.string,
        repeat(choice('+log', '+table', '-pedantic')), // 7.0
        optional(seq('/', $.literal)),
        ';',
      ),
    string_list: $ => repeat2($.string),
    inline_probe: $ => seq('{', repeat($.backend_property), '}'),

    stmt: $ => choice($.if_stmt, $._statements_with_semicolon),
    block: $ => seq('{', repeat($.stmt), '}'),

    /*
     * hack to make statements with semicolon (pretty much everything but
     * if statements) require semicolon, but not have the statement itself
     * be an error without semicolon
     */
    _statements_with_semicolon: $ =>
      seq(
        choice(
          $.set_stmt,
          $.new_stmt,
          $.unset_stmt,
          $.call_stmt,
          $.ident_call_stmt,
          $.include_declaration,
          $.ret_stmt,
        ),
        ';',
      ),

    if_stmt: $ =>
      seq(
        'if',
        field('condition', $.parenthesized_expression),
        field('consequence', $.block),
        repeat($.elsif_stmt),
        optional($.else_stmt),
      ),
    elsif_keyword: () => choice(seq('else', 'if'), 'elsif', 'elseif'),
    elsif_stmt: $ =>
      seq(
        field('keyword', $.elsif_keyword),
        field('condition', $.parenthesized_expression),
        field('consequence', $.block),
      ),
    else_stmt: $ => seq('else', $.block),
    call_stmt: $ => seq('call', field('ident', $.ident)), // subroutine call expr (e.g. «call strip_query_params;»)
    ident_call_expr: $ =>
      prec(
        'call',
        seq(
          field('ident', choice($.nested_ident, $.ident)),
          field('args', $.func_call_args),
        ),
      ), // function call expr (e.g. «if (querystring.get("")) {}»)
    ident_call_stmt: $ => $.ident_call_expr, // function call statement (e.g. «var.global_set("a", "b");» )

    ret_stmt: $ =>
      seq('return', optional(seq('(', $.varnish_internal_return_methods, ')'))),
    // new_stmt: $ => seq('new', field('ident', $.ident), '=', $.expr, ';'),
    new_stmt: $ =>
      seq(
        'new',
        optional(
          seq(
            field('ident', $.ident),
            optional(seq('=', optional(field('def_right', $.ident_call_expr)))),
          ),
        ),
      ),
    set_stmt: $ =>
      seq(
        'set',
        optional(
          seq(
            field('left', $.nested_ident),
            optional(
              seq(
                field('operator', choice('=', '+=', '*=', '/=')), // new operators in 7.0
                field('right', choice($.expr, blank())),
              ),
            ),
          ),
        ),
      ),
    unset_stmt: $ => seq('unset', $.nested_ident),
    expr: $ =>
      choice(
        $.literal,
        $.ident_call_expr,
        prec('member', $.nested_ident),
        $.parenthesized_expression,
        $.binary_expression,
        $.neg_expr,
        $.ident,
      ),
    parenthesized_expression: $ => seq('(', $.expr, ')'),

    binary_expression: $ =>
      prec.left(
        seq(
          field('left', $.expr),
          field('operator', $.operator),
          field('right', $.expr),
        ),
      ),

    neg_expr: $ => prec.left(seq('!', $.expr)),

    bool: () => choice('true', 'false'),
    add: () => choice('+', '-'),
    multiply: () => choice('*', '/'),

    eq: () => '==',
    ne: () => '!=',
    rmatch: () => '~', // regex or acl
    nmatch: () => '!~',
    g: () => '>',
    l: () => '<',
    ge: () => '>=',
    le: () => '<=',
    // not: ($) => '!',
    or: () => '||',
    and: () => '&&',

    operator: $ =>
      choice(
        $.add,
        $.multiply,
        $.and,
        $.or,
        $.eq,
        $.ne,
        $.rmatch,
        $.nmatch,
        // $.not,
        $.g,
        $.l,
        $.ge,
        $.le,
      ),

    literal: $ => choice($.string, $.duration, $.bytes, $.number, $.bool),
    string: () =>
      token(
        choice(
          // triple-quoted multiline strings
          seq('"""', /"*(([^"]+?")+?")+?"/),
          // normal strings (not multiline)
          seq('"', /([^\"\u000A\u000D]*(\\"|\\)?)*/, '"'),
          // multiline strings encapsulated with curly braces
          seq('{"', /[^"]*"+([^}"][^"]*"+)*/, '}'),
        ),
      ),
    number: () => /-?\d+(\.\d+)?/,
    // https://github.com/varnishcache/varnish-cache/blob/a3bc025c2df28e4a76e10c2c41217c9864e9963b/lib/libvcc/vcc_utils.c#L300
    duration: $ => seq($.number, choice('ms', 's', 'm', 'h', 'd', 'w', 'y')),
    bytes: $ => seq($.number, choice('B', 'KB', 'MB', 'GB', 'TB')),

    ident: () => /[a-zA-Z][\w-]*/, // ident must start with a letter
    // optional due to autocomplete
    nested_ident: () =>
      token(
        seq(
          /[a-zA-Z][\w-]*/,
          token.immediate(
            repeat(
              seq(token.immediate('.'), optional(token.immediate(/\w[\w-]*/))),
            ),
          ),
        ),
      ),
    varnish_internal_return_methods: $ =>
      seq(
        choice(
          'hit',
          'miss',
          'pass',
          'pipe',
          'retry',
          'restart',
          'fail',
          'synth',
          'hash',
          'deliver',
          'abandon',
          'lookup',
          'error',
          'purge',
          'fetch',
        ),
        optional(seq('(', optional(commaSep($.expr)), ')')),
      ),
    func_call_named_arg: $ =>
      seq(
        field('arg_name', $.ident),
        '=',
        field('arg_value', optional(choice($.expr, $.ident))),
      ),
    func_call_args: $ =>
      seq(
        '(',
        optional(
          commaSep(
            field('arg', choice($.expr, $.ident, $.func_call_named_arg)),
          ),
        ),
        ')',
      ),

    inline_c: _$ =>
      seq('C{', token(repeat1(choice(/[^{}]+/, seq('{', /[^\}]+/, '}')))), '}C'),

    _word: () => /[\w-]+/,
  },
});

function commaSep(rule) {
  return seq(
    rule,
    repeat(seq(',', rule)),
    // optional trailing comma
    optional(','),
  );
}

function repeat2(rule) {
  return seq(rule, repeat1(rule));
}

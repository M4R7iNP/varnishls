const vclGrammar = require('../tree-sitter-vcl/grammar.js');

module.exports = grammar(vclGrammar, {
  name: 'vtc',

  extras: $ => [/\s/, $.COMMENT, $.inline_c, /\\\r?\n/],

  rules: {
    COMMENT: () => token(seq('#', /.*/)),

    source_file: $ => repeat($.vtc_statement),
    source_file_vcl: $ => repeat1($.toplev_declaration),
    vtc_statement: $ =>
      choice(
        $.varnishtest,
        $.server,
        $.client,
        $.varnish,
        $.barrier,
        $.haproxy,
        $.syslog,
        $.delay,
        $.shell,
        $.setenv,
      ),
    vtc_block_statement: $ => choice($.txrx_statement, $.expect),
    varnishtest: $ => seq(choice('varnishtest', 'vtest'), $.string),
    server: $ =>
      seq(
        'server',
        $.ident,
        field('block', seq('{', repeat($.vtc_block_statement), '}')),
        repeat($.argument),
      ),
    client: $ =>
      seq(
        'client',
        $.ident,
        optional(field('block', seq('{', repeat($.vtc_block_statement), '}'))),
        repeat($.argument),
      ),
    varnish: $ =>
      seq(
        'varnish',
        $.ident,
        repeat(choice($.vcl_argument, $.expect_argument, $.argument)),
      ),
    barrier: $ => seq('barrier', $.ident, repeat($.argument)),
    haproxy: $ => seq('haproxy', $.ident, repeat($.argument)),
    syslog: $ => seq('syslog', $.ident, repeat($.argument)),
    delay: $ => seq('delay', $.number),
    setenv: $ => seq('setenv', optional('-ifunset'), $.ident, $.string),

    // TODO:
    shell: $ =>
      seq(
        choice('shell', 'process'),
        repeat1(choice(prec(1, $.block_argument_value), $.argument)),
      ),
    // shell_block: () => seq('{', repeat(/[^\{\}]*(\{[^\}]*\})?/), '}'),
    // shell_block: () =>
    // seq('{', repeat1(choice(/[^${}]+/, seq('${', /[^\}]+/, '}'))), '}'),

    vcl_argument: $ =>
      seq(
        '-vcl',
        optional(token.immediate('+backend')),
        '{',
        $.source_file_vcl,
        '}',
      ),

    expect_argument: $ => seq('-expect', $.binary_expression),
    // TODO: try to generalize -arg {str}
    // body_argument: () => seq('-body', '{', /[^\}]+/, '}'),
    req_argument: $ => seq('-req', choice(/[A-Z]+/, $.string)), // e.g. -req POST
    block_argument_value: () =>
      seq('{', repeat1(choice(/[^${}]+/, seq('${', /[^\}]+/, '}'))), '}'),

    // generic argument
    argument_ident: () => /-\w+/,
    argument: $ =>
      prec.right(
        2,
        seq(
          $.argument_ident,
          optional(choice($.string, $.number, $.block_argument_value)),
        ),
      ),

    txrx_statement: $ =>
      seq(
        choice('rxreq', 'txresp', 'txreq', 'rxresp'),
        repeat(choice($.req_argument, $.argument)),
      ),
    expect: $ => seq('expect', $.binary_expression),

    // <undef> is valid in «expect req.http.null-header == <undef>»
    literal: (_$, prev) => choice(prev, '<undef>'),

    // _terminator: () => repeat1('\n'),
  },
});

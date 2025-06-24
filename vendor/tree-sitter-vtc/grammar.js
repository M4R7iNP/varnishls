const vclGrammar = require('../tree-sitter-vcl/grammar.js');

/**
 * SEE: https://varnish-cache.org/docs/trunk/reference/vtc.html
 */

const VTC_BLOCK_STATEMENTS = [
  'rxreq',
  'txresp',
  'txreq',
  'rxresp',
  'txping',
  'rxping',
  'txprio',
  'rxprio',
  'txrst',
  'rxrst',
  'deplay',
  'rxsettings',
  'txsettings',
  'rxwinup',
  'txwinup',
  'accept',
  'txgoaway',
  'rxgoaway',
  'tls_handshake',
  'expect_close',
  'gunzip',

  // tunnel statements
  'pause',
  'delay',
  'resume',
];

module.exports = grammar(vclGrammar, {
  name: 'vtc',

  extras: $ => [/\s/, $.COMMENT, $.inline_c, /\\\r?\n/],
  conflicts: $ => [
    [$.tls_config_statement]
  ],

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
        $.logexpect,
        $.feature_statement,
        $.tunnel_block,
      ),
    feature_statement: $ => seq('feature', repeat1($.ident)), 
    loop_statement: $ => seq(
      'loop',
      $.number,
      field('block', seq('{', repeat($.vtc_block_statement), '}'))
    ),
    stream_block: $ => seq('stream', $.number, field('block', seq('{', repeat($.vtc_block_statement), '}')), repeat($.argument)),
    vtc_block_statement: $ => choice($.vtc_block_statement_with_arguments, $.expect, $.tls_config, $.send, $.recv, $.barrier, $.loop_statement, $.delay, $.stream_block),
    varnishtest: $ => seq(choice('varnishtest', 'vtest'), $.string),
    server: $ =>
      seq(
        'server',
        $.ident,
        repeat($.server_string_arguments),
        optional(field('block', seq('{', repeat($.vtc_block_statement), '}'))), // Made block optional
        repeat($.argument),
      ),
    client: $ =>
      seq(
        'client',
        $.ident,
        repeat($.client_string_arguments),
        optional(field('block', seq('{', repeat($.vtc_block_statement), '}'))),
        repeat(choice($.argument)),
      ),
    tls_config: $ =>
      seq(
        'tls_config',
        optional(field('block', seq('{', repeat($.tls_config_statement), '}'))),
        repeat($.argument),
      ),
    tunnel_block: $ =>
      seq(
        'tunnel',
        field('ident', $.ident),
        optional(field('block', seq('{', repeat($.vtc_block_statement), '}'))),
        repeat($.argument),
      ),
    varnish: $ =>
      seq(
        'varnish',
        $.ident,
        repeat(choice($.vcl_argument, $.expect_argument, $.argument)),
      ),
    barrier: $ => seq('barrier', $.ident, choice('cond', 'sock', 'sync'), optional($.number), optional('-cyclic')),
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
    expect_statement: $ =>
      seq(
        'expect',
        optional('*'),
        $.number,
        $.ident,
        $.string
      ),
    expect_argument: $ => seq('-expect', $.binary_expression),
    // TODO: try to generalize -arg {str}
    // body_argument: () => seq('-body', '{', /[^\}]+/, '}'),
    req_argument: $ => seq('-req', choice(/[A-Z]+/, $.string)), // e.g. -req POST
    number_arguments: $ => seq('-repeat', $.number),
    client_string_arguments: $ => seq(choice('-connect', '-proxy1', '-proxy2'), choice($.string, $.variable, $.number)),
    server_string_arguments: $ => seq(choice('-listen', '-dispatch', '-repeat'), choice($.string, $.variable, $.number)),

    block_argument_value: () =>
      seq('{', repeat1(choice(/[^${}]+/, seq('${', /[^\}]+/, '}'))), '}'),

    // generic argument
    argument_ident: () => /-\w+/,
    argument: $ =>
      prec.right(
        2,
        seq(
          $.argument_ident,
          optional(choice($.string, $.number, $.block_argument_value, $.ident)), // Added $.ident here
        ),
      ),

    // general block statement with arguments
    vtc_block_statement_with_arguments: $ =>
      seq(
        choice(...VTC_BLOCK_STATEMENTS),
        repeat(choice($.req_argument, $.argument)),
      ),
    expect: $ => seq('expect', $.binary_expression),

    // <undef> is valid in «expect req.http.null-header == <undef>»
    literal: (_$, prev) => choice(prev, '<undef>'),

    // _terminator: () => repeat1(\'\\\\n\'),

    // Specific for values like "http/1.1", "example.com" that contain '.' or '/'
    // and are not simple idents.
    complex_config_value: () => token(prec(1, /[a-zA-Z0-9_.-]*[./][a-zA-Z0-9_./-]+/)),

    config_value_item: $ => choice(
      $.string,
      $.variable,
      $.tls_version,
      $.complex_config_value, // For things like http/1.1
      $.ident                 // For simple idents like h2, true, etc. (from VCL grammar)
    ),

    tls_config_statement: $ =>
      seq(
        $.ident,
        '=',
        repeat1($.config_value_item)
      ),

    variable: $ => seq('${', $.ident, '}'),
    send: $ => seq('send', choice($.string, $.number)),
    recv: $ => seq('recv', choice($.string, $.number)),
    tls_version: $ => choice('TLSv1.0', 'TLSv1.1', 'TLSv1.2', 'TLSv1.3'),
    logexpect_expect: $ => seq('expect', /.*/),
    logexpect: $ =>
      seq(
        'logexpect',
        $.ident,
        (optional(seq('-v', $.ident))),
        (optional(seq('-g', $.ident))),
        optional(field('block', seq('{', repeat($.logexpect_expect), '}'))),
        repeat(choice($.argument)),
      ),
  },
});

[
    "{"
    "C{"
] @append_hardline @append_indent_start @prepend_space

[
    "}"
    "}C"
] @prepend_spaced_softline @prepend_indent_end

[
    "="
    "+="
    "*="
    "/="
] @prepend_space @append_space

[
    "vcl"
    "if"
    "sub"
    "set"
    "unset"
    "new"
    "import"
    "call"
    "include"
    "backend"
    "acl"
    ","
] @append_space

[
    "else"
    "elsif"
    "elseif"
] @append_space @prepend_space

(
    [
        (toplev_declaration)
        (stmt)
    ] @append_hardline
    .
    (COMMENT)* @do_nothing
)

[
    (toplev_declaration)
    (stmt)
    (COMMENT)
] @allow_blank_line_before

(binary_expression (operator [(or) (and)]) @append_empty_softline) (#scope_id! "tuple")

(func_call_args
  "(" @append_begin_scope @append_empty_softline @append_indent_start
  ")" @prepend_end_scope @prepend_empty_softline @prepend_indent_end
  (#scope_id! "tuple")
)

(func_call_args "," @append_empty_softline) (#scope_id! "tuple")

(binary_expression
    operator: (operator [(add) (multiply)]) @append_indent_start
    right: (_) @append_indent_end
    (#scope_id! "big_maths")
)
(binary_expression operator: (operator [(add) (multiply)]) @append_empty_softline) (#scope_id! "big_maths")

(operator) @prepend_space @append_space

(COMMENT) @append_hardline @prepend_input_softline @multi_line_indent_all
(inline_c) @append_hardline @multi_line_indent_all @allow_blank_line_before
";" @append_space

(backend_property) @append_hardline
; FIXME:
((backend_property ";"? @do_nothing) @append_delimiter (#delimiter! ";"))

[
    (string)
    (ident)
    (nested_ident)
    (inline_c)
] @leaf

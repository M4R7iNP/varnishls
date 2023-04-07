[
  "acl"
  "sub"
  "backend"
  "probe"
  "vcl"
  "else"
  "elsif"
  "elseif"
  "if"
  "return"
  "import"
  "include"
  "set"
  "unset"
  "new"
  "call"
] @keyword

[
  "hit"
  "miss"
  "pass"
  "pipe"
  "retry"
  "restart"
  "fail"
  "synth"
  "hash"
  "deliver"
  "abandon"
  "lookup"
] @keyword

(operator) @operator
"=" @operator

[
  "."
  ","
  ";"
] @punctuation.delimiter

[
  "("
  ")"
  "{"
  "}"
]  @punctuation.bracket


(string) @string
(number) @number
(float) @number
(duration) @number
(bytes) @number
(bool) @constant.builtin

(ident_call_expr
  ident: (nested_ident) @name) @function.call
(ident_call_expr
  ident: (nested_ident) @name (#eq? @name "regsub")) @function.builtin
(ident_call_expr
  ident: (nested_ident) @name (#eq? @name "regsuball")) @function.builtin
(ident_call_expr
  ident: (nested_ident) @name (#eq? @name "hash_data")) @function.builtin
(ident_call_expr
  ident: (nested_ident) @name (#eq? @name "synthetic")) @function.builtin
(ident_call_expr
  ident: (nested_ident) @name (#eq? @name "ban")) @function.builtin

(ident) @variable
(nested_ident) @variable
(enum_ident) @constant

(COMMENT) @comment

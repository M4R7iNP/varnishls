; LSP semantic token queries

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
  "ok"
] @keyword

(operator) @operator
"=" @operator
"!" @operator
; [ "(" ")" ";"] @delimiter


(string) @string
(number) @number
; (float) @number
(duration) @number
(bytes) @number
(bool) @number

(ident) @variable
(nested_ident) @property

(binary_expression
  operator: (operator (rmatch))
  right: (literal (string) @regexp (#offset! @regexp 0 1 0 -1)))



(ident_call_expr
  ident: (nested_ident) @function)

(ident_call_expr
  ident: (ident) @function.defaultLibrary (#match? @function.defaultLibrary "^regsub|regsuball|hash_data|synthetic|ban$"))

(func_call_named_arg
   arg_name: (ident) @parameter)

(sub_declaration ident: (ident) @function.declaration)
(sub_declaration ident: (ident) @function.defaultLibrary (#match? @function.defaultLibrary "^vcl_.*"))
(COMMENT) @comment

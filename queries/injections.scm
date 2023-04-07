(COMMENT) @comment
(binary_expression
  operator: (operator (rmatch))
  right: (literal (string) @regex (#offset! @regex 0 1 0 -1)))
(ident_call_expr
  ident: (nested_ident (ident) @_name (#eq? @_name "regsub"))
  (func_call_args . (_) (literal (string) @regex (#offset! @regex 0 1 0 -1))))
(ident_call_expr
  ident: (nested_ident (ident) @_name (#eq? @_name "regsuball"))
  (func_call_args . (_) (literal (string) @regex (#offset! @regex 0 1 0 -1))))
(ident_call_expr
  ident: (nested_ident (ident) @_name (#eq? @_name "synthetic"))
  (func_call_args (literal (string) @content (#match? @content "<html>")) @html (#offset! @html 0 1 0 -1)))

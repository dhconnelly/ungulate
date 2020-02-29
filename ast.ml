type expr =
  Call of call_expr |
  If of if_expr |
  Lambda of lambda_expr |
  Seq of seq_expr |
  Def of def_expr |
  Ident of string |
  Num of int
and call_expr = {fn: expr; args: expr list}
and if_expr = {cond: expr; cons: expr; alt: expr}
and lambda_expr = {params: string list; body: expr}
and def_expr = {name: string; value: expr}
and seq_expr = expr list

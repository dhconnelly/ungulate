open Ast
open Printf
open String

let parse toks = Ok []

let rec print_all exprs = List.map print exprs
and print = function
  | Call e -> sprintf "(%s %s)" (print e.fn) (print_all e.args |> concat " ")
  | If e -> sprintf "(if %s %s %s)" (print e.cond) (print e.cons) (print e.alt)
  | Lambda e -> sprintf "(lambda (%s) %s)" (concat " " e.params) (print e.body)
  | Seq es -> sprintf "(seq %s)" (print_all es |> concat "\n")
  | Def e -> sprintf "(define %s %s)" e.name (print e.value)
  | Ident s -> s
  | Num x -> string_of_int x

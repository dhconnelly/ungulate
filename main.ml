open Printf
module StringMap = Map.Make(String)

type typ = Lparen | Rparen | Ident | Int | Eof
type token = { typ: typ; text: string }

let is_alpha = function
  | 'a'..'z' | 'A'..'Z' -> true
  | _ -> false
let is_num = function
  | '0'..'9' -> true
  | _ -> false
let is_alphanum ch = (is_alpha ch) || (is_num ch)

let rec scan_while (text: string) pos f =
  if pos >= (String.length text) then pos
  else match String.get text pos with
  | ch when f ch -> scan_while text (pos+1) f
  | ')' | ' ' | '\n' | '\t' -> pos
  | ch -> failwith (sprintf "bad char in token: %c" ch)

let rec scan_tok text pos =
  if pos >= String.length text then Eof, pos, 0
  else match String.get text pos with
  | '(' -> Lparen, pos, 1
  | ')' -> Rparen, pos, 1
  | '+' | '*' | '-' | '<' -> Ident, pos, 1
  | 'a'..'z' | 'A'..'Z' -> Ident, pos, (scan_while text pos is_alphanum) - pos
  | '0'..'9' -> Int, pos, (scan_while text pos is_num) - pos
  | ' ' | '\n' | '\t' -> scan_tok text (pos+1)
  | ch -> failwith (sprintf "unknown token: %c" ch)

let scan (text: string) =
  let rec scan_rec acc pos =
    let (typ, pos, len) as tok = scan_tok text pos in
    let text = String.sub text pos len in
    match typ with
    | Eof -> acc
    | typ -> scan_rec ({typ; text}::acc) (pos+len)
  in scan_rec [] 0 |> List.rev

type expr =
  Call of call_expr |
  If of if_expr |
  Lambda of lambda_expr |
  Def of def_expr |
  Seq of expr list |
  Lit of string |
  Int of int
and call_expr = {fn: expr; args: expr list}
and if_expr = {cond: expr; cons: expr; alt: expr}
and lambda_expr = {params: string list; body: expr; name: string}
and def_expr = {key: string; value: expr}

let eat typ = function
  | [] -> failwith "unexpected eof while parsing"
  | {typ} as tok::toks when typ=typ -> tok, toks
  | {text}::_ -> failwith (sprintf "unexpected token while parsing: %s" text)

let until delim f toks =
  let rec until_rec acc = function
    | [] -> acc, []
    | {typ}::toks when typ=delim -> List.rev acc, toks
    | toks -> let (e, toks) = f toks in until_rec (e::acc) toks
  in until_rec [] toks

let rec parse_one = function
  | {typ=Lparen}::{text="if"}::toks -> parse_if toks
  | {typ=Lparen}::{text="lambda"}::toks -> parse_lambda toks
  | {typ=Lparen}::{text="define"}::toks -> parse_define toks
  | {typ=Lparen}::{text="seq"}::toks -> parse_seq toks
  | {typ=Lparen}::toks -> parse_call toks
  | {typ=Int; text}::toks -> Int (int_of_string text), toks
  | {typ=Ident; text}::toks -> Lit text, toks
  | {text}::_ -> failwith (sprintf "bad expr: %s" text)
  | [] -> failwith "unexpected eof"

and parse_if toks =
  let cond, toks = parse_one toks in
  let cons, toks = parse_one toks in
  let alt, toks = parse_one toks in
  let _, toks = eat Rparen toks in
  If {cond; cons; alt}, toks

and parse_name toks =
  let (tok, toks) = eat Ident toks in tok.text, toks

and parse_lambda_def name toks =
  let (_, toks) = eat Lparen toks in
  let (params, toks) = until Rparen parse_name toks in
  let (body, toks) = parse_one toks in
  let (_, toks) = eat Rparen toks in
  Lambda {params; body; name}, toks

and parse_lambda = function
  | {typ=Ident; text}::toks -> parse_lambda_def text toks
  | ({typ=Lparen}::_) as toks -> parse_lambda_def "" toks
  | [] -> failwith "unexpected eof"
  | {text}::_ -> failwith (sprintf "invalid token in lambda: %s" text)

and parse_define toks =
  let (key, toks) = parse_name toks in
  let (value, toks) = parse_one toks in
  let (_, toks) = eat Rparen toks in
  Def {key; value}, toks

and parse_seq toks =
  let (exprs, toks) = until Rparen parse_one toks in
  Seq exprs, toks

and parse_call toks =
  let (fn, toks) = parse_one toks in
  let (exprs, toks) = until Rparen parse_one toks in
  Call {fn; args=exprs}, toks

let rec parse = function
  | [] -> []
  | toks -> let (expr, toks) = parse_one toks in expr::(parse toks)

type value = Nil | BoolV of bool | IntV of int | FnV of fn_val
and env = value StringMap.t
and fn = env -> value list -> value
and fn_val = {name: string; arity: int; env: env; apply: fn}

let print_val = function
  | Nil -> "nil"
  | BoolV x -> sprintf "%b" x
  | IntV x -> string_of_int x
  | FnV {name} -> name

let print_all_vals vals = List.map print_val vals |> String.concat "\n"

let unwrap_bool = function
  | BoolV x -> x
  | v -> failwith (sprintf "type: expected bool, got %s" (print_val v))
let unwrap_int = function 
  | IntV x -> x
  | v -> failwith (sprintf "type: expected int, got %s" (print_val v))
let unwrap_fn = function
  | FnV f -> f
  | v -> failwith (sprintf "type: expected fn, got %s" (print_val v))

let rec eval_in env = function
  | Call e -> eval_call env e
  | If e -> eval_if env e
  | Lambda e -> eval_lambda env e
  | Def e -> eval_def env e
  | Seq es -> eval_seq env es
  | Lit s -> lookup env s, env
  | Int x -> IntV x, env

and lookup env key = match StringMap.find_opt key env with
  | None -> failwith (sprintf "unbound ident: %s" key)
  | Some v -> v

and eval_lambda_call env params args body =
  let bindings = List.combine params args in
  let local_env = List.fold_left (fun env (k,v) -> StringMap.add k v env) env bindings in
  let (value, _) = eval_in local_env body in
  value

and eval_lambda env {params; body; name} =
  FnV {
    name=if String.length name > 0 then name else "<anonymous_fn>";
    arity=(List.length params);
    env;
    apply=fun env args -> eval_lambda_call env params args body
  }, env

and eval_seq env es = 
  let (vals, _) = eval_all_in env es in match List.rev vals with
  | [] -> Nil, env
  | last::_ -> last, env

and eval_def env {key; value} =
  let (value, env) = eval_in env value in Nil, StringMap.add key value env

and eval_if env {cond; cons; alt} =
  let prev_env = env in
  let (cond, env) = eval_in env cond in
  let (value, env) = eval_in env (if unwrap_bool cond then cons else alt) in
  value, prev_env

and eval_all_in env = function
  | [] -> [], env
  | x::xs ->
    let (v, env) = eval_in env x in
    let (vs, env) = eval_all_in env xs in
    v::vs, env

and eval_call env {fn; args} =
  let (fn, env) = eval_in env fn in
  let (args, env) = eval_all_in env args in
  let {name; arity; env=local_env; apply} = unwrap_fn fn in
  if arity <> (List.length args) then
    failwith (sprintf "%s expects %d args" name arity)
  else (apply local_env args), env

let default_env = StringMap.(empty |>
  add "display" (FnV {
    name="display";
    arity=1;
    env=empty;
    apply=fun _ args -> List.nth args 0 |> print_val |> print_endline; Nil
  }) |>

  add "-" (FnV {
    name="-";
    arity=2;
    env=empty;
    apply=fun _ args -> (
      let (l, r) = (List.nth args 0), (List.nth args 1) in
      IntV (unwrap_int l - unwrap_int r))
  }) |>

  add "*" (FnV {
    name="*";
    arity=2;
    env=empty;
    apply=fun _ args -> (
      let (l, r) = (List.nth args 0), (List.nth args 1) in
      IntV (unwrap_int l * unwrap_int r))
  }) |>

  add "+" (FnV {
    name="+";
    arity=2;
    env=empty;
    apply=fun _ args -> (
      let (l, r) = (List.nth args 0), (List.nth args 1) in
      IntV (unwrap_int l + unwrap_int r))
  }) |>

  add "<" (FnV {
    name="<";
    arity=2;
    env=empty;
    apply=fun _ args -> (
      let (l, r) = (List.nth args 0), (List.nth args 1) in
      BoolV (unwrap_int l < unwrap_int r))
  }))

let eval expr = eval_in default_env expr
let eval_all exprs = eval_all_in default_env exprs

let concat = String.concat
let rec print_all exprs = List.map print exprs |> concat " "
and print = function
  | Call e -> sprintf "(%s %s)" (print e.fn) (print_all e.args)
  | If e -> sprintf "(if %s %s %s)" (print e.cond) (print e.cons) (print e.alt)
  | Lambda e -> sprintf "(lambda (%s) %s)" (concat " " e.params) (print e.body)
  | Def e -> sprintf "(define %s %s)" e.key (print e.value)
  | Seq es -> sprintf "(seq %s)" (print_all es)
  | Lit s -> s
  | Int x -> string_of_int x

let read_all ic =
  let rec read_rec acc =
    try read_rec ((input_char ic)::acc) with End_of_file -> acc in
  read_rec [] |> List.rev |> List.to_seq |> String.of_seq

let run path =
  open_in path |> read_all |> scan |> parse |> eval_all |> ignore

let () =
  let usage = "Usage: ung <file>" in
  let path = try Sys.argv.(1) with Invalid_argument _ -> failwith usage in
  run path

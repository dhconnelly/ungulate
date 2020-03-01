open Printf

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
  Lit of string |
  Int of int
and call_expr = {fn: expr; args: expr list}
and if_expr = {cond: expr; cons: expr; alt: expr}
and lambda_expr = {params: string list; body: expr}
and def_expr = {name: string; value: expr}

let eat typ = function
  | [] -> failwith "unexpected eof while parsing"
  | {typ} as tok::toks when typ=typ -> tok, toks
  | {text}::_ -> failwith (sprintf "unexpected token while parsing: %s" text)

let until delim f toks =
  let rec until_rec acc = function
    | [] -> acc, []
    | {typ}::toks when typ=delim -> acc, toks
    | toks -> let (e, toks) = f toks in until_rec (e::acc) toks
  in until_rec [] toks

let rec parse_one = function
  | {typ=Lparen}::{text="if"}::toks -> parse_if toks
  | {typ=Lparen}::{text="lambda"}::toks -> parse_lambda toks
  | {typ=Lparen}::{text="define"}::toks -> parse_define toks
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
and parse_lambda toks =
  let (_, toks) = eat Lparen toks in
  let (params, toks) = until Rparen parse_name toks in
  let (body, toks) = parse_one toks in
  let (_, toks) = eat Rparen toks in
  Lambda {params; body}, toks
and parse_define toks =
  let (name, toks) = parse_name toks in
  let (value, toks) = parse_one toks in
  let (_, toks) = eat Rparen toks in
  Def {name; value}, toks
and parse_call toks =
  let (fn, toks) = parse_one toks in
  let (exprs, toks) = until Rparen parse_one toks in
  Call {fn; args=exprs}, toks

let rec parse = function
  | [] -> []
  | toks -> let (expr, toks) = parse_one toks in expr::(parse toks)

let concat = String.concat
let rec print_all exprs = List.map print exprs |> concat " "
and print = function
  | Call e -> sprintf "(%s %s)" (print e.fn) (print_all e.args)
  | If e -> sprintf "(if %s %s %s)" (print e.cond) (print e.cons) (print e.alt)
  | Lambda e -> sprintf "(lambda (%s) %s)" (concat " " e.params) (print e.body)
  | Def e -> sprintf "(define %s %s)" e.name (print e.value)
  | Lit s -> s
  | Int x -> string_of_int x

let read_all ic =
  let rec read_rec acc =
    try read_rec ((input_char ic)::acc) with End_of_file -> acc in
  read_rec [] |> List.rev |> List.to_seq |> String.of_seq

let run path =
  open_in path |> read_all |> scan |> parse |> print_all |> print_endline

let () =
  let usage = "Usage: ung <file>" in
  let path = try Sys.argv.(1) with Invalid_argument _ -> failwith usage in
  run path

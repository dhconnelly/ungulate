type toktyp =
  Lparen | Rparen | Ident | Int | Plus | Minus | Lt | Eof

type token = {
  typ: toktyp;
  cargo: string;
  line: int;
  col: int;
}

type scanner
val scan : in_channel -> scanner
val peek : scanner -> (token, string) result
val next : scanner -> (token, string) result

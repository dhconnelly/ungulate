type toktyp =
  Lparen | Rparen | Ident | Int | Plus | Minus | Lt | Eof

type token = {
  typ: toktyp;
  cargo: string;
  line: int;
  col: int;
}

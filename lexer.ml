type toktyp =
  Lparen | Rparen | Ident | Int | Plus | Minus | Lt | Eof

type token = {
  typ: toktyp;
  cargo: string;
  line: int;
  col: int;
}

type scanner = unit
let scan ic = ()
let peek scanner = Ok {typ=Eof; cargo=""; line=1; col=1}
let next scanner = Error "undefined"

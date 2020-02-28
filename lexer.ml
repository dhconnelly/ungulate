open Token

type scanner = unit
let scan ic = ()
let peek scanner = Ok {typ=Eof; cargo=""; line=1; col=1}
let next scanner = Error "undefined"

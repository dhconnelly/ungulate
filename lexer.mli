type scanner
val scan : in_channel -> scanner
val peek : scanner -> (Token.token, string) result
val next : scanner -> (Token.token, string) result

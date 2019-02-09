{ 
  type token  = Int of int | Abs | Plus | Minus |Mult | Div | Rem | Exp | Lparen | Rparen | True | False | Not | And | Or | Eq | Gt | Lt | Ge | Le | If | Then | Else | Iden | Def | Deli
}

let whitespace = [' ' '\t']+
let digit = ['0'-'9']
let digits = digit+
let integer =  ('-'|'+')?(0|['1'-'9']digits)
let identifier = ['a'-'z']['a'-'z''A'-'Z''0'-'9']*

rule read = parse
| whitespace { token lexbuf }
| integer as n {Int (int_ofstring)}
| "abs"           {Abs}
|  '+'               {Plus}
|  '-'               {Minus}
|  '*'               {Mult}
|  "div"               {Div}
|  "mod"               {Rem}
|  '^'               {Exp}
|  '('               {Lparen}
|  ')'               {Rparen}
|  'T'               {True}
|  'F'               {False}
|  "not"               {Not}
|  "\/\\"               {And}
|  "\\\/"               {Or}
|  '='               {Eq}
|  '>'               {Gt}
|  '<'               {Lt}
|  ">="               {Ge}
|  "<="               {Le}
|  "if"               {If}
|  "then"               {Then}
|  "else"               {Else}
|  identifier               {Iden}
|  "def"               {Def}
|  ';'               {Deli}
| _                      {read lexbuf}


{
  let lexme s = read (Lexing.from_string s)
}

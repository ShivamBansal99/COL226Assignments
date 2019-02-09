{ 
  type token  = Int of int | Abs | Plus | Minus |Mult | Div | Rem | Exp | Lparen | Rparen | True | False | Not | And | Or | Eq | Gt | Lt | Ge | Le | If | Then | Else | Iden | Def | Deli
}

let whitespace = [' ' '\t']+
let digit = ['0'-'9']
let digits = digit*
let integer =  ('-'|'+')?('0'|['1'-'9']digits)
let identifier = ['a'-'z']['a'-'z''A'-'Z''0'-'9']*

rule read = parse
| whitespace { read lexbuf }
| integer as n {(Int (int_of_string(n)))::(read lexbuf)}
| "abs"           {Abs::(read lexbuf)}
|  '+'               {Plus::(read lexbuf)}
|  '-'               {Minus::(read lexbuf)}
|  '*'               {Mult::(read lexbuf)}
|  "div"               {Div::(read lexbuf)}
|  "mod"               {Rem::(read lexbuf)}
|  '^'               {Exp::(read lexbuf)}
|  '('               {Lparen::(read lexbuf)}
|  ')'               {Rparen::(read lexbuf)}
|  'T'               {True::(read lexbuf)}
|  'F'               {False::(read lexbuf)}
|  "not"               {Not::(read lexbuf)}
|  "/\\"               {And::(read lexbuf)}
|  "\\/"               {Or::(read lexbuf)}
|  '='               {Eq::(read lexbuf)}
|  '>'               {Gt::(read lexbuf)}
|  '<'               {Lt::(read lexbuf)}
|  ">="               {Ge::(read lexbuf)}
|  "<="               {Le::(read lexbuf)}
|  "if"               {If::(read lexbuf)}
|  "then"               {Then::(read lexbuf)}
|  "else"               {Else::(read lexbuf)}
|  "def"               {Def::(read lexbuf)}
|  identifier               {Iden::(read lexbuf)}
|  ';'               {[Deli]}
| _                      {failwith "nothing"}


{
  let scanner s = read (Lexing.from_string s)
}

{ 
  type token =
   INT of int          (* integer constant, positive or negative w/o leading zeros *)
|  TRUE                (* boolean constant "T" *)
|  FALSE               (* boolean constant "F" *)
|  ABS                 (* unary operator, "abs" *)
|  PLUS                (* arithmetic plus, "+" *)
|  MINUS               (* arithmetic minus, "-" *)
|  MUL                 (* arithmetic multiply, "*" *)
|  DIV                 (* integer div, "div" *)
|  MOD                 (* remainder, "mod" *)
|  EXP                 (* exponentiation, "^" *)
|  LP                  (* left paren, "(" *)
|  RP                  (* right paren, ")" *)
|  NOT                 (* boolean NOT, "not" *)
|  AND                 (* boolean AND, "/\ " *)
|  OR                  (* boolean OR, "\/" *)
|  EQ                  (* equal to, "=" *)
|  GTA                 (* greater than, ">" *)
|  LTA                 (* less than, "<" *)
|  GEQ                 (* greater than/equal to, ">=" *)
|  LEQ                 (* less than/equal to, "<=" *)
|  IF                  (* keyword "if" *)
|  THEN                (* keyword "then" *)
|  ELSE                (* keyword "else" *)
|  ID of string        (* variable identifier, alphanumeric string with first char lowercase *)
|  DEF                 (* definition construct, "def" *)
|  DELIMITER;;         (* delimiter, ";" *)
}

let whitespace = [' ' '\t']+  (*whitespaces*)
let digit = ['0'-'9']   (*digit*)
let digits = digit* (*digits*)
let integer =  ('-'|'+')?('0'|['1'-'9']digits) (*integers*)
let identifier = ['a'-'z']['a'-'z''A'-'Z''0'-'9']*  (*identifiers*)

rule read = parse
| whitespace { read lexbuf }
| integer as n {(INT (int_of_string(remplus(n))))::(read lexbuf)}
| "abs"           {ABS::(read lexbuf)}
|  '+'               {PLUS::(read lexbuf)}
|  '-'               {MINUS::(read lexbuf)}
|  '*'               {MULT::(read lexbuf)}
|  "div "               {DIV::(read lexbuf)}
|  "mod "               {MOD::(read lexbuf)}
|  '^'               {EXP::(read lexbuf)}
|  '('               {LP::(read lexbuf)}
|  ')'               {RP::(read lexbuf)}
|  'T'               {TRUE::(read lexbuf)}
|  'F'               {FALSE::(read lexbuf)}
|  "not"               {NOT::(read lexbuf)}
|  "/\\"               {AND::(read lexbuf)}
|  "\\/"               {OR::(read lexbuf)}
|  '='               {EQ::(read lexbuf)}
|  '>'               {GTA::(read lexbuf)}
|  '<'               {LTA::(read lexbuf)}
|  ">="               {GEQ::(read lexbuf)}
|  "<="               {LEQ::(read lexbuf)}
|  "if "               {IF::(read lexbuf)}
|  "then "               {THEN::(read lexbuf)}
|  "else "               {ELSE::(read lexbuf)}
|  "def "               {DEF::(read lexbuf)}
|  identifier as a          {(ID (a)) ::(read lexbuf)}
|  ';'                 {[DELIMITER]::(read lexbuf)}
| eof                 {[]}
|  _                   {failwith "Illegal character"}

{
let rec remplus k = match k with
| "+" ^ s -> remplus s
| s -> s
;;
  let scanner s = read (Lexing.from_string s)
}



(*

COUNTER EXAMPLES

>>scanner "ifthenelse"
[ID(ifthenelse)]
>>scanner "if then else"
[IF;THEN;ELSE]
>>scanner "defmod"
[ID(defmod)]
>>scanner "def mod"
[DEF;REM]
>>scanner "5+4"
[INT(5);INT(4)]
>>scanner "5 + 4"
[INT(5);PLUS;INT(4)]
*)

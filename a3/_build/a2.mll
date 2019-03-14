{
open A3
}

let whitespace = [' ' '\t']+  (*whitespaces*)
let digit = ['0'-'9']   (*digit*)
let digits = digit* (*digits*)
let integer =  ('0'|['1'-'9']digits) (*integers*)
let identifier = ['A'-'Z']['a'-'z''A'-'Z''0'-'9''_''\'']*  (*identifiers*)



rule read = parse
| whitespace { read lexbuf }
| integer as n {(INT (int_of_string(n))) }
| "abs"           {ABS }
| '~'				{TILDA}
|  '+'               {PLUS }
|  '-'               {MINUS }
|  '*'               {TIMES }
|  "div"               {DIV }
|  "mod"               {REM }
|  '('               {LP }
|  ')'               {RP }
|  'T'               {BOOL(true) }
|  'F'               {BOOL(false) }
|  "not"               {NOT }
|  "/\\"               {CONJ }
|  "\\/"               {DISJ }
|  '='               {EQ }
|  '>'               {GT }
|  '<'               {LT }
| "proj"			{PROJ}
|  "if"               {IF }
|  "then"               {THEN }
|  "else"               {ELSE }
|  "fi"               {FI }
|  "def "               {DEF }
|  identifier as a          {(ID (a))  }
| ','					{COMMA}
|  ';'                 {DELIMITER }
| eof                 {EOF}
|  _                   {failwith "Illegal character"}





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

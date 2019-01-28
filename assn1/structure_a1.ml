type  exptree =  N of int 
  |  Plus of exptree *  exptree 
  | Minus of exptree *  exptree 
  |  Mult of exptree *  exptree 
  | Div of exptree *  exptree 
  | Rem of exptree *  exptree 
  | Neg of  exptree 
  | Abs of  exptree 
;;
type opcode = CONST of bigint | PLUS | TIMES | MINUS | DIV | REM | ABS | UNARYMINUS ;;

let rec eval (a:exptree) = match a with
| N(i) -> i
| Plus(x,y) -> (eval x) + (eval y)
| Minus(x,y) -> (eval x) - (eval y)
| Mult(x,y) -> (eval x)*(eval y)
| Div(x,y) -> (eval x)/(eval y)
| Rem(x,y) -> (eval x) mod (eval y)
| Neg(x) -> ~-(eval x)
| Abs(x) -> if (eval x) < 0 then ~-(eval x) else eval x
;;

let rec compile (a:exptree) = match a with
| N(i) -> [CONST(mk_big i)]
| Plus(x,y) -> (compile x)@(compile y)@[PLUS]
| Minus(x,y) -> (compile x)@(compile y)@[MINUS]
| Mult(x,y) -> (compile x)@(compile y)@[TIMES]
| Div(x,y) -> (compile x)@(compile y)@[DIV]
| Rem(x,y) -> (compile x)@(compile y)@[REM]
| Neg(x) -> (compile x)@[UNARYMINUS]
| Abs(x) -> (compile x)@[ABS]
;;

let rec stackmc (s:bigint list) (o:opcode list) = match (o,s) with
| ([],hd::tl) -> hd
| (CONST(i)::tl,s) -> stackmc (i::s) tl
| (PLUS::tl1,hd1::hd2::tl) -> stackmc ((add hd1 hd2)::tl) tl1
| (TIMES::tl1,hd1::hd2::tl) -> stackmc ((mult hd1 hd2)::tl) tl1
| (MINUS::tl1,hd1::hd2::tl) -> stackmc ((sub hd1 hd2)::tl) tl1
| (DIV::tl1,hd1::hd2::tl) -> stackmc ((div hd1 hd2)::tl) tl1
| (REM::tl1,hd1::hd2::tl) -> stackmc ((rem hd1 hd2)::tl) tl1
| (ABS::tl1,hd1::tl) -> stackmc ((abs hd1)::tl) tl1
| (UNARYMINUS::tl1,hd1::tl) -> stackmc ((minus hd1)::tl) tl1
;;

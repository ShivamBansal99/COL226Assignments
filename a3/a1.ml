open A0
type  exptree =  Done (* End of input *)
  | Var of string (* variables starting with a Capital letter, represented as alphanumeric strings with underscores (_) and apostrophes (') *)
  | N of int      (* Integer constant *)
  | B of bool     (* Boolean constant *)
  (* unary operators on integers *)
  | Abs of exptree                   (* abs *)
  | Negative of exptree              (* unary minus ~ *)
  (* unary operators on booleans *)
  | Not of exptree
  (* binary operators on integers *)
  | Add of exptree * exptree         (* Addition + *)
  | Sub of exptree * exptree         (* Subtraction - *)
  | Mult of exptree * exptree        (* Multiplication * *)
  | Div of exptree * exptree         (* div *)
  | Rem of exptree * exptree         (* mod *)
  (* binary operators on booleans *)
  | Conjunction of exptree * exptree (* conjunction /\ *)
  | Disjunction of exptree * exptree (* binary operators on booleans \/ *)
  (* comparison operations on integers *)
  | Equals of exptree * exptree      (* = *)
  | GreaterTE of exptree * exptree   (* >= *)
  | LessTE of exptree * exptree      (* <= *)
  | GreaterT of exptree * exptree    (* > *)
  | LessT of exptree * exptree       (* < *)
  (* expressions using parenthesis *)
  | InParen of exptree               (* ( ) *)
  (* a conditional expression *)
  | IfThenElse of exptree * exptree * exptree (* if then else fi  *)
  (* creating n-tuples (n >= 0) *)
  | Tuple of int * (exptree list)
  (* projecting the i-th component of an expression (which evaluates to an n-tuple, and 1 <= i <= n) *)
  | Project of (int*int) * exptree   (* Proj((i,n), e)  0 < i <= n *)

type opcode = NCONST of bigint | BCONST of bool | ABS | UNARYMINUS | NOT
  | PLUS | MINUS | MULT | DIV | REM | CONJ | DISJ | EQS | GTE | LTE | GT | LT
  | PAREN | IFTE | TUPLE of int | PROJ of int*int

type answer = Num of bigint | Bool of bool | Tup of int * (answer list)

let rec get_nth = function
    | [], _ -> raise (Failure "get_nth")
    | _, n when n < 0 -> raise (Invalid_argument "get_nth")
    | x::_, 0 -> x
    | x::xs, n -> get_nth(xs, n-1)
;;
    let rec map f x rho= match x with
      | [] -> []
      | a::l ->  (f a rho) :: map f l rho
and eval (a:exptree) rho= match a with
|Var(i) -> rho i
| N(i) -> Num(mk_big(i))
| B(i) -> Bool(i)
| Add(x,y) -> (match (eval x rho ,eval y rho) with
	| (Num(j),Num(k))-> Num(add j k)
	| _ -> failwith "not possible")
| Sub(x,y) -> (match (eval x rho ,eval y rho) with
	| (Num(j),Num(k))-> Num(sub j k)
	| _ -> failwith "not possible")
| Mult(x,y) -> (match (eval x rho ,eval y rho) with
	| (Num(j),Num(k))-> Num(mult j k)
	| _ -> failwith "not possible")
| Div(x,y) -> (match (eval x rho ,eval y rho) with
	| (Num(j),Num(k))-> Num(div j k)
	| _ -> failwith "not possible")
| Rem(x,y) -> (match (eval x rho ,eval y rho) with
	| (Num(j),Num(k))-> Num(rem j k)
	| _ -> failwith "not possible")
| Negative(x) -> (match (eval x rho) with
	| (Num(j))-> Num(minus j)
	| _ -> failwith "not possible")
| Abs(x) -> (match (eval x rho) with
	| (Num(j))-> Num(abs j)
	| _ -> failwith "not possible")
| Conjunction(x,y) -> ( match (eval x rho, eval y rho) with
	| (Bool(i),Bool(j)) -> Bool(i && j)
	| _ -> failwith "not possible")
| Disjunction(x,y)-> ( match (eval x rho, eval y rho) with
	| (Bool(i),Bool(j)) -> Bool(i or j)
	| _ -> failwith "not possible"
	)
|Equals(x,y) -> (match (eval x rho ,eval y rho) with
	| (Num(j),Num(k))-> Bool(eq j k)
	| _ -> failwith "not possible")
| GreaterTE(x,y) -> (match (eval x rho ,eval y rho) with
	| (Num(j),Num(k))-> Bool(geq j k)
	| _ -> failwith "not possible")
| LessTE(x,y) -> (match (eval x rho ,eval y rho) with
	| (Num(j),Num(k))-> Bool(leq j k)
	| _ -> failwith "not possible")
| GreaterT(x,y) -> (match (eval x rho ,eval y rho) with
	| (Num(j),Num(k))-> Bool(gt j k)
	| _ -> failwith "not possible")
| LessT(x,y) -> (match (eval x rho ,eval y rho) with
	| (Num(j),Num(k))-> Bool(lt j k)
	| _ -> failwith "not possible")
| InParen(x) -> eval x rho
| IfThenElse(x,y,z) -> (match (eval x rho) with
		Bool(i) -> if i then eval y rho else eval z rho
	)
| Tuple(i,x) -> Tup(i,map eval x rho)
| Project((i,n),x) -> match eval x rho with
	| Tup(j,k) -> if i<=j then get_nth (k,i-1) else failwith "not possible"
;;
(*
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
| (MINUS::tl1,hd1::hd2::tl) -> stackmc ((sub hd2 hd1)::tl) tl1
| (DIV::tl1,hd1::hd2::tl) -> stackmc ((div hd1 hd2)::tl) tl1
| (REM::tl1,hd1::hd2::tl) -> stackmc ((rem hd1 hd2)::tl) tl1
| (ABS::tl1,hd1::tl) -> stackmc ((abs hd1)::tl) tl1
| (UNARYMINUS::tl1,hd1::tl) -> stackmc ((minus hd1)::tl) tl1
;;
*)

(* Dummy implementation of A1 *)
open A0
exception Not_implemented

(* abstract syntax *)
type  exptree =
  Var of string (* variables starting with a Capital letter, represented as alphanumeric strings with underscores (_) and apostrophes (') *)
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
  | Let of definition * exptree
  | FunctionAbstraction of (string * exptype) * exptree
  | FunctionCall of exptree * exptree
(* definition *)
and definition =
    Simple of (string * exptype) * exptree
  | Sequence of (definition list)
  | Parallel of (definition list)
  | Local of definition * definition

(* opcodes of the stack machine (in the same sequence as above) *)
and opcode = VAR of string | NCONST of bigint | BCONST of bool | ABS | UNARYMINUS | NOT
  | PLUS | MINUS | MULT | DIV | REM | CONJ | DISJ | EQS | GTE | LTE | GT | LT
  | PAREN | IFTE | TUPLE of int | PROJ of int*int | LET | FABS | FCALL
  | SIMPLEDEF | SEQCOMPOSE | PARCOMPOSE | LOCALDEF

(* The possible types of expressions in the language of expressions *)
and exptype = Tstray | Tint | Tunit | Tbool | Ttuple of (exptype list) | Tfunc of (exptype * exptype)

(* The type of value returned by the definitional interpreter. *)
and value = NumVal of int | BoolVal of bool | TupVal of int * (value list)

(* The language should contain the following types of expressions:  integers and booleans *)
and answer = Num of bigint | Bool of bool | Tup of int * (answer list)


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
| N(i) -> NumVal(i)
| B(i) -> BoolVal(i)
| Add(x,y) -> (match (eval x rho ,eval y rho) with
	| (NumVal(j),NumVal(k))-> NumVal(j + k)
	| _ -> failwith "not possible")
| Sub(x,y) -> (match (eval x rho ,eval y rho) with
	| (NumVal(j),NumVal(k))-> NumVal( j - k)
	| _ -> failwith "not possible")
| Mult(x,y) -> (match (eval x rho ,eval y rho) with
	| (NumVal(j),NumVal(k))-> NumVal( j * k)
	| _ -> failwith "not possible")
| Div(x,y) -> (match (eval x rho ,eval y rho) with
	| (NumVal(j),NumVal(k))-> NumVal(j/k)
	| _ -> failwith "not possible")
| Rem(x,y) -> (match (eval x rho ,eval y rho) with
	| (NumVal(j),NumVal(k))-> NumVal( j mod k)
	| _ -> failwith "not possible")
| Negative(x) -> (match (eval x rho) with
	| (NumVal(j))-> NumVal(-1*j)
	| _ -> failwith "not possible")
| Abs(x) -> (match (eval x rho) with
	| (NumVal(j))-> NumVal( if j>=0 then j else -1*j)
	| _ -> failwith "not possible")
| Conjunction(x,y) -> ( match (eval x rho, eval y rho) with
	| (BoolVal(i),BoolVal(j)) -> BoolVal(i && j)
	| _ -> failwith "not possible")
| Disjunction(x,y)-> ( match (eval x rho, eval y rho) with
	| (BoolVal(i),BoolVal(j)) -> BoolVal(i or j)
	| _ -> failwith "not possible"
	)
|Equals(x,y) -> (match (eval x rho ,eval y rho) with
	| (NumVal(j),NumVal(k))-> BoolVal(j=k)
	| _ -> failwith "not possible")
| GreaterTE(x,y) -> (match (eval x rho ,eval y rho) with
	| (NumVal(j),NumVal(k))-> BoolVal( j>= k)
	| _ -> failwith "not possible")
| LessTE(x,y) -> (match (eval x rho ,eval y rho) with
	| (NumVal(j),NumVal(k))-> BoolVal(j<= k)
	| _ -> failwith "not possible")
| GreaterT(x,y) -> (match (eval x rho ,eval y rho) with
	| (NumVal(j),NumVal(k))-> BoolVal(j> k)
	| _ -> failwith "not possible")
| LessT(x,y) -> (match (eval x rho ,eval y rho) with
	| (NumVal(j),NumVal(k))-> BoolVal( j< k)
	| _ -> failwith "not possible")
| InParen(x) -> eval x rho
| IfThenElse(x,y,z) -> (match (eval x rho) with
		BoolVal(i) -> if i then eval y rho else eval z rho
	)
| Tuple(i,x) -> TupVal(i,map eval x rho)
| Project((i,n),x) -> match eval x rho with
	| TupVal(j,k) -> if i<=j && n=j then get_nth (k,i-1) else failwith "not possible"
;;
let rec append_all f x= match x with
| [] -> []
| x::xs -> (append_all f xs)@(f x)
;;
let rec compile (a:exptree) = match a with
| N(i) -> [NCONST(mk_big i)]
| B(i) -> [BCONST(i)]
| Var(i) -> [VAR(i)]
| Add(x,y) -> (compile x)@(compile y)@[PLUS]
| Sub(x,y) -> (compile x)@(compile y)@[MINUS]
| Mult(x,y) -> (compile x)@(compile y)@[MULT]
| Div(x,y) -> (compile x)@(compile y)@[DIV]
| Rem(x,y) -> (compile x)@(compile y)@[REM]
| Negative(x) -> (compile x)@[UNARYMINUS]
| Abs(x) -> (compile x)@[ABS]
| Conjunction(x,y) -> (compile x)@(compile y)@[CONJ]
| Disjunction(x,y) -> (compile x)@(compile y)@[DISJ]
| Equals(x,y) -> (compile x)@(compile y)@[EQS]
| GreaterTE(x,y) -> (compile x)@(compile y)@[GTE]
| LessTE(x,y) -> (compile x)@(compile y)@[LTE]
| GreaterT(x,y) -> (compile x)@(compile y)@[GT]
| LessT(x,y) -> (compile x)@(compile y)@[LT]
| InParen(x) -> (compile x)@[PAREN]
| IfThenElse(x,y,z) -> (compile x)@(compile y)@(compile z)@[IFTE]
| Tuple(x,y) -> (append_all compile y)@[TUPLE(x)]
| Project((x,y),z) -> (compile z)@[PROJ(x,y)]
;;
let get_bigint a = match a with
| Num(i) -> i
;;
let get_bool a = match a with
| Bool(i) -> i
;;
let rec firstk k xs = match xs with
| [] -> if k>0 then failwith "firstk" else []
| x::xs -> if k=1 then [x] else x::firstk (k-1) xs
;;
let rec lastk k xs = match xs with
| [] ->if k>0 then failwith "firstk" else []
| x::xs -> if k=1 then xs else lastk (k-1) xs
;;
let rec stackmc (s:answer list) rho (o:opcode list) = match (o,s) with
| ([],hd::tl) -> hd
| (VAR(i)::tl,s) -> stackmc ((rho i)::s) rho tl
| (NCONST(i)::tl,s) -> stackmc ((Num(i))::s) rho tl
| (BCONST(i)::tl,s) -> stackmc ((Bool(i))::s) rho tl
| (PLUS::tl1,hd1::hd2::tl) -> stackmc ((Num(add (get_bigint hd1) (get_bigint hd2)))::tl) rho tl1
| (MULT::tl1,hd1::hd2::tl) -> stackmc (Num(mult (get_bigint hd1) (get_bigint hd2))::tl) rho tl1
| (MINUS::tl1,hd1::hd2::tl) -> stackmc (Num(sub  (get_bigint hd2) (get_bigint hd1))::tl) rho tl1
| (DIV::tl1,hd1::hd2::tl) -> stackmc (Num(div (get_bigint hd1) (get_bigint hd2))::tl) rho tl1
| (REM::tl1,hd1::hd2::tl) -> stackmc (Num(rem (get_bigint hd2) (get_bigint hd1))::tl) rho tl1
| (ABS::tl1,hd1::tl) -> stackmc (Num(abs (get_bigint hd1))::tl) rho tl1
| (UNARYMINUS::tl1,hd1::tl) -> stackmc (Num(minus (get_bigint hd1))::tl) rho tl1
| (CONJ::tl1,hd1::hd2::tl) -> stackmc (Bool((get_bool hd1) && (get_bool hd2))::tl) rho tl1
| (DISJ::tl1,hd1::hd2::tl) -> stackmc (Bool((get_bool hd1) || (get_bool hd2))::tl) rho tl1
| (EQS::tl1,hd1::hd2::tl) -> stackmc (Bool((get_bigint hd1) = (get_bigint hd2))::tl) rho tl1
| (GTE::tl1,hd1::hd2::tl) -> stackmc (Bool((get_bigint hd1) <= (get_bigint hd2))::tl) rho tl1
| (LTE::tl1,hd1::hd2::tl) -> stackmc (Bool((get_bigint hd1) >= (get_bigint hd2))::tl) rho tl1
| (GT::tl1,hd1::hd2::tl) -> stackmc (Bool((get_bigint hd1) < (get_bigint hd2))::tl) rho tl1
| (LT::tl1,hd1::hd2::tl) -> stackmc (Bool((get_bigint hd1) > (get_bigint hd2))::tl) rho tl1
| (PAREN::tl1,tl) -> stackmc (tl) rho tl1
| (IFTE::tl1,hd1::hd2::hd3::tl) -> stackmc (if hd3=Bool(true) then hd2::tl else hd1::tl) rho tl1
| (TUPLE(i)::tl1,tl) -> stackmc ((Tup(i,(firstk i tl)))::(lastk i tl)) rho tl1
| (PROJ(i,j)::tl1,hd1::tl) -> stackmc ((match hd1 with
                                        | Tup(k,l)-> if i<=j && j=k then get_nth(l,i-1) else failwith "not possible")::tl) rho tl1

;;

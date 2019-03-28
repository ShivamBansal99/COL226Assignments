open A1
exception Not_implemented

let rec find g v = match g with
| [] -> failwith "exceptional variable (raised by shivam)"
| (a,b)::tl -> if a=v then b else find tl v
;;
let rec match_all f g e t = match (e,t) with
| ([],[]) -> true
| (e1::et,t1::tt) -> (f g e1 t1 ) && (match_all f g et tt)
| _ ->false
;;
(* hastype : ((string * exptype) list) -> exptree -> exptype -> bool *)
let rec hastype g e t = match e with
| N(i) -> (t=Tint)
| B(i) -> (t=Tbool)
| Var(i) -> ((find g i)=t)
| Add(x,y) -> (hastype g x t) && (hastype g y t) && t=Tint
| Sub(x,y) -> (hastype g x t) && (hastype g y t) && t=Tint
| Mult(x,y) -> (hastype g x t) && (hastype g y t) && t=Tint
| Div(x,y) -> (hastype g x t) && (hastype g y t) && t=Tint
| Rem(x,y) -> (hastype g x t) && (hastype g y t) && t=Tint
| Negative(x) -> (hastype g x t) && t=Tint
| Abs(x) -> (hastype g x t) && t=Tint
| Conjunction(x,y) -> (hastype g x t) && (hastype g y t) && t=Tbool
| Disjunction(x,y) -> (hastype g x t) && (hastype g y t) && t=Tbool
| Equals(x,y) -> (hastype g x t) && (hastype g y t) && t=Tint
| GreaterTE(x,y) -> (hastype g x t) && (hastype g y t) && t=Tint
| LessTE(x,y) -> (hastype g x t) && (hastype g y t) && t=Tint
| GreaterT(x,y) -> (hastype g x t) && (hastype g y t) && t=Tint
| LessT(x,y) -> (hastype g x t) && (hastype g y t) && t=Tint
| InParen(x) -> (hastype g x t)
| IfThenElse(x,y,z) -> (hastype g x Tbool) && (hastype g y t) && (hastype g z t)
| Tuple(x,y) -> (match t with
  | Ttuple(tlist) -> match_all hastype g y tlist
  | _ -> false)
| Project((x,y),z) -> hastype g (List.nth (match z with
    | Tuple(x,y) -> y
    | _ -> failwith "shivam") x) t

(*TODO: let implementation*)

| Let(d,x) -> raise Not_implemented

| FunctionAbstraction(s,x) -> (match t with
  | Tfunc(t1,t2) -> hastype ((s,t1)::g) x t2
  | _ -> false)

| FunctionCall(x,y) -> raise Not_implemented
;;

(* yields : ((string * exptree) list) -> definition -> ((string * exptree) list) -> bool *)
let rec yields g d g_dash = match d with
| Simple(s,x) -> ((s,x)::g)=g_dash
| Sequence(l) -> (match l with
  | [] -> g=g_dash
  | hd::tl -> false)
;;

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
let rec gettype g e = match e with
| N(i) -> (Tint)
| B(i) -> (Tbool)
| Var(i) -> ((find g i))
| Add(x,y) ->  if (gettype g x=Tint) && (gettype g y =Tint) then Tint else failwith "not possible"
| Sub(x,y) -> if (gettype g x=Tint) && (gettype g y =Tint) then Tint else failwith "not possible"
| Mult(x,y) -> if (gettype g x=Tint) && (gettype g y =Tint) then Tint else failwith "not possible"
| Div(x,y) -> if (gettype g x=Tint) && (gettype g y =Tint) then Tint else failwith "not possible"
| Rem(x,y) -> if (gettype g x=Tint) && (gettype g y =Tint) then Tint else failwith "not possible"
| Negative(x) -> if (gettype g x=Tint)  then Tint else failwith "not possible"
| Abs(x) -> if (gettype g x=Tint)  then Tint else failwith "not possible"
| Conjunction(x,y) -> if (gettype g x=Tbool) && (gettype g y =Tbool) then Tbool else failwith "not possible"
| Disjunction(x,y) -> if (gettype g x=Tbool) && (gettype g y =Tbool) then Tbool else failwith "not possible"
| Equals(x,y) -> if (gettype g x=Tint) && (gettype g y =Tint) then Tint else failwith "not possible"
| GreaterTE(x,y) -> if (gettype g x=Tint) && (gettype g y =Tint) then Tint else failwith "not possible"
| LessTE(x,y) -> if (gettype g x=Tint) && (gettype g y =Tint) then Tint else failwith "not possible"
| GreaterT(x,y) -> if (gettype g x=Tint) && (gettype g y =Tint) then Tint else failwith "not possible"
| LessT(x,y) -> if (gettype g x=Tint) && (gettype g y =Tint) then Tint else failwith "not possible"
| InParen(x) -> (gettype g x )
| IfThenElse(x,y,z) -> if (gettype g x=Tbool) && (gettype g y) = (gettype g z) then gettype g y else failwith "not possibe"
| Tuple(x,y) -> Ttuple(List.map (gettype g) y)
| Project((x,y),z) -> (match gettype z with
    | Ttuple(x) -> List.nth x n
    | _ -> failwith "shivam")

(*TODO: let implementation*)

| Let(d,x) -> gettype (getTable g d) x

| FunctionAbstraction(s,x) -> Tfunc(Tbool,Tbool)

| FunctionCall(x,y) -> (match (gettype x) with
  | Tfunc(t1,t2) -> if (gettype y)=t1 then t2 else failwith "other"
  | _ -> failwith "other"
  )
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

| Let(d,x) -> (gettype g Let(d,x)) = t

| FunctionAbstraction(s,x) -> (match t with
  | Tfunc(t1,t2) -> hastype ((s,t1)::g) x t2
  | _ -> false)

| FunctionCall(x,y) -> (match getType g x with
    | Tfunc(t1,t2) -> if (t1= gettype g y) && (t2= t) then true else false
    | _ -> false
  )
;;

let rec gettable g d = match d with
| Simple(s,x) -> (s,getType g x)::g
| Sequence(l) -> (match l with
  | [] -> g
  | hd::tl -> gettable (gettable g hd) Sequence(tl)
  )
| Parallel(l) -> (match l with
  | [] -> g
  | hd::tl -> gettable (gettable g hd) Sequence(tl)
  )
| Local(d1,d2) -> gettable (gettable g d1) d2
;;
(* yields : ((string * exptree) list) -> definition -> ((string * exptree) list) -> bool *)
let rec yields g d g_dash = match d with
| Simple(s,x) -> ((s,getType g x)::g)=g_dash
| Sequence(l) -> (match l with
  | [] -> g=g_dash
  | hd::tl -> )
;;

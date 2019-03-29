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
let rec gettable g d = (match d with
| Simple(s,x) -> [s,gettype g x]
| Sequence(l) -> (match l with
  | [] -> []
  | hd::tl -> (gettable g hd)@(gettable ((gettable g hd)@g) (Sequence(tl)))
  )
| Parallel(l) -> (match l with
  | [] -> []
  | hd::tl -> (gettable g hd)@(gettable g (Parallel(tl)))
  )
| Local(d1,d2) -> gettable ((gettable g d1)@g) d2)
and gettype g e = match e with
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
| Project((x,y),z) -> (match gettype g z with
    | Ttuple(l) ->if x<=y && y=List.length l then List.nth l (x-1) else failwith "aj"
    | _ -> failwith "shivam")

(*TODO: let implementation*)

| Let(d,x) -> gettype ((gettable g d)@g) x

| FunctionAbstraction(s,x) -> Tfunc(gettype g (Var(s)),(gettype ((s,gettype g (Var(s)))::g) x))

| FunctionCall(x,y) -> (match (gettype g x) with
  | Tfunc(t1,t2) -> if (gettype g y)=t1 then t2 else failwith "other"
  | _ -> failwith "other"
  )
;;

(* hastype : ((string * exptype) list) -> exptree -> exptype -> bool *)
let rec hastype g e t =try( match e with
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
  | _ -> failwith "fdsa")
| Project((x,y),z) -> (match gettype g z with
    | Ttuple(w) ->  if x<y && y=List.length w then (List.nth w (x-1)=t) else false
    | _ -> failwith "fdsa")

(*TODO: let implementation*)

| Let(d,x) -> (gettype g (Let(d,x))) = t

| FunctionAbstraction(s,x) -> (match t with
  | Tfunc(t1,t2) -> hastype ((s,t1)::g) x t2
  | _ -> false)

| FunctionCall(x,y) -> (match gettype g x with
    | Tfunc(t1,t2) -> if (t1= gettype g y) && (t2= t) then true else false
    | _ -> false
  )
) with _-> false
;;
let same_def a b = match (a,b) with
| ((x1,t1),(x2,t2)) -> x1=x2;
| _-> false
;;
let rec rem_occurances a x= match a with
| [] -> []
| hd::tl -> if same_def hd x then rem_occurances tl x else hd::(rem_occurances tl x)
;;
let rec normalise a = match a with
| [] -> []
| hd::tl -> hd::(normalise (rem_occurances tl hd))
;;
let rec find_and_rem a b= match b with
| [] -> failwith "not possible"
| hd:: tl -> if (hd=a) then tl else hd::(find_and_rem a tl)
;;
let rec table_eq g g_dash = match g with
| [] -> (match g_dash with
          | [] -> true
          | _-> false)
| hd::tl -> table_eq tl (find_and_rem hd g_dash)
;;

(* yields : ((string * exptree) list) -> definition -> ((string * exptree) list) -> bool *)
let rec yields g d g_dash =try table_eq (normalise (gettable g d)) (normalise g_dash) with _-> false
;;

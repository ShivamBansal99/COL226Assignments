open Signature_a0
  module A0 : BigInt = struct

type bigint = sign * int list
and sign = Neg | NonNeg;; 

(* checks and corrects invariant that every element is between 0 to 9*)
let rec inv a = match a with 
| [] -> []
| hd::sec::tl -> (hd mod 10) :: inv ((sec + hd/10) ::tl)
| hd::[] -> (hd mod 10) :: (if  hd/10=0 then [] else [hd/10])
;;

(* function to get first k elements of list*)
let rec firstk k xs = match xs with
| [] -> failwith "firstk"
| [x] -> if k=1 || k=2 then [x] else x::firstk (k-1) []
| x::xs -> if k=1 then [x] else x::firstk (k-1) xs;;

(* function to remove leading zeros*)
let rem_zero a = let b= List.rev a in 
let rec temp k = (match k with
| [] -> []
|0::tl -> temp tl
| x -> x)
in List.rev (temp b)
;;

(*adds reversed lists*)
let rec addList a b = match a with
| [] -> b
| hda::tla -> (match b with
	| [] -> hda::tla
	| hdb::[] -> ((hdb+hda) mod 10) :: (
		(if (hdb+hda)/10 = 0 then tla else (match tla with
			| [] -> [1]
			| hd::tl -> inv ((hd+1)::tl) )
		)
	)
	| hdb::secb::tl -> ((hdb+hda) mod 10) :: addList tla (inv ((secb+ (hdb+hda)/10)::tl) )
);;

(* multiplies reversed lists*)
let rec multList a b = match b with 
| [] -> []
| hd::[] -> rem_zero (inv (List.map ( fun x -> x*hd ) a))
| hd::tl -> rem_zero (addList (inv (List.map ( fun x -> x*hd ) a))  (0::(multList a tl)))
;;

(*checks if lists are equal*)
let rec equal_list a b = match a, b with
| [], [] -> true
| [], _
| _, [] -> false
| c::cc, d::dd -> if c = d then equal_list cc dd else false;;

(* checks if one reversed list is greater than or equal to other reversed list*)
let rec great_or_equal_list a b= match (a,b) with
| (_,[]) -> true
| ([],_) -> false
| (hda::[], hdb::[]) -> hda>=hdb
| (hda::tla, hdb::tlb) -> if (equal_list tla tlb) then hda>=hdb else great_or_equal_list tla tlb
;;

(*subtracts one from each element i.e. carries one*)
let rec carry a = match a with
| [] -> failwith "carry operation not possible"
| hd::tl -> if hd>0 then (hd-1)::tl else
 9::(carry tl)
;;

(* subtracts reversed lists*)
let rec subList a b = if great_or_equal_list a b then
(
match (a,b) with
| (a,[]) -> a
| (hda::[], hdb::[]) -> [hda-hdb]
| (hda::tla, hdb::tlb) -> if hda >= hdb then
   (hda-hdb)::(subList tla tlb)
  else
   (10 + hda - hdb):: (subList (carry tla) tlb)
| _ -> failwith "sub not possible" 
)
else
(
match (b,a) with
| (a,[]) -> a
| (hda::[], hdb::[]) -> [hda-hdb]
| (hda::tla, hdb::tlb) -> if hda >= hdb then
   (hda-hdb)::(subList tla tlb)
  else
   (10 + hda - hdb):: (subList (carry tla) tlb)
| _ ->failwith "sub not possible" 
)
;;

(* get last n elements of list*)
let rec lastn k a = match k with
| 0 -> a
| 1->  (match a with
| [] -> []
| hd::tl -> tl)
| k -> (match a with
| [] -> failwith "small list"
| hd::tl -> lastn (k-1) tl)
;;

(* gets remainder from reversed list devision*)
let rec rem_list a b = if great_or_equal_list a b then (
if great_or_equal_list (rem_zero(subList a b)) b then
rem_list (rem_zero(subList a b)) b
else
rem_zero (subList a b)
)
else a;;

(* corrects invariant: elements between 0-9*)
let rec sep a = match a with
| [] -> []
| hd::tl -> (hd mod 10)::(if hd/10 =0 then sep tl else (hd/10)::(sep tl));;

(*div by subtraction*)
let rec small_div_list a b accum = if great_or_equal_list a b then small_div_list (rem_zero (subList a b)) b (addList accum [1]) else accum;;

(*extracts first few elements of "a" that are less than "b"*)
let rec extract a b accum = if great_or_equal_list accum b then (match accum with 
                | [] -> ([],a)
                | hd::tl -> (tl,hd::a)
            )else (
            match a with 
                  | [] -> (accum,a)
                  | hd::tl -> extract tl b (hd::accum)
            );;
	    
(*division by long division method*)
let rec div_list_temp a b accum rem= match a with
            | [] -> accum
            | hd::tl -> div_list_temp tl b (accum@(if equal_list (small_div_list (hd::rem) b []) [] then [0] else (small_div_list (hd::rem) b []))) (rem_list (hd::rem) b);;

(* calls division with extract*)
let div_list a b =let c = extract a b [] in (
                    match c with
                    | (x,y) -> div_list_temp y b [] x 
                    ) ;;

(*comparision functions*)
let greater_than_list a b = (great_or_equal_list a b) && not (equal_list a b);;
let less_than_list a b = not (great_or_equal_list a b);;
let less_or_equal_list a b = not (greater_than_list a b);;

(* add for bigint*)
let add (a:bigint) (b:bigint) = match (a,b) with
| ((NonNeg, a) , (NonNeg, b)) -> ((NonNeg, List.rev (addList (List.rev a) (List.rev b))):bigint)
| ((Neg, a) , (Neg, b)) -> (Neg, List.rev (addList (List.rev a) (List.rev b)))
| ((Neg, a) , (NonNeg, b)) -> (if greater_than_list b a then (NonNeg,List.rev (rem_zero (subList (List.rev b) (List.rev a)))) else (Neg, List.rev (rem_zero (subList (List.rev b) (List.rev a)))))
| ((NonNeg, a) , (Neg, b)) -> (if greater_than_list b a then (Neg,List.rev (rem_zero (subList (List.rev b) (List.rev a)))) else (NonNeg, List.rev (rem_zero (subList (List.rev b) (List.rev a)))))
;;

(* does a-b of bigint*)
let sub (a:bigint) (b:bigint) = match (a,b) with
| ((Neg, a) , (NonNeg, b)) -> ((Neg, List.rev (addList (List.rev a) (List.rev b))):bigint)
| ((NonNeg, a) , (Neg, b)) -> (NonNeg, List.rev (addList (List.rev a) (List.rev b)))
| ((NonNeg, a) , (NonNeg, b)) -> (if greater_than_list b a then (Neg,List.rev (rem_zero (subList (List.rev b) (List.rev a)))) else (NonNeg, List.rev (rem_zero (subList (List.rev b) (List.rev a)))))
| ((Neg, a) , (Neg, b)) -> (if greater_than_list b a then (NonNeg,List.rev (rem_zero (subList (List.rev b) (List.rev a)))) else (Neg, List.rev (rem_zero (subList (List.rev b) (List.rev a)))))
;;

(*multiply for bigint*)
let mult (a:bigint) (b:bigint) = match (a,b) with
| ((Neg,a),(Neg,b))
| ((NonNeg,a),(NonNeg,b)) -> ((NonNeg, List.rev (multList (List.rev a) (List.rev b))):bigint)
| ((Neg,a),(NonNeg,b))
| ((NonNeg,a),(Neg,b)) -> ((Neg, List.rev (multList (List.rev a) (List.rev b))):bigint)
;;


(*(=) for bigint*)
let eq (a:bigint) (b:bigint) = match (a,b) with
| ((Neg,a),(Neg,b)) 
| ((NonNeg,a),(NonNeg,b)) -> equal_list (List.rev a) (List.rev b)
| ((Neg,a),(NonNeg,b))
| ((NonNeg,a),(Neg,b)) -> false
;;

(* div for bigint*)
let div (a:bigint) (b:bigint) =if (eq b (NonNeg,[])) || (eq b (Neg,[])) then failwith "not possible division" else  match (a,b) with
| ((Neg,a),(Neg,b)) -> ((NonNeg, (div_list (a) (List.rev b))):bigint)
| ((NonNeg,a),(NonNeg,b)) -> (NonNeg, (div_list (a) (List.rev b)))
| ((Neg,a),(NonNeg,b)) -> (Neg, (div_list (a) (List.rev b)))
| ((NonNeg,a),(Neg,b)) -> (Neg, (div_list (a) (List.rev b)))
;;

(*rem for bigint*)
let rem (a:bigint) (b:bigint) = sub a (mult b (div a b));;
let minus (a:bigint) = match a with
| (Neg,a) -> ((NonNeg,a):bigint)
| (NonNeg,a) -> (Neg,a)
;;

(*absolute value function*)
let abs (a:bigint) = match a with
|(Neg, a) -> (NonNeg,a)
|b -> b
;;

(*negation function*)
let minus (a:bigint) = match a with
| (_,a) -> ((NonNeg,a):bigint)
;;

(*(>) for bigint*)
let gt (a:bigint) (b:bigint) = match (a,b) with
| ((Neg,a),(Neg,b)) ->less_than_list (List.rev a) (List.rev b)
| ((NonNeg,a),(NonNeg,b)) -> greater_than_list (List.rev a) (List.rev b)
| ((Neg,a),(NonNeg,b)) -> false
| ((NonNeg,a),(Neg,b)) -> true
;;

(*comparison function for bigint*)
let geq (a:bigint) (b:bigint) = (gt a b) || (eq a b)
;;
let lt (a:bigint) (b:bigint) = not (geq a b)
;;
let leq (a:bigint) (b:bigint) = not (gt a b)
;;

(*returns int list as string*)
let rec print_list a = match a with
| [] -> ""
| hd::tl -> string_of_int(hd)^(print_list tl)
;;

(*returns number as string*)
let print_num (a:bigint) = match a with
|(Neg,a) -> "-"^(print_list a)
|(NonNeg,a) -> (print_list a)
;;

(*makes int list from int*)
let rec make_big_list i = match i with
| 0 -> []
| i -> (i mod 10)::(make_big_list (i/10))
;;

(*make bigint from int*)
let mk_big i = if i>=0 then ((NonNeg ,List.rev ( make_big_list i)):bigint) else (Neg,List.rev (make_big_list (-i)));;

end

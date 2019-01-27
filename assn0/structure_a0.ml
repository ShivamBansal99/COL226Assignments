open Signature_a0
  module A0 : BigInt = struct

type bigint = sign * int list
and sign = Neg | NonNeg;; 

(* a function to check the invariant that all elements are from 0 to 9. Works on reverse lists*)
let rec inv a = match a with 
| [] -> []
| hd::sec::tl -> (hd mod 10) :: inv ((sec + hd/10) ::tl)
| hd::[] -> (hd mod 10) :: (if  hd/10=0 then [] else [hd/10])
;;

(* function to remove zeros in front. Works on reverse lists*)
let rem_zero a = let b= List.rev a in 
let rec temp k = (match k with
| [] -> []
|0::tl -> temp tl
| x -> x)
in List.rev (temp b)
;;

(* adds reversed lists a, b rexursively by adding first element*)
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

(*Multiplies reversed lists a,b recursively. eg. mul 456 234 = (mul 456 4)+(mul 456 230) = 456*4 +10* (mul 456 23)*) 
let rec multList a b = match b with 
| [] -> []
| hd::[] -> rem_zero (inv (List.map ( fun x -> x*hd ) a))
| hd::tl -> rem_zero (addList (inv (List.map ( fun x -> x*hd ) a))  (0::(multList a tl)))
;;

(*checks if lists are equal recursively, by comparing head of both lists*)
let rec equal_list a b = match a, b with
| [], [] -> true
| [], _
| _, [] -> false
| c::cc, d::dd -> if c = d then equal_list cc dd else false;;

(* geq function for reversed lists. Works by comparing head elements and tail lists recursively*)
let rec great_or_equal_list a b= match (a,b) with
| (_,[]) -> true
| ([],_) -> false
| (hda::[], hdb::[]) -> hda>=hdb
| (hda::tla, hdb::tlb) -> if (equal_list tla tlb) then hda>=hdb else great_or_equal_list tla tlb
;;

(* (reversed list) carries 1 from head(if possible) else assigns 9 to head and carries from next element recursively*) 
let rec carry a = match a with
| [] -> failwith "carry operation not possible"
| hd::tl -> if hd>0 then (hd-1)::tl else
 9::(carry tl)
;;

(*subtracts reversed lists in same way as add_list adds the reversed lists*)
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

(* divides reversed lists and stores result in accum*)
let rec div_list a b accum = if great_or_equal_list a b then div_list (rem_zero (subList a b)) b (addList accum [1]) else accum;;

(* finds remainder of reversed lists after division*) 
let rec rem_list a b = if great_or_equal_list a b then (
if great_or_equal_list (rem_zero(subList a b)) b then
rem_list (rem_zero(subList a b)) b
else
rem_zero (subList a b)
)
else a;;

(* some comparison functions for reversed lists*)
let greater_than_list a b = (great_or_equal_list a b) && not (equal_list a b);;
let less_than_list a b = not (great_or_equal_list a b);;
let less_or_equal_list a b = not (greater_than_list a b);;

(* checks for 4 cases of bigint and does add on two bigints*)
let add (a:bigint) (b:bigint) = match (a,b) with
| ((NonNeg, a) , (NonNeg, b)) -> ((NonNeg, List.rev (addList (List.rev a) (List.rev b))):bigint)
| ((Neg, a) , (Neg, b)) -> (Neg, List.rev (addList (List.rev a) (List.rev b)))
| ((Neg, a) , (NonNeg, b)) -> (if greater_than_list b a then (NonNeg,List.rev (rem_zero (subList (List.rev b) (List.rev a)))) else (Neg, List.rev (rem_zero (subList (List.rev b) (List.rev a)))))
| ((NonNeg, a) , (Neg, b)) -> (if greater_than_list b a then (Neg,List.rev (rem_zero (subList (List.rev b) (List.rev a)))) else (NonNeg, List.rev (rem_zero (subList (List.rev b) (List.rev a)))))
;;

(* does a-b on bigint by comparing 4 possible cases*)
let sub (a:bigint) (b:bigint) = match (a,b) with
| ((Neg, a) , (NonNeg, b)) -> ((Neg, List.rev (addList (List.rev a) (List.rev b))):bigint)
| ((NonNeg, a) , (Neg, b)) -> (NonNeg, List.rev (addList (List.rev a) (List.rev b)))
| ((NonNeg, a) , (NonNeg, b)) -> (if greater_than_list b a then (Neg,List.rev (rem_zero (subList (List.rev b) (List.rev a)))) else (NonNeg, List.rev (rem_zero (subList (List.rev b) (List.rev a)))))
| ((Neg, a) , (Neg, b)) -> (if greater_than_list b a then (NonNeg,List.rev (rem_zero (subList (List.rev b) (List.rev a)))) else (Neg, List.rev (rem_zero (subList (List.rev b) (List.rev a)))))
;;

(* multiplies two bigints*)
let mult (a:bigint) (b:bigint) = match (a,b) with
| ((Neg,a),(Neg,b))
| ((NonNeg,a),(NonNeg,b)) -> ((NonNeg, List.rev (multList (List.rev a) (List.rev b))):bigint)
| ((Neg,a),(NonNeg,b))
| ((NonNeg,a),(Neg,b)) -> ((Neg, List.rev (multList (List.rev a) (List.rev b))):bigint)
;;

(* divides two bigints*)
let div (a:bigint) (b:bigint) = match (a,b) with
| ((Neg,a),(Neg,b)) -> ((NonNeg, List.rev (div_list (List.rev a) (List.rev b) [0])):bigint)
| ((NonNeg,a),(NonNeg,b)) -> (NonNeg, List.rev (div_list (List.rev a) (List.rev b) [0]))
| ((Neg,a),(NonNeg,b)) -> (Neg, List.rev (div_list (List.rev a) (List.rev b) [0]))
| ((NonNeg,a),(Neg,b)) -> (Neg, List.rev (div_list (List.rev a) (List.rev b) [0]))
;;

(* remainder function*)
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

(* (=) for bigint*)
let eq (a:bigint) (b:bigint) = match (a,b) with
| ((Neg,a),(Neg,b)) 
| ((NonNeg,a),(NonNeg,b)) -> equal_list (List.rev a) (List.rev b)
| ((Neg,a),(NonNeg,b))
| ((NonNeg,a),(Neg,b)) -> false
;;

(* (>) for bigint*)
let gt (a:bigint) (b:bigint) = match (a,b) with
| ((Neg,a),(Neg,b)) ->less_than_list (List.rev a) (List.rev b)
| ((NonNeg,a),(NonNeg,b)) -> greater_than_list (List.rev a) (List.rev b)
| ((Neg,a),(NonNeg,b)) -> false
| ((NonNeg,a),(Neg,b)) -> true
;;

(* (>=) for bigint*)
let geq (a:bigint) (b:bigint) = (gt a b) || (eq a b)
;;

(* (<) for bigint*)
let lt (a:bigint) (b:bigint) = not (geq a b)
;;

(* (<=) for bigint*)
let leq (a:bigint) (b:bigint) = not (gt a b)
;;

(* returns the string of the integer in list*)
let rec print_list a = match a with
| [] -> ""
| hd::tl -> string_of_int(hd)^(print_list tl)
;;

(*returns string of bigint*)
let print_num (a:bigint) = match a with
|(Neg,a) -> "-"^(print_list a)
|(NonNeg,a) -> (print_list a)
;;

(* makes list of int*)
let rec make_big_list i = match i with
| 0 -> []
| i -> (i mod 10)::(make_big_list (i/10))
;;

(* makes bigint from int*)
let mk_big i = if i>=0 then ((NonNeg ,List.rev ( make_big_list i)):bigint) else (Neg,List.rev (make_big_list (-i)));;

end

type bigint = sign * int list
and sign = Neg | NonNeg;; 
let rec inv a = match a with 
| [] -> []
| hd::sec::tl -> (hd mod 10) :: inv ((sec + hd/10) ::tl)
| hd::[] -> (hd mod 10) :: (if  hd/10=0 then [] else [hd/10])
;;
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
let rec multList a b = match b with 
| [] -> []
| hd::[] -> inv (List.map ( fun x -> x*hd ) a)
| hd::tl -> addList (inv (List.map ( fun x -> x*hd ) a))  (0::(multList a tl))
;;
let rec great_or_equal_list a b= match (a,b) with
| (_,[]) -> true
| ([],_) -> false
| (hda::[], hdb::[]) -> hda>=hdb
| (hda::tla, hdb::tlb) -> great_or_equal_list tla tlb
;;
let rec carry a = match a with
| [] -> failwith "carry operation not possible"
| hd::tl -> if hd>0 then (hd-1)::tl else
 9::(carry tl)
;;
let rec subList a b = if great_or_equal_list a b then
(
match (a,b) with
| (a,[]) -> a
| (hda::[], hdb::[]) -> [hda-hdb]
| (hda::tla, hdb::tlb) -> if hda >= hdb then
   (hda-hdb)::(subList tla tlb)
  else
   (10 + hda - hdb):: (subList (carry tla) tlb) 
)
else
(
match (b,a) with
| (a,[]) -> a
| (hda::[], hdb::[]) -> (if hda-hdb <>0 then [hda-hdb] else [])
| (hda::tla, hdb::tlb) -> if hda >= hdb then
   (hda-hdb)::(subList tla tlb)
  else
   (10 + hda - hdb):: (subList (carry tla) tlb) 
)
;;
let rem_zero a = let b= List.rev a in 
let rec temp k = (match k with
| [] -> []
|0::tl -> temp tl
| x -> x)
in List.rev (temp b)
;;
let rec div_list a b accum = if great_or_equal_list a b then div_list (rem_zero (subList a b)) b (addList accum [1]) else accum;;
let rec rem_list a b = if great_or_equal_list a b then (
if great_or_equal_list (rem_zero(subList a b)) b then
rem_list (rem_zero(subList a b)) b
else
rem_zero (subList a b)
)
else a;;

let rec equal_list a b = match a, b with
| [], [] -> true
| [], _
| _, [] -> false
| c::cc, d::dd -> if c = d then equal_list cc dd else false;;

let greater_than_list a b = (great_or_equal_list a b) & not (equal_list a b);;
let less_than_list a b = not (great_or_equal_list a b);;
let less_or_equal_list a b = not (greater_than_list a b);;

let add (a:bigint) (b:bigint) = match (a,b) with
| ((NonNeg, a) , (NonNeg, b)) -> ((NonNeg, List.rev (addList (List.rev a) (List.rev b))):bigint)
| ((Neg, a) , (Neg, b)) -> (Neg, List.rev (addList (List.rev a) (List.rev b)))
| ((Neg, a) , (NonNeg, b)) -> (if greater_than_list b a then (NonNeg,List.rev (subList (List.rev b) (List.rev a))) else (Neg, List.rev (subList (List.rev b) (List.rev a))))
| ((NonNeg, a) , (Neg, b)) -> (if greater_than_list b a then (Neg,List.rev (subList (List.rev b) (List.rev a))) else (NonNeg, List.rev (subList (List.rev b) (List.rev a))))
;;
(* does a-b*)
let sub (a:bigint) (b:bigint) = match (a,b) with
| ((Neg, a) , (NonNeg, b)) -> ((Neg, List.rev (addList (List.rev a) (List.rev b))):bigint)
| ((NonNeg, a) , (Neg, b)) -> (NonNeg, List.rev (addList (List.rev a) (List.rev b)))
| ((NonNeg, a) , (NonNeg, b)) -> (if greater_than_list b a then (Neg,List.rev (subList (List.rev b) (List.rev a))) else (NonNeg, List.rev (subList (List.rev b) (List.rev a))))
| ((Neg, a) , (Neg, b)) -> (if greater_than_list b a then (NonNeg,List.rev (subList (List.rev b) (List.rev a))) else (Neg, List.rev (subList (List.rev b) (List.rev a))))
;;
let mult (a:bigint) (b:bigint) = match (a,b) with
| ((Neg,a),(Neg,b))
| ((NonNeg,a),(NonNeg,b)) -> ((NonNeg, List.rev (multList (List.rev a) (List.rev b))):bigint)
| ((Neg,a),(NonNeg,b))
| ((NonNeg,a),(Neg,b)) -> ((Neg, List.rev (multList (List.rev a) (List.rev b))):bigint)
;;

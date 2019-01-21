type bigint = sign * int list
and sign = Neg | NonNeg;; 
let rec addList a b = match a with
| [] -> b
| hda::tla -> (match b with
	| [] -> hda::tla
	| hdb::[] -> ((hdb+hda) mod 10) :: (
		(if (hdb+hda)/10 = 0 then tla else (match tla with
			| [] -> [1]
			| hd::tl -> (hd+1)::tl )
		)
	)
	| hdb::secb::tl -> ((hdb+hda) mod 10) :: addList tla ((secb+ (hdb+hda)/10)::tl) 
);;
let rec inv a = match a with 
| [] -> []
| hd::sec::tl -> (hd mod 10) :: inv ((sec + hd/10) ::tl)
| hd::[] -> (hd mod 10) :: (if  hd/10=0 then [] else [hd/10])
;;
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
| (hda::[], hdb::[]) -> [hda-hdb]
| (hda::tla, hdb::tlb) -> if hda >= hdb then
   (hda-hdb)::(subList tla tlb)
  else
   (10 + hda - hdb):: (subList (carry tla) tlb) 
)
;;

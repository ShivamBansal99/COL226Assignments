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
	| hdb::secb::tl -> ((hdb+hda) mod 10) :: addRev tla ((secb+ (hdb+hda)/10)::tl) 
);;
let rec inv a = match a with 
| [] -> []
| hd::sec::tl -> (hd mod 10) :: inv ((sec + hd/10) ::tl)
| hd::[] -> (hd mod 10) :: (if  hd/10=0 then [] else [hd/10])
;;
let rec multList a b = match b with 
| [] -> [0]
| hd::[] -> inv (List.map ( fun x -> x*hd ) a)
| hd::tl -> add (inv (List.map ( fun x -> x*hd ) a))  (0::(mult a tl));;


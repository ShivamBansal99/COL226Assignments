type bigint = sign * int list
and sign = Neg | NonNeg;; 
let rec add a b = match a with
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
let inv a = match a with 
| [] ->
let rec mult a b = match b with 
| [] -> [0]
| hd::tl -> add (inv (List.map ( fun x -> x*hd ) a))  (0::(mult a tl))

open Expression

(* This is the top level file for the expression evaluator. It is basically an infinite loop that consumes legal expression inputs
 * and prints their corresponding parse tree and evaluated output *)
type frame = FR of string * string list * ((((string, int) Hashtbl.t) * int)) *  (int) * string list * ((string, int) Hashtbl.t) * frame ref | NULL


let rec find_var v disp_reg = match !disp_reg with
| hd ::tl->(match !hd with FR (f,_,(l1,i),_,_,(l2),_) -> try Hashtbl.find l1 v with _->(try Hashtbl.find l2 v with _-> find_var v (ref tl)))
| _-> failwith "variable not declared"

let rec replace v i disp_reg = match !disp_reg with
| hd ::tl->(match !hd with FR (f,s1,(l1,_),_,s2,(l2),_) -> if List.mem v s1 then let ()=Printf.printf "i= %d\n " i; in Hashtbl.replace l1 v i else (if List.mem v s2 then Hashtbl.replace l2 v i else replace v i (ref tl)))
| _-> failwith "variable not declared"

let rec find_tree l1 f2 = match l1 with
| hd::tl -> if hd = f2 then true else find_tree tl f2
| [] -> false

let rec poss_then_update disp_reg fr tree = match (!disp_reg,fr) with
| (hd::tl,FR (f2,_,(l21,i21),i22,_,(l22),_ ))-> (match !hd with FR (f1,_,(l11,i11),i12,_,(l12),_) -> if (try find_tree (Hashtbl.find tree f1) f2 with _-> false) || f1=f2 then true else poss_then_update (ref tl) fr tree)
| _-> false

let rec get_hashtbl_from_list l sl e disp_reg= match (l,sl) with
| ((NUM(i))::tl,s::tls) -> let () = Hashtbl.replace e s i in get_hashtbl_from_list tl tls e disp_reg
| ((VAR(i))::tl,s::tls) -> let () = Hashtbl.replace e s (find_var i disp_reg) in get_hashtbl_from_list tl tls e disp_reg
| ([],[]) -> e
| _-> failwith "parameters sahi ni hai.. kuchh galat hai"

let rec update call_stack fr l disp_reg= match (fr,!call_stack) with
| (FR (f,s1,(l1,i1),i2,s2,(l2),point ),call_s) -> ref ((FR(f,s1,(get_hashtbl_from_list l s1 (Hashtbl.create 5) disp_reg,i1),i2,s2,Hashtbl.create 5,point))::call_s)

let update_link f1 f2 = match (!f1) with
| FR(a,b,c,d,e,f,g) -> let ()=f1:=FR(a,b,c,d,e,f,f2) in f1

let rec update_disp disp_reg call_stack tree = match (!disp_reg,!call_stack) with
| (hd::tl,FR (f2,_,(l21,i21),i22,_,(l22),_ )::tl2)->(match !hd with FR (f1,_,(l11,i11),i12,_,(l12),_) -> if (try find_tree (Hashtbl.find tree f1) f2 with _-> false)  then
      let ()= call_stack:=(!(update_link (ref (List.hd !call_stack)) (List.hd !disp_reg)))::tl2 in ref ((ref (List.hd !call_stack))::(!disp_reg))
      else ( if f1=f2 then
        let ()= call_stack:=(!(update_link (ref (List.hd !call_stack)) (List.hd !disp_reg)))::tl2 in ref ((ref (List.hd !call_stack))::(tl))
        else
        update_disp (ref tl) call_stack tree))
| _-> failwith "disp not possible"

let rec print_disp d = match d with
| (hd::tl) -> (match !hd with FR(f1,_,_,_,_,_,l) -> print_endline f1; print_disp tl)
| _-> ()

let rec print_callstack c = match c with
| (hd::tl) -> (match hd with FR(f1,_,_,_,_,_,l) -> print_endline f1; print_callstack tl)
| _-> ()

let rec print_vars d = match d with
| (hd::tl) -> (match !hd with FR(f1,s1,(l1,_),_,s2,l2,_) ->  Hashtbl.iter (fun x y -> Printf.printf "%s -> %d\n" x y) l1; Hashtbl.iter (fun x y -> Printf.printf "%s -> %d\n" x y) l2; print_vars tl)
| _-> ()

let rec print_list = function
[] -> ()
| e::l -> print_string e ; print_string " " ; print_list l

let rec print_u_vars d = match d with
| (hd::tl) -> (match !hd with FR(f1,s1,(l1,_),_,s2,l2,_) ->  print_list s1;print_string "\n" ; print_list s2;print_string "\n" ;print_u_vars tl)
| _-> ()

let rec get_disp called_f = match !called_f with
| NULL -> []
| FR(f1,_,_,_,_,_,link) -> called_f::(get_disp link)

let rec print_callable disp_reg tree = match !disp_reg with
| hd::tl -> (match !hd with FR(f1,s1,(l1,_),_,s2,l2,_) -> print_list (try Hashtbl.find tree f1 with _-> []);print_string "\n" ; print_callable (ref tl) tree )
| [] -> ()

let rec machine call_stack tree disp_reg frame_table =
Printf.printf "disp_reg: \n"; print_disp (!disp_reg);Printf.printf "\n";
Printf.printf "call_stack: \n"; print_callstack (!call_stack);Printf.printf "\n";
Printf.printf "callable_functions: \n"; print_callable (disp_reg) tree;Printf.printf "\n";
Printf.printf "Defined vars: \n"; print_vars (!disp_reg);Printf.printf "\n";
Printf.printf "Usable vars: \n"; print_u_vars (!disp_reg);
    Printf.printf "==> ";flush stdout;
    try
        let lexbuf = Lexing.from_channel stdin in
            let result = Parser.main Lexer.token lexbuf in
              (match result with
              | ASS(VAR(x),NUM(i)) -> let ()= replace x i disp_reg in machine call_stack tree disp_reg frame_table
              | ASS(VAR(x),VAR(i)) -> let () = replace x (find_var i disp_reg) disp_reg in machine call_stack tree disp_reg frame_table
              | CALL( VAR(f),l) ->if poss_then_update disp_reg (Hashtbl.find frame_table f) tree then let call_stack_new = update call_stack (Hashtbl.find frame_table f) l disp_reg in  machine (call_stack_new) tree (update_disp disp_reg (call_stack_new) tree) frame_table else Printf.printf "Not possible call\n";machine call_stack tree disp_reg frame_table
              | Ret -> (match (!call_stack,!disp_reg) with (*update disp_reg in ret*)
                | (hd1::tl1,hd2::tl2) ->let new_call_stack = (ref tl1) in machine (new_call_stack) tree (ref (get_disp (ref (List.hd !new_call_stack)))) frame_table
              )
              );
              Printf.printf "\n==> "; flush stdout
            (* flush ensures that the evaluated information gets printed to stdout *)
        with Lexer.Eof ->
            exit 0
;;
let tree = Hashtbl.create 5;;
let () = Hashtbl.replace tree "main" ["P";"Q"];Hashtbl.replace tree "P" ["R";"S"];Hashtbl.replace tree "R" ["V"];Hashtbl.replace tree "Q" ["T";"U"];Hashtbl.replace tree "T" ["W"] ;;
let frame_table = Hashtbl.create 5;;
let () = Hashtbl.replace frame_table "main" (FR("main",[],(Hashtbl.create 5, 0), 0,["a";"b";"c"],Hashtbl.create 5,ref NULL));Hashtbl.replace frame_table "P" (FR("P",["x";"y"],(Hashtbl.create 5, 0), 0,["z";"a"],Hashtbl.create 5,ref NULL));Hashtbl.replace frame_table "R" (FR("R",["w";"i"],(Hashtbl.create 5, 0), 0,["j";"b"],Hashtbl.create 5,ref NULL));Hashtbl.replace frame_table "V" (FR("V",["m";"n"],(Hashtbl.create 5, 0), 0,["c"],Hashtbl.create 5,ref NULL))
let () = Hashtbl.replace frame_table "S" (FR("S",["c";"k"],(Hashtbl.create 5, 0), 0,["m";"n"],Hashtbl.create 5,ref NULL));Hashtbl.replace frame_table "Q" (FR("Q",["z";"w"],(Hashtbl.create 5, 0), 0,["x";"b"],Hashtbl.create 5,ref NULL));Hashtbl.replace frame_table "T" (FR("T",["a";"y"],(Hashtbl.create 5, 0), 0,["i";"f"],Hashtbl.create 5,ref NULL))
let () = Hashtbl.replace frame_table "W" (FR("W",["m";"p"],(Hashtbl.create 5, 0), 0,["j";"h"],Hashtbl.create 5,ref NULL));Hashtbl.replace frame_table "U" (FR("U",["c";"z"],(Hashtbl.create 5, 0), 0,["p";"g"],Hashtbl.create 5,ref NULL))
let call_stack = ref [Hashtbl.find frame_table "main"]
let disp_reg = ref [ ref(List.hd !call_stack)];;
let () = try machine call_stack tree disp_reg frame_table with Failure("hd")-> exit 0;;

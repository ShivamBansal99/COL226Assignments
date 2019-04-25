open Expression

(* This is the top level file for the expression evaluator. It is basically an infinite loop that consumes legal expression inputs
 * and prints their corresponding parse tree and evaluated output *)
type frame = FR of string * string list * ((((string, int) Hashtbl.t) * int)) *  (int) * string list * ((string, int) Hashtbl.t)


let rec find_var v disp_reg = match !disp_reg with
| hd ::tl->(match !hd with FR (f,_,(l1,i),_,_,(l2)) -> try Hashtbl.find l1 v with _->(try Hashtbl.find l2 v with _-> find_var v (ref tl)))
| _-> failwith "empty disp_reg"

let rec replace v i disp_reg = match !disp_reg with
| hd ::tl->(match !hd with FR (f,s1,(l1,_),_,s2,(l2)) -> if List.mem v s1 then let ()=Printf.printf "i= %d\n " i; in Hashtbl.replace l1 v i else (if List.mem v s2 then Hashtbl.replace l2 v i else replace v i (ref tl)))
| _-> failwith "empty disp_reg"

let rec find_tree l1 f2 = match l1 with
| hd::tl -> if hd = f2 then true else find_tree tl f2
| [] -> false

let rec poss_then_update disp_reg fr tree = match (!disp_reg,fr) with
| (hd::tl,FR (f2,_,(l21,i21),i22,_,(l22) ))-> (match !hd with FR (f1,_,(l11,i11),i12,_,(l12)) -> if find_tree (Hashtbl.find tree f1) f2 || f1=f2 then true else poss_then_update (ref tl) fr tree)
| _-> false

let rec get_hashtbl_from_list l sl e= match (l,sl) with
| ((i)::tl,s::tls) -> let () = Hashtbl.replace e s i in get_hashtbl_from_list tl tls e
| ([],[]) -> e

let rec update call_stack fr l = match (fr,!call_stack) with
| (FR (f,s1,(l1,i1),i2,s2,(l2) ),call_s) -> ref ((FR(f,s1,(get_hashtbl_from_list l s1 (Hashtbl.create 5),i1),i2,s2,l2))::call_s)

let rec update_disp disp_reg call_stack tree = match (!disp_reg,!call_stack) with
| (hd::tl,FR (f2,_,(l21,i21),i22,_,(l22) )::tl2)->(match !hd with FR (f1,_,(l11,i11),i12,_,(l12)) -> if find_tree (Hashtbl.find tree f1) f2  then ref ((ref (List.hd !call_stack))::(!disp_reg)) else ( if f1=f2 then ref ((ref (List.hd !call_stack))::(tl)) else update_disp (ref tl) call_stack tree))
| _-> failwith "disp not possible"

let rec print_disp d = match d with
| (hd::tl) -> (match !hd with FR(f1,_,_,_,_,_) -> print_endline f1; print_disp tl)
| _-> ()

let rec print_callstack c = match c with
| (hd::tl) -> (match hd with FR(f1,_,_,_,_,_) -> print_endline f1; print_callstack tl)
| _-> ()

let rec print_vars d = match d with
| (hd::tl) -> (match !hd with FR(f1,s1,(l1,_),_,s2,l2) ->  Hashtbl.iter (fun x y -> Printf.printf "%s -> %d\n" x y) l1; Hashtbl.iter (fun x y -> Printf.printf "%s -> %d\n" x y) l2; print_vars tl)
| _-> ()

let rec machine call_stack tree disp_reg frame_table =
Printf.printf "disp_reg: "; print_disp (!disp_reg);Printf.printf "call_stack: "; print_callstack (!call_stack); Printf.printf "vars: "; print_vars (!disp_reg);
    Printf.printf "==> ";flush stdout;
    try
        let lexbuf = Lexing.from_channel stdin in
            let result = Parser.main Lexer.token lexbuf in print_tree 0 result;
              (match result with
              | ASS(VAR(x),NUM(i)) -> let ()= replace x i disp_reg in machine call_stack tree disp_reg frame_table
              | ASS(VAR(x),VAR(i)) -> let () = replace x (find_var i disp_reg) disp_reg in machine call_stack tree disp_reg frame_table
              | CALL( VAR(f),l) ->let () = print_endline ("call") in if poss_then_update disp_reg (Hashtbl.find frame_table f) tree then let call_stack_new = update call_stack (Hashtbl.find frame_table f) l in  machine (call_stack_new) tree (update_disp disp_reg (call_stack_new) tree) frame_table else failwith "not possible call"
              | Ret -> (match (!call_stack,!disp_reg) with (*update disp_reg in ret*)
                | (hd1::tl1,hd2::tl2) -> machine (ref tl1) tree (ref tl2) frame_table
              )
              );
              Printf.printf "\n==> "; flush stdout
            (* flush ensures that the evaluated information gets printed to stdout *)
        with Lexer.Eof ->
            exit 0
;;
let tree = Hashtbl.create 5 in let () = Hashtbl.replace tree "main" ["p";"q"] in
let frame_table = Hashtbl.create 5 in let () = Hashtbl.replace frame_table "main" (FR("main",["x";"y"],(Hashtbl.create 5, 0), 0,["u";"v"],Hashtbl.create 5)) in
let call_stack = ref [Hashtbl.find frame_table "main"] in
let disp_reg = ref [ ref(List.hd !call_stack)] in
machine call_stack tree disp_reg frame_table;;

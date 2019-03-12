#directory "_build";; (* Consider this folder when looking for files *)
#load "a0.cmo";; (* Load the a0 bytecode ((if T then 6 else 5 fi,proj (1,2) (4,5)),(7,9)) *)
#load "a1.cmo";;
#load "a2.cmo";;
#load "a3.cmo";;
open A0;;
open A1;;
open A2;;
open A3;;

exception Not_implemented
(* Helper function to print *)
let rec print_tuple t lev= match t with
  | [] -> Printf.printf ""
  | x::h -> print_tree x (lev); print_tuple h lev;
and print_tree t level = match t with

    | N(x)       -> Printf.printf "Level %d INT %d " level x;
    | Mult(t1,t2)  -> Printf.printf "Level %d *\n" level; print_tree t1 (level+1); print_tree t2 (level+1); print_newline();
    | Add(t1,t2)  -> Printf.printf "Level %d +\n" level; print_tree t1 (level+1); print_tree t2 (level+1); print_newline();
    | Tuple(t1,t2) -> print_newline();Printf.printf "%d \n (" t1;print_tuple t2 level;print_newline();
    | Project((i,n),e) -> print_newline();Printf.printf "%d %d \n" i n;print_tree e level;print_newline();
    |_          -> Printf.printf "Empty Tree\n";
;;

let rec print_answer tr = match tr with
  Num a -> let () = print_newline() in print_num a;
  | Bool a -> let () = print_newline() in string_of_bool a;
  | Tup(i,n) -> match n with
              | []->""
              | x::xs -> (print_answer x)^ " " ^(print_answer (Tup(i-1,xs)))
;;


(* Parser accepts the expression as string and binding as hash map with variable to values (integer, boolean, tuple) *)
let parser s binding =
  let result = A3.main A2.read (Lexing.from_string s) in
    (* Return the three versions as abstract syntax tree, value, compiled opcode*)
    (result(*), (A1.eval result binding), (A1.stackmc [] binding (A1.compile result))*))
;;

(* Input is given as string *)
let rho s = match s with
   "X_3'5" -> Num (A0.mk_big 5)
|  "Y" -> Bool true
|  "Z" -> Tup (3, [Num (A0.mk_big 5); Bool true; Num (A0.mk_big 1)]);;
let s= (parser "" rho)
let _ = Printf.printf "%s " (print_answer (stackmc [] rho  (compile s)));;
let _ = Printf.printf "%s \n" (print_answer (eval s rho));;

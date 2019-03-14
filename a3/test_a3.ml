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
let rec print_value tr = match tr with
  NumVal a -> let () = print_newline() in string_of_int a
  | BoolVal a -> let () = print_newline() in string_of_bool a
  | TupVal(i,n) -> match n with
              | []->""
              | x::xs -> (print_value x)^ " " ^(print_value (TupVal(i-1,xs)))
;;
let rec toAnswer v = match v with
  NumVal a     -> Num (mk_big a)
| BoolVal b    -> Bool b
| TupVal (n,xs) -> Tup (n, List.map toAnswer xs);;

let binding rho s = toAnswer (rho s);;

(* Parser accepts the expression as string and binding as hash map with variable to values (integer, boolean, tuple) *)
let parser s binding =
  let result = A3.main A2.read (Lexing.from_string s) in
    (* Return the three versions as abstract syntax tree, value, compiled opcode*)
    (result(*), (A1.eval result binding), (A1.stackmc [] binding (A1.compile result))*))
;;

(* Input is given as string *)
let rho s = match s with
   "X" -> NumVal 5
|  "Y" -> BoolVal true
|  "Z" -> TupVal (3, [NumVal 5; BoolVal true; NumVal 1]);;

let s= (parser " proj(1,4)(if if 5>3 then T else F fi /\\ F then 10 else ~3 fi, (T/\\F,F), proj(2,2)(0,~20),(1+3,2+4,3-6))" rho)
let _ = Printf.printf "%s " (print_answer (stackmc [] (binding rho)  (compile s)));;
let _ = Printf.printf "%s \n" (print_value (eval s rho));;

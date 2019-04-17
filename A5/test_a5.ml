#directory "_build";; (* Consider this folder when looking for files *)
#load "a5.cmo";;
#load "a2.cmo";;
#load "a3.cmo";;
open A2;;
open A3;;
open A5;;




exception Not_implemented
exception Definition_Error
(* Helper function to print *)


let e = Hashtbl.create 5
let exp_parser s = A3.exp_parser A2.read (Lexing.from_string s) ;;
let inp = exp_parser ("(rec Fib.\\X.(if (cmp X-1) then Fib(X-1) + Fib(X-2) else 1 fi ))(3)")
let p = App(Rec(V "f",V "x", If_Then_Else(Cmp (Minus(V "x",Integer 1)), Plus(App(V "f", Minus(V "x", Integer 1)),App(V "f", Minus(V "x", Integer 2))), Integer 1) ) ,Integer 6)
let inp2 = exp_parser ("(rec Fact.\\X.(if (cmp X) then Fact(X-1) * X else 1 fi ))(3)")
let p2 = App(Rec(V "f",V "x", If_Then_Else(Cmp (V "x"), Mult(App(V "f", Minus(V "x", Integer 1)),V "x"), Integer 1) ) ,Integer 50)
let a = stackmc [] e (compile inp2) []
let b = krivine (CLOSURE(inp2,e)) []

(*let p = App(Rec(V "f", Lambda (V "x", If_Then_Else(Cmp (V "x"), Mult(App(V "f", Minus(V "x", Integer 1)),V "x"), Integer 1) ) ),Integer 5)
let p = App(Rec(V "f", Lambda (V "x",
If_Then_Else (Cmp (Integer 0),
    App (V "f", Integer 0),
        Integer 110) ) ),Integer 2)


let p = App(Rec(V "f",V "x", If_Then_Else(Cmp (V "x"), Mult(App(V "f", Minus(V "x", Integer 1)),V "x"), Integer 1) ) ,Integer 50)
*)

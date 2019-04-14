type expr = V of string |
            Lambda of (expr * expr) |
            App of (expr * expr) |
             Plus of (expr * expr) |
             Minus of (expr * expr) |
              Mult of (expr * expr) |
              And of (expr * expr) |
              Or of (expr * expr) |
              Bool of bool |
              Integer of int |
              Cmp of expr |
              If_Then_Else of (expr * expr * expr) |
              Rec of (expr * expr * expr)
and answer = INT of int | BOOL of bool | BIGCLOSURE of (string * expr* ((string, answer) Hashtbl.t)) | CLOSURE of (expr* ((string, answer) Hashtbl.t))
;;
let rec krivine c s = match c with
| CLOSURE(V(str),e) -> krivine (Hashtbl.find e str) s
| CLOSURE(App(e1,e2),e) -> krivine (CLOSURE(e1,(Hashtbl.copy e))) (CLOSURE(e2,(Hashtbl.copy e))::s)
| CLOSURE(Lambda(V(str),e1),e) -> (match s with
  | CLOSURE(e2,gamma)::tl -> krivine (CLOSURE(e1,let () = Hashtbl.replace e str (CLOSURE(e2, Hashtbl.copy gamma)) in e)) tl
  )
|  CLOSURE(Plus (e1 ,e2),e) ->(match (krivine (CLOSURE(e1,Hashtbl.copy e)) s,krivine (CLOSURE(e2,Hashtbl.copy e)) s) with
  | (CLOSURE(Integer a,alpha),CLOSURE(Integer b, beta)) -> CLOSURE(Integer (a+b),e)
   )
|  CLOSURE(Minus (e1 ,e2),e) ->(match (krivine (CLOSURE(e1,Hashtbl.copy e)) s,krivine (CLOSURE(e2,Hashtbl.copy e)) s) with
  | (CLOSURE(Integer a,alpha),CLOSURE(Integer b, beta)) -> CLOSURE(Integer (a-b),e)
  )
|   CLOSURE(Mult (e1 ,e2),e) ->(match (krivine (CLOSURE(e1,Hashtbl.copy e)) s,krivine (CLOSURE(e2,Hashtbl.copy e)) s) with
  | (CLOSURE(Integer a,alpha),CLOSURE(Integer b, beta)) -> CLOSURE(Integer (a*b),e)
   )
|   CLOSURE(And (e1 ,e2),e) ->(match (krivine (CLOSURE(e1,Hashtbl.copy e)) s,krivine (CLOSURE(e2,Hashtbl.copy e)) s) with
  | (CLOSURE(Bool a,alpha),CLOSURE(Bool b, beta)) -> CLOSURE(Bool (a && b),e)
   )
|   CLOSURE(Or (e1 ,e2),e) ->(match (krivine (CLOSURE(e1,Hashtbl.copy e)) s,krivine (CLOSURE(e2,Hashtbl.copy e)) s) with
  | (CLOSURE(Bool a,alpha),CLOSURE(Bool b, beta)) -> CLOSURE(Bool (a || b),e)
   )
|   CLOSURE(Bool (b),e) ->CLOSURE(Bool (b),e)
|   CLOSURE(Integer (b),e) ->CLOSURE(Integer (b),e)
|   CLOSURE(Cmp (e1),e) ->(match krivine (CLOSURE(e1,Hashtbl.copy e)) s with
  | (CLOSURE(Integer a,alpha))-> if a>0 then CLOSURE(Bool true, Hashtbl.copy e) else CLOSURE(Bool false, Hashtbl.copy e)
  )
|   CLOSURE(If_Then_Else (e1,e2,e3),e) -> (match krivine (CLOSURE(e1,Hashtbl.copy e)) s with
  | (CLOSURE(Bool a,alpha))-> if a then krivine (CLOSURE(e2,Hashtbl.copy e)) s else krivine (CLOSURE(e3,Hashtbl.copy e)) s
  )
| CLOSURE(Rec(V(f),V(x),e1),e)->(match s with
  | CLOSURE(e2,gamma)::tl ->let gamma_new = Hashtbl.copy e in let ()= Hashtbl.replace gamma_new f (CLOSURE(Rec(V(f),V(x),e1),gamma_new)) in krivine (CLOSURE(e1,let () = Hashtbl.replace gamma_new x (CLOSURE(e2, gamma)) in gamma_new)) tl
  )

let e = Hashtbl.create 5
let p = App(Rec(V "f",V "x", If_Then_Else(Cmp (V "x"), Mult(App(V "f", Minus(V "x", Integer 1)),V "x"), Integer 1) ) ,Integer 20)

let a = krivine (CLOSURE(p,e)) []

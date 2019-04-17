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
and opcode= VAR of string | NCONST of int | BCONST of bool
  | PLUS |MINUS | MULT | OR | AND | IFTE of (opcode list * opcode list) | RET | APP | CMP | CLOS of (string * opcode list) | REC of (string * string * opcode list)
and  answer = INT of int | BOOL of bool | BIGCLOSURE of (string * opcode list* ((string, answer) Hashtbl.t)) |  CLOSURE of (expr* ((string, answer) Hashtbl.t))
;;
let rec compile (a:expr) = match a with
| Integer(i) -> [NCONST(i)]
| Bool(i) -> [BCONST(i)]
| V(i) -> [VAR(i)]
| Plus(x,y) -> (compile x)@(compile y)@[PLUS]
| Minus(x,y) -> (compile x)@(compile y)@[MINUS]
| Mult(x,y) -> (compile x)@(compile y)@[MULT]
| And(x,y) -> (compile x)@(compile y)@[AND]
| Or(x,y) -> (compile x)@(compile y)@[OR]
| If_Then_Else(x,y,z) -> (compile x)@[IFTE((compile y),(compile z))]
| Cmp(x) -> (compile x)@[CMP]
| App(x,y) -> (compile x)@(compile y) @[APP]
| Lambda(V(x),y) -> [(CLOS(x,(compile y)@[RET]))]
| Rec(V(f),V(x),e) -> [REC(f,x,(compile e)@[RET])]
;;
let rec stackmc (s:answer list) e c d = match c with
| [] -> (match s with
  | hd::tl -> hd
  | _-> failwith("stack empty"))
| (NCONST(i))::c1 -> stackmc ((INT i)::s) e c1 d
| VAR(s1)::c1 -> stackmc ((Hashtbl.find e s1)::s) e c1 d
| BCONST(b)::c1 -> stackmc ((BOOL b)::s) e c1 d
| PLUS::c1 -> (match s with
  | (INT a)::(INT b)::s1 -> stackmc (INT (a+b)::s1) e c1 d
  | _-> failwith("stack incomplete")
  )
| MINUS::c1 -> (match s with
    | (INT a)::(INT b)::s1 -> stackmc (INT (b-a)::s1) e c1 d
    | _-> failwith("stack incomplete")
  )
| MULT::c1 -> (match s with
  | (INT a)::(INT b)::s1 -> stackmc (INT (a*b)::s1) e c1 d
  | _-> failwith("stack incomplete")
  )
| OR::c1 -> (match s with
  | (BOOL a)::(BOOL b)::s1 -> stackmc (BOOL (a || b)::s1) e c1 d
  | _-> failwith("stack incomplete")
  )
| AND::c1 -> (match s with
  | (BOOL a)::(BOOL b)::s1 -> stackmc (BOOL (a && b)::s1) e c1 d
  | _-> failwith("stack incomplete")
  )
| IFTE(a,b)::c1 -> (match s with
  | (BOOL c)::s1 -> if c then stackmc (s1) e (a@c1) d else stackmc (s1) e (b@c1) d
  | _-> failwith("stack incomplete")
  )
| RET::c1 -> (match (s,d) with
  | (a::s1,(sd,ed,cd)::d) -> stackmc (a::sd) ed cd d
  | _-> failwith "not possible return"
  )
| APP::c1 -> (match s with
  | a::BIGCLOSURE (x,c,e1)::s1 ->let e2= (Hashtbl.copy e1) in let () =(Hashtbl.replace (e2) x a) in (stackmc [] e2 c ((s1,e,c1)::d))
  )
| CMP::c1 -> (match s with
  | INT a::s1 ->stackmc (BOOL(a>0)::s1) e c1 d
  | _-> failwith("stack incomplete")
  )
| CLOS(str,c1)::c2 -> stackmc (BIGCLOSURE(str,c1,(Hashtbl.copy e))::s) e c2 d
| REC(str1,str2,ex)::c1 ->let e1= (Hashtbl.copy e) in let ()= Hashtbl.replace e1 str1 (BIGCLOSURE(str2,ex,e1)) in stackmc (BIGCLOSURE(str2,ex,e1)::s) (let ()= Hashtbl.replace e str1 (BIGCLOSURE(str2,ex,e1)) in e ) c1 d
| _-> failwith "not implemented"


let rec krivine c s = match c with
| CLOSURE(V(str),e) -> krivine (Hashtbl.find e str) s
| CLOSURE(App(e1,e2),e) -> krivine (CLOSURE(e1,(Hashtbl.copy e))) (CLOSURE(e2,(Hashtbl.copy e))::s)
| CLOSURE(Lambda(V(str),e1),e) -> (match s with
  | CLOSURE(e2,gamma)::tl -> krivine (CLOSURE(e1,let () = Hashtbl.replace e str (CLOSURE(e2, Hashtbl.copy gamma)) in e)) tl
  )
|  CLOSURE(Plus (e1 ,e2),e) ->( krivine (CLOSURE(e1,Hashtbl.copy e)) ((CLOSURE(Plus(Integer 0,e2),Hashtbl.copy e))::s)
   )
|  CLOSURE(Minus (e1 ,e2),e) ->( krivine (CLOSURE(e1,Hashtbl.copy e)) ((CLOSURE(Minus(Integer 0,e2),Hashtbl.copy e))::s)
   )
|   CLOSURE(Mult (e1 ,e2),e) ->( krivine (CLOSURE(e1,Hashtbl.copy e)) ((CLOSURE(Mult(Integer 1,e2),Hashtbl.copy e))::s))
|   CLOSURE(And (e1 ,e2),e) ->( krivine (CLOSURE(e1,Hashtbl.copy e)) ((CLOSURE(And(Bool true,e2),Hashtbl.copy e))::s))
|   CLOSURE(Or (e1 ,e2),e) ->( krivine (CLOSURE(e1,Hashtbl.copy e)) ((CLOSURE(Or(Bool false,e2),Hashtbl.copy e))::s))
|   CLOSURE(Bool (b),e) ->(match s with
  | []-> BOOL (b)
  | (CLOSURE(And(Bool a , Bool true),gamma))::s -> krivine (CLOSURE(Bool (a && b),e)) s
  | (CLOSURE(And(Bool true , e2),gamma))::s -> krivine (CLOSURE(e2,gamma)) (CLOSURE(And(Bool b , Bool true),gamma)::s)
  | (CLOSURE(Or(Bool a , Bool true),gamma))::s -> krivine (CLOSURE(Bool (a || b),e)) s
  | (CLOSURE(Or(Bool true , e2),gamma))::s -> krivine (CLOSURE(e2,gamma)) (CLOSURE(Or(Bool b , Bool false),gamma)::s)
  | (CLOSURE(If_Then_Else(_ ,e1, e2),gamma))::s -> if b then krivine (CLOSURE(e1, Hashtbl.copy gamma)) s else krivine (CLOSURE(e2, Hashtbl.copy gamma)) s
  )
|   CLOSURE(Integer (b),e) ->(match s with
  | []-> INT (b)
  | (CLOSURE(Plus(Integer a , Integer 0),gamma))::s -> krivine (CLOSURE(Integer (a + b),e)) s
  | (CLOSURE(Plus(Integer 0 , e2),gamma))::s -> krivine (CLOSURE(e2,gamma)) (CLOSURE(Plus(Integer b , Integer 0),gamma)::s)
  | (CLOSURE(Minus(Integer a , Integer 0),gamma))::s ->krivine (CLOSURE(Integer (a - b),e)) s
  | (CLOSURE(Minus(Integer 0 , e2),gamma))::s -> krivine (CLOSURE(e2,gamma)) (CLOSURE(Minus(Integer b , Integer 0),e)::s)
  | (CLOSURE(Mult(Integer a , Integer 1),gamma))::s ->krivine (CLOSURE(Integer (a * b),e)) s
  | (CLOSURE(Mult(Integer 1 , e2),gamma))::s -> krivine (CLOSURE(e2,gamma)) (CLOSURE(Mult(Integer b , Integer 1),e)::s)
  | (CLOSURE(Cmp(_),_))::s -> krivine (CLOSURE(Bool (b>0),e)) s
  )

|   CLOSURE(Cmp (e1),e) ->krivine (CLOSURE(e1,Hashtbl.copy e)) (CLOSURE(Cmp(Bool true),e)::s)
|   CLOSURE(If_Then_Else (e1,e2,e3),e) -> krivine (CLOSURE(e1,Hashtbl.copy e)) (CLOSURE(If_Then_Else(Bool true,e2,e3),e)::s)
| CLOSURE(Rec(V(f),V(x),e1),e)->(match s with
  | CLOSURE(e2,gamma)::tl ->let gamma_new = Hashtbl.copy e in let ()= Hashtbl.replace gamma_new f (CLOSURE(Rec(V(f),V(x),e1),gamma_new)) in krivine (CLOSURE(e1,let () = Hashtbl.replace gamma_new x (CLOSURE(e2, gamma)) in gamma_new)) tl
  )

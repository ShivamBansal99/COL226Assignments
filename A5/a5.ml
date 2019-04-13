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
and  closure = INT of int | BOOL of bool | BIGCLOSURE of (string * opcode list* ((string, closure) Hashtbl.t))
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
let rec stackmc (s:closure list) e c d = match c with
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

let e = Hashtbl.create 5
let p = App(Rec(V "f",V "x", If_Then_Else(Cmp (V "x"), Mult(App(V "f", Minus(V "x", Integer 1)),V "x"), Integer 1) ) ,Integer 20)
let a = stackmc [] e (compile p) []
(*let p = App(Rec(V "f", Lambda (V "x", If_Then_Else(Cmp (V "x"), Mult(App(V "f", Minus(V "x", Integer 1)),V "x"), Integer 1) ) ),Integer 5)
let p = App(Rec(V "f", Lambda (V "x",
If_Then_Else (Cmp (Integer 0),
    App (V "f", Integer 0),
        Integer 110) ) ),Integer 2)

*)

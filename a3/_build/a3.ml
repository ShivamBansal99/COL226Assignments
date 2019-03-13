type token =
  | INT of (int)
  | BOOL of (bool)
  | ID of (string)
  | ABS
  | TILDA
  | NOT
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | REM
  | CONJ
  | DISJ
  | EQ
  | GT
  | LT
  | LP
  | RP
  | IF
  | THEN
  | ELSE
  | FI
  | COMMA
  | PROJ
  | EOF
  | DEF
  | DELIMITER

open Parsing;;
let _ = parse_error;;
# 2 "a3.mly"
    open A1
# 35 "a3.ml"
let yytransl_const = [|
  260 (* ABS *);
  261 (* TILDA *);
  262 (* NOT *);
  263 (* PLUS *);
  264 (* MINUS *);
  265 (* TIMES *);
  266 (* DIV *);
  267 (* REM *);
  268 (* CONJ *);
  269 (* DISJ *);
  270 (* EQ *);
  271 (* GT *);
  272 (* LT *);
  273 (* LP *);
  274 (* RP *);
  275 (* IF *);
  276 (* THEN *);
  277 (* ELSE *);
  278 (* FI *);
  279 (* COMMA *);
  280 (* PROJ *);
    0 (* EOF *);
  281 (* DEF *);
  282 (* DELIMITER *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* BOOL *);
  259 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\005\000\005\000\004\000\
\004\000\004\000\004\000\004\000\004\000\006\000\006\000\006\000\
\007\000\007\000\007\000\007\000\008\000\008\000\008\000\009\000\
\009\000\010\000\010\000\011\000\011\000\012\000\012\000\013\000\
\013\000\013\000\013\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\001\000\002\000\001\000\003\000\
\003\000\004\000\004\000\003\000\001\000\003\000\003\000\001\000\
\003\000\003\000\003\000\001\000\002\000\002\000\001\000\007\000\
\001\000\007\000\001\000\003\000\001\000\003\000\003\000\001\000\
\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\033\000\034\000\032\000\000\000\000\000\000\000\
\000\000\000\000\000\000\036\000\001\000\000\000\000\000\005\000\
\000\000\000\000\000\000\023\000\025\000\027\000\029\000\021\000\
\022\000\007\000\006\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\035\000\000\000\028\000\000\000\000\000\002\000\004\000\
\008\000\000\000\009\000\000\000\012\000\015\000\014\000\017\000\
\018\000\019\000\000\000\030\000\000\000\000\000\010\000\011\000\
\000\000\000\000\000\000\000\000\024\000\026\000"

let yydgoto = "\002\000\
\012\000\013\000\014\000\015\000\016\000\017\000\018\000\019\000\
\020\000\021\000\022\000\029\000\023\000"

let yysindex = "\011\000\
\005\255\000\000\000\000\000\000\000\000\051\255\051\255\005\255\
\005\255\005\255\006\255\000\000\000\000\019\255\022\255\000\000\
\000\255\253\254\063\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\053\255\021\255\023\255\040\255\005\255\
\005\255\051\255\016\255\045\255\051\255\051\255\051\255\051\255\
\051\255\000\000\005\255\000\000\005\255\028\255\000\000\000\000\
\000\000\051\255\000\000\051\255\000\000\000\000\000\000\000\000\
\000\000\000\000\034\255\000\000\024\255\062\255\000\000\000\000\
\005\255\066\255\058\255\064\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\037\000\031\000\000\000\
\025\000\013\000\001\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\067\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\249\255\054\000\056\000\248\255\079\000\023\000\038\000\076\000\
\000\000\022\000\000\000\048\000\000\000"

let yytablesize = 316
let yytable = "\026\000\
\020\000\028\000\030\000\037\000\038\000\003\000\004\000\005\000\
\006\000\007\000\008\000\001\000\016\000\034\000\035\000\036\000\
\003\000\004\000\005\000\006\000\007\000\009\000\031\000\010\000\
\013\000\049\000\051\000\053\000\011\000\050\000\007\000\032\000\
\009\000\033\000\010\000\059\000\003\000\061\000\044\000\011\000\
\046\000\063\000\045\000\064\000\065\000\003\000\004\000\005\000\
\006\000\007\000\062\000\003\000\004\000\005\000\006\000\007\000\
\043\000\067\000\052\000\054\000\055\000\009\000\066\000\010\000\
\003\000\004\000\005\000\009\000\011\000\010\000\042\000\039\000\
\040\000\041\000\011\000\043\000\056\000\057\000\058\000\069\000\
\009\000\024\000\025\000\068\000\031\000\047\000\027\000\011\000\
\048\000\070\000\060\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\020\000\
\020\000\000\000\000\000\000\000\020\000\020\000\020\000\020\000\
\020\000\000\000\020\000\000\000\020\000\020\000\020\000\020\000\
\016\000\016\000\016\000\016\000\016\000\000\000\016\000\000\000\
\016\000\016\000\016\000\016\000\013\000\013\000\000\000\000\000\
\000\000\000\000\013\000\007\000\013\000\013\000\013\000\013\000\
\007\000\000\000\007\000\007\000\007\000\007\000\003\000\000\000\
\003\000\003\000\003\000\003\000"

let yycheck = "\008\000\
\000\000\009\000\010\000\007\001\008\001\001\001\002\001\003\001\
\004\001\005\001\006\001\001\000\000\000\014\001\015\001\016\001\
\001\001\002\001\003\001\004\001\005\001\017\001\017\001\019\001\
\000\000\034\000\035\000\036\000\024\001\014\001\000\000\013\001\
\017\001\012\001\019\001\043\000\000\000\045\000\018\001\024\001\
\001\001\050\000\020\001\052\000\021\001\001\001\002\001\003\001\
\004\001\005\001\023\001\001\001\002\001\003\001\004\001\005\001\
\023\001\065\000\014\001\037\000\038\000\017\001\001\001\019\001\
\001\001\002\001\003\001\017\001\024\001\019\001\018\001\009\001\
\010\001\011\001\024\001\023\001\039\000\040\000\041\000\022\001\
\017\001\006\000\007\000\018\001\018\001\032\000\008\000\024\001\
\033\000\068\000\043\000\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\007\001\
\008\001\255\255\255\255\255\255\012\001\013\001\014\001\015\001\
\016\001\255\255\018\001\255\255\020\001\021\001\022\001\023\001\
\012\001\013\001\014\001\015\001\016\001\255\255\018\001\255\255\
\020\001\021\001\022\001\023\001\012\001\013\001\255\255\255\255\
\255\255\255\255\018\001\013\001\020\001\021\001\022\001\023\001\
\018\001\255\255\020\001\021\001\022\001\023\001\018\001\255\255\
\020\001\021\001\022\001\023\001"

let yynames_const = "\
  ABS\000\
  TILDA\000\
  NOT\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  REM\000\
  CONJ\000\
  DISJ\000\
  EQ\000\
  GT\000\
  LT\000\
  LP\000\
  RP\000\
  IF\000\
  THEN\000\
  ELSE\000\
  FI\000\
  COMMA\000\
  PROJ\000\
  EOF\000\
  DEF\000\
  DELIMITER\000\
  "

let yynames_block = "\
  INT\000\
  BOOL\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'disj) in
    Obj.repr(
# 25 "a3.mly"
           (_1)
# 249 "a3.ml"
               : A1.exptree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'conj) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'disj) in
    Obj.repr(
# 30 "a3.mly"
                  (Disjunction(_1,_3))
# 257 "a3.ml"
               : 'disj))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'conj) in
    Obj.repr(
# 31 "a3.mly"
        (_1)
# 264 "a3.ml"
               : 'disj))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compare) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'conj) in
    Obj.repr(
# 35 "a3.mly"
                     (Conjunction(_1,_3))
# 272 "a3.ml"
               : 'conj))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'nots) in
    Obj.repr(
# 36 "a3.mly"
        (_1)
# 279 "a3.ml"
               : 'conj))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'nots) in
    Obj.repr(
# 39 "a3.mly"
             ( Not(_2))
# 286 "a3.ml"
               : 'nots))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'compare) in
    Obj.repr(
# 40 "a3.mly"
             (_1)
# 293 "a3.ml"
               : 'nots))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'subs) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compare) in
    Obj.repr(
# 43 "a3.mly"
                   (Equals(_1,_3))
# 301 "a3.ml"
               : 'compare))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'subs) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compare) in
    Obj.repr(
# 44 "a3.mly"
                   (GreaterT(_1,_3))
# 309 "a3.ml"
               : 'compare))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'subs) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'compare) in
    Obj.repr(
# 45 "a3.mly"
                      (GreaterTE(_1,_4))
# 317 "a3.ml"
               : 'compare))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'subs) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'compare) in
    Obj.repr(
# 46 "a3.mly"
                      (LessTE(_1,_4))
# 325 "a3.ml"
               : 'compare))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'subs) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compare) in
    Obj.repr(
# 47 "a3.mly"
                   (LessT(_1,_3))
# 333 "a3.ml"
               : 'compare))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'subs) in
    Obj.repr(
# 48 "a3.mly"
        (_1)
# 340 "a3.ml"
               : 'compare))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'multi) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'subs) in
    Obj.repr(
# 52 "a3.mly"
                    (Sub(_1,_3))
# 348 "a3.ml"
               : 'subs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'multi) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'subs) in
    Obj.repr(
# 53 "a3.mly"
                    (Add(_1,_3))
# 356 "a3.ml"
               : 'subs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'multi) in
    Obj.repr(
# 54 "a3.mly"
         (_1)
# 363 "a3.ml"
               : 'subs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'unary) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'multi) in
    Obj.repr(
# 58 "a3.mly"
                     (Mult(_1,_3))
# 371 "a3.ml"
               : 'multi))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'unary) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'multi) in
    Obj.repr(
# 59 "a3.mly"
                    (Div(_1,_3))
# 379 "a3.ml"
               : 'multi))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'unary) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'multi) in
    Obj.repr(
# 60 "a3.mly"
                    (Rem(_1,_3))
# 387 "a3.ml"
               : 'multi))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'unary) in
    Obj.repr(
# 61 "a3.mly"
          (_1)
# 394 "a3.ml"
               : 'multi))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'unary) in
    Obj.repr(
# 65 "a3.mly"
             ( Abs(_2))
# 401 "a3.ml"
               : 'unary))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'unary) in
    Obj.repr(
# 66 "a3.mly"
               (Negative(_2))
# 408 "a3.ml"
               : 'unary))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cond) in
    Obj.repr(
# 67 "a3.mly"
        (_1)
# 415 "a3.ml"
               : 'unary))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : A1.exptree) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : A1.exptree) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : A1.exptree) in
    Obj.repr(
# 70 "a3.mly"
                                  (IfThenElse(_2,_4,_6))
# 424 "a3.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'projection) in
    Obj.repr(
# 71 "a3.mly"
               (_1)
# 431 "a3.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'projection) in
    Obj.repr(
# 75 "a3.mly"
                                       (Project((_3,_5),_7))
# 440 "a3.ml"
               : 'projection))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tupl) in
    Obj.repr(
# 76 "a3.mly"
        (_1)
# 447 "a3.ml"
               : 'projection))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'tuptemp) in
    Obj.repr(
# 80 "a3.mly"
                 ((_2))
# 454 "a3.ml"
               : 'tupl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 81 "a3.mly"
             (_1)
# 461 "a3.ml"
               : 'tupl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : A1.exptree) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tuptemp) in
    Obj.repr(
# 84 "a3.mly"
                      (match _3 with
								| Tuple(i,lis)-> Tuple(i+1,_1::lis)
							)
# 471 "a3.ml"
               : 'tuptemp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : A1.exptree) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : A1.exptree) in
    Obj.repr(
# 87 "a3.mly"
                    (Tuple(2,[_1]@[_3]))
# 479 "a3.ml"
               : 'tuptemp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 90 "a3.mly"
                                       ( Var(_1) )
# 486 "a3.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 91 "a3.mly"
                                       ( N(_1) )
# 493 "a3.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 92 "a3.mly"
        (B(_1))
# 500 "a3.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : A1.exptree) in
    Obj.repr(
# 93 "a3.mly"
              (InParen(_2))
# 507 "a3.ml"
               : 'constant))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : A1.exptree)

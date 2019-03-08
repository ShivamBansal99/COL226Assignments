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
\001\000\003\000\003\000\002\000\002\000\002\000\002\000\002\000\
\002\000\004\000\004\000\005\000\005\000\006\000\006\000\007\000\
\007\000\008\000\008\000\009\000\009\000\010\000\010\000\011\000\
\011\000\011\000\011\000\012\000\012\000\012\000\012\000\012\000\
\013\000\013\000\014\000\015\000\015\000\000\000"

let yylen = "\002\000\
\001\000\007\000\001\000\003\000\003\000\004\000\004\000\003\000\
\001\000\003\000\001\000\003\000\001\000\003\000\001\000\003\000\
\001\000\003\000\001\000\003\000\001\000\003\000\001\000\002\000\
\002\000\002\000\001\000\001\000\001\000\001\000\003\000\001\000\
\007\000\001\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\029\000\030\000\028\000\000\000\000\000\000\000\
\000\000\000\000\038\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\027\000\032\000\034\000\025\000\
\026\000\024\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\031\000\
\000\000\035\000\000\000\004\000\000\000\005\000\000\000\008\000\
\010\000\012\000\014\000\016\000\018\000\020\000\022\000\000\000\
\036\000\000\000\006\000\007\000\000\000\000\000\000\000\033\000\
\000\000"

let yydgoto = "\002\000\
\011\000\012\000\000\000\013\000\014\000\015\000\016\000\017\000\
\018\000\019\000\020\000\021\000\022\000\023\000\028\000"

let yysindex = "\009\000\
\063\255\000\000\000\000\000\000\000\000\033\255\033\255\033\255\
\063\255\241\254\000\000\000\000\006\255\254\254\003\255\009\255\
\011\255\014\255\013\255\020\255\000\000\000\000\000\000\000\000\
\000\000\000\000\247\254\015\255\030\255\063\255\002\255\039\255\
\063\255\063\255\063\255\063\255\063\255\063\255\063\255\000\000\
\063\255\000\000\016\255\000\000\063\255\000\000\063\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\023\255\
\000\000\037\255\000\000\000\000\029\255\031\255\063\255\000\000\
\023\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\012\000\061\000\055\000\049\000\
\037\000\025\000\013\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\034\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\247\255\028\000\000\000\018\000\036\000\027\000\035\000\039\000\
\034\000\038\000\000\000\021\000\000\000\016\000\033\000"

let yytablesize = 340
let yytable = "\027\000\
\023\000\029\000\003\000\004\000\005\000\006\000\007\000\008\000\
\040\000\001\000\033\000\009\000\021\000\041\000\034\000\045\000\
\035\000\036\000\009\000\030\000\031\000\032\000\037\000\038\000\
\019\000\010\000\024\000\025\000\026\000\039\000\043\000\056\000\
\042\000\003\000\004\000\005\000\017\000\061\000\058\000\003\000\
\004\000\005\000\006\000\007\000\008\000\041\000\062\000\063\000\
\015\000\009\000\049\000\037\000\047\000\065\000\013\000\009\000\
\010\000\044\000\046\000\048\000\011\000\051\000\010\000\003\000\
\004\000\005\000\006\000\007\000\008\000\050\000\052\000\054\000\
\059\000\057\000\060\000\053\000\055\000\064\000\000\000\009\000\
\000\000\000\000\000\000\000\000\000\000\000\000\010\000\000\000\
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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\023\000\
\023\000\023\000\000\000\023\000\023\000\023\000\023\000\023\000\
\023\000\000\000\023\000\021\000\021\000\021\000\000\000\023\000\
\021\000\021\000\021\000\021\000\021\000\009\000\021\000\019\000\
\019\000\000\000\009\000\021\000\019\000\019\000\019\000\019\000\
\019\000\000\000\019\000\000\000\017\000\000\000\000\000\019\000\
\017\000\017\000\017\000\017\000\017\000\000\000\017\000\000\000\
\000\000\000\000\000\000\017\000\015\000\015\000\015\000\015\000\
\015\000\000\000\015\000\013\000\013\000\013\000\013\000\015\000\
\013\000\000\000\011\000\011\000\011\000\013\000\011\000\000\000\
\000\000\000\000\000\000\011\000"

let yycheck = "\009\000\
\000\000\017\001\001\001\002\001\003\001\004\001\005\001\006\001\
\018\001\001\000\013\001\000\000\000\000\023\001\012\001\014\001\
\008\001\007\001\017\001\014\001\015\001\016\001\009\001\011\001\
\000\000\024\001\006\000\007\000\008\000\010\001\001\001\041\000\
\018\001\001\001\002\001\003\001\000\000\001\001\023\001\001\001\
\002\001\003\001\004\001\005\001\006\001\023\001\018\001\017\001\
\000\000\017\001\033\000\018\001\014\001\063\000\000\000\017\001\
\024\001\030\000\031\000\032\000\000\000\035\000\024\001\001\001\
\002\001\003\001\004\001\005\001\006\001\034\000\036\000\038\000\
\045\000\041\000\047\000\037\000\039\000\062\000\255\255\017\001\
\255\255\255\255\255\255\255\255\255\255\255\255\024\001\255\255\
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
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\007\001\
\008\001\009\001\255\255\011\001\012\001\013\001\014\001\015\001\
\016\001\255\255\018\001\007\001\008\001\009\001\255\255\023\001\
\012\001\013\001\014\001\015\001\016\001\018\001\018\001\007\001\
\008\001\255\255\023\001\023\001\012\001\013\001\014\001\015\001\
\016\001\255\255\018\001\255\255\008\001\255\255\255\255\023\001\
\012\001\013\001\014\001\015\001\016\001\255\255\018\001\255\255\
\255\255\255\255\255\255\023\001\012\001\013\001\014\001\015\001\
\016\001\255\255\018\001\013\001\014\001\015\001\016\001\023\001\
\018\001\255\255\014\001\015\001\016\001\023\001\018\001\255\255\
\255\255\255\255\255\255\023\001"

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
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'compare) in
    Obj.repr(
# 25 "a3.mly"
              (_1)
# 255 "a3.ml"
               : A1.exptree))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'compare) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'compare) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'compare) in
    Obj.repr(
# 28 "a3.mly"
                                           (IfThenElse(_2,_4,_6))
# 264 "a3.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'compare) in
    Obj.repr(
# 29 "a3.mly"
            (_1)
# 271 "a3.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'disj) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compare) in
    Obj.repr(
# 32 "a3.mly"
                   (Equals(_1,_3))
# 279 "a3.ml"
               : 'compare))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'disj) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compare) in
    Obj.repr(
# 33 "a3.mly"
                   (GreaterT(_1,_3))
# 287 "a3.ml"
               : 'compare))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'disj) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'compare) in
    Obj.repr(
# 34 "a3.mly"
                      (GreaterTE(_1,_4))
# 295 "a3.ml"
               : 'compare))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'disj) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'compare) in
    Obj.repr(
# 35 "a3.mly"
                      (LessTE(_1,_4))
# 303 "a3.ml"
               : 'compare))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'disj) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'compare) in
    Obj.repr(
# 36 "a3.mly"
                   (LessT(_1,_3))
# 311 "a3.ml"
               : 'compare))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'disj) in
    Obj.repr(
# 37 "a3.mly"
        (_1)
# 318 "a3.ml"
               : 'compare))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'conj) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'disj) in
    Obj.repr(
# 40 "a3.mly"
                  (Disjunction(_1,_3))
# 326 "a3.ml"
               : 'disj))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'conj) in
    Obj.repr(
# 41 "a3.mly"
        (_1)
# 333 "a3.ml"
               : 'disj))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'subs) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'conj) in
    Obj.repr(
# 45 "a3.mly"
                  (Conjunction(_1,_3))
# 341 "a3.ml"
               : 'conj))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'subs) in
    Obj.repr(
# 46 "a3.mly"
        (_1)
# 348 "a3.ml"
               : 'conj))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'adds) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'subs) in
    Obj.repr(
# 50 "a3.mly"
                   (Sub(_1,_3))
# 356 "a3.ml"
               : 'subs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'adds) in
    Obj.repr(
# 51 "a3.mly"
        (_1)
# 363 "a3.ml"
               : 'subs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'multi) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'adds) in
    Obj.repr(
# 55 "a3.mly"
                   (Add(_1,_3))
# 371 "a3.ml"
               : 'adds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'multi) in
    Obj.repr(
# 56 "a3.mly"
         (_1)
# 378 "a3.ml"
               : 'adds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'modu) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'multi) in
    Obj.repr(
# 60 "a3.mly"
                    (Mult(_1,_3))
# 386 "a3.ml"
               : 'multi))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'modu) in
    Obj.repr(
# 61 "a3.mly"
        (_1)
# 393 "a3.ml"
               : 'multi))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'divide) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'modu) in
    Obj.repr(
# 65 "a3.mly"
                   (Rem(_1,_3))
# 401 "a3.ml"
               : 'modu))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'divide) in
    Obj.repr(
# 66 "a3.mly"
          (_1)
# 408 "a3.ml"
               : 'modu))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'unary) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'divide) in
    Obj.repr(
# 69 "a3.mly"
                    (Div(_1,_3))
# 416 "a3.ml"
               : 'divide))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'unary) in
    Obj.repr(
# 70 "a3.mly"
         (_1)
# 423 "a3.ml"
               : 'divide))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 73 "a3.mly"
                ( Not(_2))
# 430 "a3.ml"
               : 'unary))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 74 "a3.mly"
                ( Abs(_2))
# 437 "a3.ml"
               : 'unary))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 75 "a3.mly"
                  (Negative(_2))
# 444 "a3.ml"
               : 'unary))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 76 "a3.mly"
            (_1)
# 451 "a3.ml"
               : 'unary))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 79 "a3.mly"
                                       ( Var(_1) )
# 458 "a3.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 80 "a3.mly"
                                       ( N(_1) )
# 465 "a3.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 81 "a3.mly"
        (B(_1))
# 472 "a3.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : A1.exptree) in
    Obj.repr(
# 82 "a3.mly"
              (InParen(_2))
# 479 "a3.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'projection) in
    Obj.repr(
# 83 "a3.mly"
              (_1)
# 486 "a3.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'tupl) in
    Obj.repr(
# 86 "a3.mly"
                                 (Project((_3,_5),_7))
# 495 "a3.ml"
               : 'projection))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tupl) in
    Obj.repr(
# 87 "a3.mly"
        (_1)
# 502 "a3.ml"
               : 'projection))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'tuptemp) in
    Obj.repr(
# 90 "a3.mly"
                 ((_2))
# 509 "a3.ml"
               : 'tupl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : A1.exptree) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tuptemp) in
    Obj.repr(
# 93 "a3.mly"
                      (match _3 with
								| Tuple(i,lis)-> Tuple(i+1,_1::lis)
							)
# 519 "a3.ml"
               : 'tuptemp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : A1.exptree) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : A1.exptree) in
    Obj.repr(
# 96 "a3.mly"
                    (Tuple(2,[_1]@[_3]))
# 527 "a3.ml"
               : 'tuptemp))
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

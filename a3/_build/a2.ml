# 1 "a2.mll"
 
open A3

# 6 "a2.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\227\255\228\255\229\255\230\255\079\000\000\000\000\000\
    \000\000\000\000\237\255\238\255\239\255\011\000\001\000\000\000\
    \160\000\235\000\245\255\246\255\002\000\002\000\249\255\252\255\
    \000\000\054\001\254\255\064\001\074\001\001\000\000\000\253\255\
    \004\000\000\000\002\000\248\255\003\000\232\255\017\000\004\000\
    \247\255\003\000\242\255\241\255\240\255\009\000\015\000\005\000\
    \236\255\006\000\235\255\021\000\013\000\007\000\234\255\009\000\
    \024\000\014\000\233\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\255\255\255\255\024\000\028\000\028\000\
    \028\000\028\000\255\255\255\255\255\255\028\000\028\000\028\000\
    \012\000\011\000\255\255\255\255\028\000\028\000\255\255\255\255\
    \028\000\001\000\255\255\004\000\005\000\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255";
  Lexing.lex_default =
   "\001\000\000\000\000\000\000\000\000\000\255\255\255\255\255\255\
    \255\255\255\255\000\000\000\000\000\000\255\255\255\255\255\255\
    \255\255\255\255\000\000\000\000\255\255\255\255\000\000\000\000\
    \255\255\255\255\000\000\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\000\000\255\255\000\000\255\255\255\255\
    \000\000\255\255\000\000\000\000\000\000\255\255\255\255\255\255\
    \000\000\255\255\000\000\255\255\255\255\255\255\000\000\255\255\
    \255\255\255\255\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\029\000\029\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \029\000\029\000\035\000\037\000\040\000\048\000\050\000\054\000\
    \019\000\018\000\022\000\027\000\004\000\028\000\058\000\014\000\
    \026\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\044\000\003\000\010\000\012\000\011\000\000\000\
    \000\000\005\000\005\000\005\000\005\000\005\000\016\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\017\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\000\000\013\000\043\000\000\000\000\000\
    \000\000\024\000\030\000\000\000\021\000\006\000\049\000\032\000\
    \051\000\008\000\036\000\033\000\055\000\020\000\015\000\041\000\
    \009\000\038\000\045\000\031\000\007\000\039\000\034\000\042\000\
    \046\000\047\000\052\000\053\000\056\000\057\000\023\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\000\000\000\000\000\000\000\000\000\000\
    \002\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \026\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\026\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\000\000\000\000\000\000\000\000\
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
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\029\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\029\000\034\000\036\000\039\000\047\000\049\000\053\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\057\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\013\000\000\000\000\000\000\000\000\000\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\000\000\014\000\255\255\255\255\
    \255\255\000\000\024\000\255\255\000\000\000\000\008\000\021\000\
    \007\000\000\000\032\000\021\000\006\000\000\000\000\000\015\000\
    \000\000\020\000\009\000\030\000\000\000\038\000\033\000\041\000\
    \045\000\046\000\051\000\052\000\055\000\056\000\000\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\255\255\255\255\255\255\255\255\255\255\
    \000\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\255\255\255\255\255\255\255\255\
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
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec read lexbuf =
   __ocaml_lex_read_rec lexbuf 0
and __ocaml_lex_read_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 14 "a2.mll"
             ( read lexbuf )
# 206 "a2.ml"

  | 1 ->
let
# 15 "a2.mll"
             n
# 212 "a2.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 15 "a2.mll"
               ((INT (int_of_string(n))) )
# 216 "a2.ml"

  | 2 ->
# 16 "a2.mll"
                  (ABS )
# 221 "a2.ml"

  | 3 ->
# 17 "a2.mll"
         (TILDA)
# 226 "a2.ml"

  | 4 ->
# 18 "a2.mll"
                     (PLUS )
# 231 "a2.ml"

  | 5 ->
# 19 "a2.mll"
                     (MINUS )
# 236 "a2.ml"

  | 6 ->
# 20 "a2.mll"
                     (TIMES )
# 241 "a2.ml"

  | 7 ->
# 21 "a2.mll"
                        (DIV )
# 246 "a2.ml"

  | 8 ->
# 22 "a2.mll"
                        (REM )
# 251 "a2.ml"

  | 9 ->
# 23 "a2.mll"
                     (LP )
# 256 "a2.ml"

  | 10 ->
# 24 "a2.mll"
                     (RP )
# 261 "a2.ml"

  | 11 ->
# 25 "a2.mll"
                     (BOOL(true) )
# 266 "a2.ml"

  | 12 ->
# 26 "a2.mll"
                     (BOOL(false) )
# 271 "a2.ml"

  | 13 ->
# 27 "a2.mll"
                       (NOT )
# 276 "a2.ml"

  | 14 ->
# 28 "a2.mll"
                       (CONJ )
# 281 "a2.ml"

  | 15 ->
# 29 "a2.mll"
                       (DISJ )
# 286 "a2.ml"

  | 16 ->
# 30 "a2.mll"
                     (EQ )
# 291 "a2.ml"

  | 17 ->
# 31 "a2.mll"
                     (GT )
# 296 "a2.ml"

  | 18 ->
# 32 "a2.mll"
                     (LT )
# 301 "a2.ml"

  | 19 ->
# 33 "a2.mll"
            (PROJ)
# 306 "a2.ml"

  | 20 ->
# 34 "a2.mll"
                       (IF )
# 311 "a2.ml"

  | 21 ->
# 35 "a2.mll"
                         (THEN )
# 316 "a2.ml"

  | 22 ->
# 36 "a2.mll"
                         (ELSE )
# 321 "a2.ml"

  | 23 ->
# 37 "a2.mll"
                        (DEF )
# 326 "a2.ml"

  | 24 ->
let
# 38 "a2.mll"
                 a
# 332 "a2.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 38 "a2.mll"
                            ((ID (a))  )
# 336 "a2.ml"

  | 25 ->
# 39 "a2.mll"
          (COMMA)
# 341 "a2.ml"

  | 26 ->
# 40 "a2.mll"
                       (DELIMITER )
# 346 "a2.ml"

  | 27 ->
# 41 "a2.mll"
                      (EOF)
# 351 "a2.ml"

  | 28 ->
# 42 "a2.mll"
                       (failwith "Illegal character")
# 356 "a2.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_rec lexbuf __ocaml_lex_state

;;

# 44 "a2.mll"
 
  let scanner s = read (Lexing.from_string s)

# 367 "a2.ml"

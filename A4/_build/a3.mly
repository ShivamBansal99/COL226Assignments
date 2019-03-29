%{
    open A1
%}

/*
- Tokens (token name and rules) are modified wrt to A2. Please make necessary changes in A3
- LP and RP are left and right parenthesis
- Write grammar rules to recognize
  - >= <= from GT EQ LT tokens
  - if then else fi
*/
/* Tokens are defined below.  */
%token <int> INT
%token <bool> BOOL
%token <string> ID
%token ABS TILDA NOT PLUS MINUS TIMES DIV REM CONJ DISJ EQ GT LT LP RP IF THEN ELSE FI COMMA PROJ
LET IN END BACKSLASH DOT DEF SEMICOLON PARALLEL LOCAL EOF
%start def_parser exp_parser
%type <A1.definition> def_parser /* Returns definitions */
%type <A1.exptree> exp_parser /* Returns expression */
%%
/*
DESIGN a grammar for a simple expression language, taking care to enforce precedence rules (e.g., BODMAS)
The language should contain the following types of expressions:  integers and booleans.
*/

exp_parser: disj EOF	{$1}
;

disj:
	| disj DISJ conj	{Disjunction($1,$3)}
	| conj	{$1}
;

conj:
	| conj CONJ compare	{Conjunction($1,$3)}
	| nots	{$1}
;
nots:
  | NOT nots	{ Not($2)}
  | compare  {$1}
;
compare:
	| compare EQ subs {Equals($1,$3)}
	| compare GT subs {GreaterT($1,$3)}
	| compare GT EQ subs {GreaterTE($1,$4)}
	| compare LT EQ subs {LessTE($1,$4)}
	| compare LT subs {LessT($1,$3)}
	| subs {$1}
;

subs:
	| subs MINUS multi	{Sub($1,$3)}
  | subs PLUS multi	{Add($1,$3)}
	| multi	{$1}
;

multi:
	| multi TIMES unary	{Mult($1,$3)}
  | multi DIV unary	{Div($1,$3)}
  | multi REM unary	{Rem($1,$3)}
  | unary	{$1}
;

unary:
	| ABS unary	{ Abs($2)}
	| TILDA unary {Negative($2)}
	| cond	{$1}
;
cond:
	| IF disj THEN disj ELSE disj FI {IfThenElse($2,$4,$6)}
	| projection  {$1}
;

projection:
	| PROJ LP INT COMMA INT RP projection	{Project(($3,$5),$7)}
	| tupl	{$1}

;
tupl:
	| LP tuptemp RP {($2)}
  | funccall {$1}
;
tuptemp:
	| disj COMMA tuptemp	{match $3 with
								| Tuple(i,lis)-> Tuple(i+1,$1::lis)
							}
	| disj COMMA disj		{Tuple(2,[$1]@[$3])}
;
funccall:
| funcabs LP funccall RP {FunctionCall($1,$3)}
| funcabs {$1}
;
funcabs:
| BACKSLASH ID DOT funcabs {FunctionAbstraction($2,$4)}
| lets  {$1}
;
lets:
| LET defseq IN disj END {Let($2,$4)}
| constant {$1}
;

constant:
    ID                                 { Var($1) }      /* To be interpreted as a variable name with string as tokenised */
    | INT                              { N($1) }      /* To be interpreted as an integer with its value as tokenised   */
	| BOOL	{B($1)}
	| LP disj RP	{InParen($2)}
  | LP RP {Tuple(0,[])}
;

def_parser:
  | defseq EOF  {$1}
;
defseq:
  | defseq SEMICOLON defs {match $1 with
                            | Sequence(x) -> Sequence(x@[$3])
                            | x -> Sequence([x]@[$3])}
  | defseq PARALLEL defs {match $1 with
                            | Parallel(x) -> Parallel(x@[$3])
                            | x -> Parallel([x]@[$3])}

  | defs                    {$1}
;
defs:
  | DEF ID EQ disj {Simple($2,$4)}
  | LOCAL defseq IN defseq END  {Local($2,$4)}
;

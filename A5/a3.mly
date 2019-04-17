%{
    open A5
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
LET IN END BACKSLASH DOT DEF SEMICOLON PARALLEL LOCAL EOF TUNIT TINT TBOOL TFUNC TTUPLE COLON REC CMP
%start exp_parser
%type <A5.expr> exp_parser /* Returns expression */
%%
/*
DESIGN a grammar for a simple expression language, taking care to enforce precedence rules (e.g., BODMAS)
The language should contain the following types of expressions:  integers and booleans.
*/

exp_parser: disj EOF	{$1}
;

disj:
	| disj DISJ conj	{Or($1,$3)}
	| conj	{$1}
;

conj:
	| conj CONJ compare	{And($1,$3)}
	| compare	{$1}
;

compare:
 | CMP compare {Cmp($2)}
 | subs {$1}
;

subs:
	| subs MINUS multi	{Minus($1,$3)}
  | subs PLUS multi	{Plus($1,$3)}
	| multi	{$1}
;

multi:
	| multi TIMES cond	{Mult($1,$3)}
  | cond	{$1}
;
cond:
	| IF disj THEN disj ELSE disj FI {If_Then_Else($2,$4,$6)}
	| funccall  {$1}
;
funccall:
| funcabs funccall {App($1,$2)}
| funcabs {$1}
;
funcabs:
| REC ID DOT BACKSLASH ID DOT funcabs {Rec(V $2,V $5,$7)}
| BACKSLASH ID DOT funcabs {Lambda(V $2, $4)}
| constant  {$1}
;

constant:
    ID                                 { V($1) }      /* To be interpreted as a variable name with string as tokenised */
    | INT                              { Integer($1) }      /* To be interpreted as an integer with its value as tokenised   */
	| BOOL	{Bool($1)}
	| LP disj RP	{($2)}
;

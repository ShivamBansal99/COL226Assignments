%{
    open A1
%}

/*
- Tokens (token name and rules) are modified wrt to A2. Please make necessary changes in A3
- LP and RP are left and right parenthesis
- Write grammar rules to recognize
  - >= <= from GT EQ LT tokens
  - if then else fi
Tokens for tuple will be added shortly
*/
%token <int> INT
%token <bool> BOOL
%token <string> ID
%token ABS TILDA NOT PLUS MINUS TIMES DIV REM CONJ DISJ EQ GT LT LP RP IF THEN ELSE FI COMMA PROJ EOF DEF DELIMITER
%start main
%type <A1.exptree> main /* Return type */
%%
/*
DESIGN a grammar for a simple expression language, taking care to enforce precedence rules (e.g., BODMAS)
The language should contain the following types of expressions:  integers and booleans.
*/

main: disj	{$1}
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
	| IF main THEN main ELSE main FI {IfThenElse($2,$4,$6)}
	| projection  {$1}
;

projection:
	| PROJ LP INT COMMA INT RP projection	{Project(($3,$5),$7)}
	| tupl	{$1}

;
tupl:
	| LP tuptemp RP {($2)}
  | constant {$1}
;
tuptemp:
	| tuptemp COMMA main	{match $3 with
								| Tuple(i,lis)-> Tuple(i+1,$1::lis)
							}
	| main COMMA main		{Tuple(2,[$1]@[$3])}
;
constant:
    ID                                 { Var($1) }      /* To be interpreted as a variable name with string as tokenised */
    | INT                              { N($1) }      /* To be interpreted as an integer with its value as tokenised   */
	| BOOL	{B($1)}
	| LP main RP	{InParen($2)}
;

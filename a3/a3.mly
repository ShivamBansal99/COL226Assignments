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

main: compare	{$1}
;

compare:
	| disj EQ compare {Equals($1,$3)}
	| disj GT compare {GreaterT($1,$3)}
	| disj GT EQ compare {GreaterTE($1,$4)}
	| disj LT EQ compare {LessTE($1,$4)}
	| disj LT compare {LessT($1,$3)}
	| disj {$1}
;
disj:
	| conj DISJ disj	{Disjunction($1,$3)}
	| conj	{$1}
;

conj:
	| subs CONJ conj	{Conjunction($1,$3)}
	| subs	{$1}
;

subs:
	| adds MINUS subs	{Sub($1,$3)}
	| adds	{$1}
;

adds:
	| multi PLUS adds	{Add($1,$3)}
	| multi	{$1}
;

multi:
	| modu TIMES multi	{Mult($1,$3)}
	| modu	{$1}
;

modu:
	| divide REM modu	{Rem($1,$3)}
	| divide	{$1}
;
divide:
	| unary DIV divide	{Div($1,$3)}
	| unary	{$1}
;
unary:
	| NOT unary	{ Not($2)}
	| ABS unary	{ Abs($2)}
	| TILDA unary {Negative($2)}
	| cond	{$1}
;
cond:
	| IF cond THEN cond ELSE cond FI {IfThenElse($2,$4,$6)}
	| constant  {$1}
;
constant:
    ID                                 { Var($1) }      /* To be interpreted as a variable name with string as tokenised */
    | INT                              { N($1) }      /* To be interpreted as an integer with its value as tokenised   */
	| BOOL	{B($1)}
	| LP main RP	{InParen($2)}
	| projection	{$1}
;
projection:
	| PROJ LP INT COMMA INT RP tupl	{Project(($3,$5),$7)}
	| tupl	{$1}
;
tupl:
	| LP tuptemp RP {($2)}
;
tuptemp:
	| main COMMA tuptemp	{match $3 with
								| Tuple(i,lis)-> Tuple(i+1,$1::lis)
							}
	| main COMMA main		{Tuple(2,[$1]@[$3])}
;

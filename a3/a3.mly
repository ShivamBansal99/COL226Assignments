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
%token ABS TILDA NOT PLUS MINUS TIMES DIV REM CONJ DISJ EQ GT LT LP RP IF THEN ELSE FI COMMA PROJ EOF
%start main
%type <A1.exptree> main /* Return type */
%%
/*
DESIGN a grammar for a simple expression language, taking care to enforce precedence rules (e.g., BODMAS)
The language should contain the following types of expressions:  integers and booleans.
*/

main:
  |
  | EOF { Done }
;
main: compare	{$1}
;
cond: 
	| IF compare THEN compare ELSE compare FI {IfThenElse($2,$4.$6)}
	| compare
;
compare:
	| disj EQ compare {Equals($1,$3)}
	| disj GT compare {GreaterT($1,$3)}
	| disj GT EQ compare {GreaterTE($1,$3)}
	| disj LT EQ compare {LessTE($1,$3)}
	| disj LT compare {LessT($1,$3)}
	| disj {$1}
;
disj:
	| conj OR disj	{disjunction($1,$3)}
	| conj	{$1}
;

conj:
	| subs AND conj	{Conjunction($1,$3)}
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
	| modu MUL multi	{Mult($1,$3)}
	| modu	{$1}
;

modu:
	| divide MOD modu	{Rem($1,$3)}
	| divide	{$1}
;
divide:
	| unary DIV divide	{Div($1,$3)}
	| unary	{$1}
;
unary:
	| NOT constant	{ Not($2)}
	| ABS constant	{ Abs($2)}
	| TILDA constant {Negative($2)}
	| constant	{$1}
;
constant:
    ID                                 { Var($1) }      /* To be interpreted as a variable name with string as tokenised */
    | INT                              { N($1) }      /* To be interpreted as an integer with its value as tokenised   */
	| BOOL	{B($1)}
	| LP main RP	{InParen($2)}
	| projection	{$1}
;
projection:
	| PROJ tupl tupl	{match $2 with
							| (2,i::[j]) -> Proj((i,j),$3)
						}
	| tupl	{$1}
;
tupl:
	| LP tuptemp RP {Tup($2)}
;	
tuptemp:
	| main COMMA tuptemp	{match $3 with 
								| (i,lis)-> (i+1,$1::lis)
							}
	| main COMMA main		{(2,[$1]@[$3])}
;

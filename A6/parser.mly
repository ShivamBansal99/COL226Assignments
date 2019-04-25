/* File parser.mly */
%{
    open Expression
%}

%token COLON EQUAL LP RP COMMA CALL EOL EOF RET
%token <int> INT
%token <string> ID
%start main             /* the entry point */
%type <Expression.expr_tree> main                     /* Specifying the type to be returned for the grammar symbol main */
%%
main:
    call_expression EOL                 { $1 }          /* $n on the rhs returns the value for nth symbol in the grammar on lhs */
    | EOF                              { NULL }
;
call_expression:
    CALL call_expression LP list_exp RP { CALL($2,$4) } /* Created a tree with PLUS at root and two subtrees corresponding to left: add_expression and right: mult_expression */
    | CALL call_expression LP RP { CALL($2,[]) }
    | ass_expression                  { $1 }
;
ass_expression:
    ass_expression COLON EQUAL constant      { ASS($1,$4) }
    | constant                         { $1 }
;
constant:
    ID                                 { VAR($1) }      /* To be interpreted as a variable name with string as tokenised */
    | INT                              { NUM($1) }      /* To be interpreted as an integer with its value as tokenised   */
    | RET {Ret}
;
list_exp:
| INT COMMA list_exp {$1::$3}
| INT COMMA INT {$1::[$3]}
;
/*TODO
 * Add support in the grammar for parenthesis
 *  - Adding the parenthesis should be able to change the parse tree to effectively modify precedence.
 *  E.g. 1+2*3  ==>        PLUS
 *                        /    \
 *                      NUM1   INTO
 *                            /    \
 *                         NUM 2  NUM 3
 *
 *  vs (1+2)*3  ==>        INTO
 *                        /    \
 *                     PLUS     NUM 3
 *                    /    \
 *                 NUM 1   NUM 2
 *
 * Try completing the calculator for basic arithmetic by adding division and subtraction, while respecting precedence
 * This will require changes right from the lexer.mll and parser.mly to the definition of print and evaluation functions in expression.ml
 *
 * ADVANCED
 * Try creating an expression for assigning new variables in the variable_set in the expression.ml file, so that they can be reused in a later evaluation statement.
 * E.g. myVar:=4.
 *      // Stores the integer value 4 corresponding to the string myVar in variable_set
 *
 *      myVar*3+1
 *      Answer: 13
 * */

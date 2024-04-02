%{
    open Ast

    (*Le parseur doit générer des Ast.program.
    Pour les annotation, il vous suffit de créer l’annotation avec [Annotation.create $loc] qui sera ce qu’il vous faut dans tous les cas : $loc est un paramètre qui correspond aux positions délimitant la chaîne de caractères parsée par la règle où il est utilisé, ce qui est exactement ce qu’il faut mettre dans l’annotation.*)
%}

%token AND
%token BLUE
%token BOOL
%token COLOR
%token COORD
%token COS
%token DRAW
%token ELSE
%token FALSE
%token FLOOR
%token FOR
%token FOREACH
%token FORE
%token FROM
%token GREEN
%token HEAD
%token IF
%token IN
%token INT_TYP
%token LIST
%token NOT
%token OR
%token PIXEL
%token PRINT
%token REAL_TYP
%token REAL_OF_INT
%token RED
%token SET
%token SIN
%token STEP
%token TAIL
%token TO
%token TRUE

%token X Y
%token PI

%token BLOCKSTART
%token BLOCKEND
%token PLUS
%token MINUS
%token TIMES
%token DIV
%token REM
%token EQ
%token DIFF
%token LEQ
%token GEQ
%token LT
%token GT
%token COLON
%token DOUBLECOLON
%token DOT
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token COMMA
%token SEMICOLON

%token EOF
%token <int> INT
%token <float> REAL
%token <string> ID


%start <program> main
%%

main:
| EOF { Program([],Block([],Annotation.create $loc)) }

type_expression:
| {}

binary_operator:
| {}

unary_operator:
| {}

field_accessor:
| {}

expression:
| {}

statement_list:
| {[]}
| statement = statement statement_list = statement_list { [statement] @ statement_list }

statement:
| SET LPAREN expr_1 = expression COMMA expr_2 = expression RPAREN { Affectation(expr_1, expr_2, Annotation.create $loc) }
| type_e = type_expression COLON var_name = ID { Declaration(var_name, type_e, Annotation.create $loc) }
| BLOCKSTART statement_list = statement_list BLOCKEND { Block(statement_list, Annotation.create $loc) }
(*Check priorities for ifthenelse*)
| IF LPAREN expr = expression RPAREN statement_1 = statement ELSE statement_2 = statement { IfThenElse(expr, statement_1, statement_2, Annotation.create $loc) }
| IF LPAREN expr = expression RPAREN statement = statement { IfThenElse(expr, statement, Nop, Annotation.create $loc) }
(*End of check*)
| FOR var_name = ID FROM expr_1 = expression TO expr_2 = expression STEP expr_3 = expression statement = statement { For(var_name, expr_1, expr_2, expr_3, statement, Annotation.create $loc) }
(*TO CONTINUE*)

argument:
| type_e = type_expression COLON var_name = ID { Argument(var_name, type_e, Annotation.create $loc) }

argument_list:
| {[]}
| argument = argument argument_list = argument_list { [argument] @ argument_list }

program:
| LT argument_list = argument_list GT statement = statement { Program(argument_list, statement) }
| statement = statement { Program([], statement) }
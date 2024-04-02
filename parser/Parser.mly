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
%token SUB
%token MUL
%token DIV
%token MOD
%token EQ
%token NEQ
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
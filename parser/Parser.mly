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
%token WHILE
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
%token MAP
%token POW2

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



%nonassoc OR
%nonassoc AND
%nonassoc EQ DIFF LEQ GEQ LT GT
%left PLUS MINUS 
%left TIMES DIV REM

%right DOT
%right DOUBLECOLON

%nonassoc IFTHEN
%nonassoc ELSE

%nonassoc BINOP
%nonassoc UNOP


%start <program> main
%%

main:
| program = program EOF { program }

type_expression:
| INT_TYP { Type_int }
| REAL_TYP { Type_real }
| BOOL { Type_bool }
| COORD { Type_coord }
| COLOR { Type_color }
| PIXEL { Type_pixel }
| LIST LPAREN type_e = type_expression RPAREN { Type_list(type_e) }

field_accessor:
| COLOR { Color_field }
| COORD { Coord_field }
| X { X_field }
| Y { Y_field }
| RED { Red_field }
| GREEN { Green_field }
| BLUE { Blue_field }

unary_operator:
| MINUS { Opposite }
| NOT { Not }
| HEAD { Head }
| TAIL { Tail }
| FLOOR { Floor }
| REAL_OF_INT { Real_of_int }
| COS { Cos }
| SIN { Sin }

binary_operator:
| PLUS { Plus }
| MINUS { Minus }
| TIMES { Times }
| DIV { Div }
| REM { Rem }
| AND { And }
| OR { Or }
| EQ { Equal }
| DIFF { Diff }
| LT { Lt }
| GT { Gt }
| LEQ { Leq }
| GEQ { Geq }

expression_list:
| {[]}
| expr = expression { [expr] }
| expr = expression COMMA expr_list = expression_list { [expr] @ expr_list }

expression:
| var_int = INT { Const_int(var_int, Annotation.create $loc) }
| var_real = REAL { Const_real(var_real, Annotation.create $loc) }
| PI { Const_real(Float.pi, Annotation.create $loc) }
| TRUE { Const_bool(true, Annotation.create $loc) }
| FALSE { Const_bool(false, Annotation.create $loc) }
| var_name = ID { Variable(var_name, Annotation.create $loc) }
| COORD LPAREN expr_1 = expression COMMA expr_2 = expression RPAREN { Coord(expr_1, expr_2, Annotation.create $loc) }
| COLOR LPAREN expr_1 = expression COMMA expr_2 = expression COMMA expr_3 = expression RPAREN { Color(expr_1, expr_2, expr_3, Annotation.create $loc) }
| PIXEL LPAREN expr_1 = expression COMMA expr_2 = expression RPAREN { Pixel(expr_1, expr_2, Annotation.create $loc) }
| expr_1 = expression binop = binary_operator expr_2 = expression %prec BINOP { Binary_operator(binop, expr_1, expr_2, Annotation.create $loc) }
| unaryop = unary_operator expr = expression %prec UNOP { Unary_operator(unaryop, expr, Annotation.create $loc) }
| expr = expression DOT field = field_accessor { Field_accessor(field, expr, Annotation.create $loc) }
| LBRACKET expr_list = expression_list RBRACKET { List(expr_list, Annotation.create $loc) }
| expr_1 = expression DOUBLECOLON expr_2 = expression { Append(expr_1, expr_2, Annotation.create $loc) }
| LPAREN expr = expression RPAREN { expr }
| POW2 LPAREN expr = expression RPAREN { Binary_operator(Times, expr, expr, Annotation.create $loc) }

statement_list:
| statement = statement { [statement] }
| statement = statement SEMICOLON statement_list = statement_list { [statement] @ statement_list }

statement:
| SET LPAREN expr_1 = expression COMMA expr_2 = expression RPAREN { Affectation(expr_1, expr_2, Annotation.create $loc) }
| type_e = type_expression COLON var_name = ID { Declaration(var_name, type_e, Annotation.create $loc) }
| BLOCKSTART statement_list = statement_list BLOCKEND { Block(statement_list, Annotation.create $loc) }
| IF LPAREN expr = expression RPAREN statement = statement %prec IFTHEN { IfThenElse(expr, statement, Nop, Annotation.create $loc) }
| IF LPAREN expr = expression RPAREN statement_1 = statement ELSE statement_2 = statement { IfThenElse(expr, statement_1, statement_2, Annotation.create $loc) }
| FOR var_name = ID FROM expr_1 = expression TO expr_2 = expression STEP expr_3 = expression statement = statement { For(var_name, expr_1, expr_2, expr_3, statement, Annotation.create $loc) }
| FOREACH var_name = ID IN expr = expression statement = statement { Foreach(var_name, expr, statement, Annotation.create $loc) } 
| WHILE LPAREN expr = expression RPAREN statement = statement { While(expr, statement, Annotation.create $loc) }
| DRAW LPAREN expr = expression RPAREN { Draw_pixel(expr, Annotation.create $loc) }
| PRINT LPAREN expr = expression RPAREN { Print(expr, Annotation.create $loc) }
| MAP LPAREN list_name = expression operation = binary_operator value = expression RPAREN { Foreach("mapping_variable", list_name, Block([Affectation(Variable("mapping_variable", Annotation.create $loc), Binary_operator(operation, Variable("mapping_variable", Annotation.create $loc), value, Annotation.create $loc), Annotation.create $loc)], Annotation.create $loc), Annotation.create $loc ) }
| var_name = ID PLUS PLUS { Affectation(Variable(var_name, Annotation.create $loc), Binary_operator(Plus, Variable(var_name, Annotation.create $loc), Const_int(1, Annotation.create $loc), Annotation.create $loc), Annotation.create $loc) }
| var_name = ID MINUS MINUS { Affectation(Variable(var_name, Annotation.create $loc), Binary_operator(Minus, Variable(var_name, Annotation.create $loc), Const_int(1, Annotation.create $loc), Annotation.create $loc), Annotation.create $loc) }
| { Nop }

argument:
| type_e = type_expression COLON var_name = ID { Argument(var_name, type_e, Annotation.create $loc) }

argument_list:
| {[]}
| argument = argument { [argument] }
| argument = argument SEMICOLON argument_list = argument_list { [argument] @ argument_list }

program:
| LT argument_list = argument_list GT statement = statement { Program(argument_list, statement) }
| statement = statement { Program([], statement) }
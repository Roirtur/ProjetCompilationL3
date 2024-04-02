
{
    open Parser
    exception Error of string
}

let digit = ['0'-'9']
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9' '\'' '_']

rule token = parse

    (* Keywords *)
    | "And"             { AND }
    | "Blue"            { BLUE }
    | "Bool"            { BOOL }
    | "Color"           { COLOR }
    | "Coord"           { COORD }
    | "Cos"             { COS }
    | "Draw"            { DRAW }
    | "Else"            { ELSE }
    | "False"           { FALSE }
    | "Floor"           { FLOOR }
    | "For"             { FOR }
    | "Foreach"         { FOREACH }
    | "From"            { FROM }
    | "Green"           { GREEN }
    | "Head"            { HEAD }
    | "If"              { IF }
    | "In"              { IN }
    | "Int"             { INT_TYP } 
    | "List"            { LIST }
    | "Not"             { NOT }
    | "Or"              { OR }
    | "Pixel"           { PIXEL }
    | "Print"           { PRINT }
    | "Real"            { REAL_TYP }
    | "Real_of_int"     { REAL_OF_INT }
    | "Red"             { RED }
    | "Set"             { SET }
    | "Sin"             { SIN }
    | "Step"            { STEP }
    | "Tail"            { TAIL }
    | "To"              { TO }
    | "True"            { TRUE }
    | "X"               { X }
    | "Y"               { Y }
    | "Pi"              { PI }

    (* Symbols *)
    | "$<"              { BLOCKSTART }
    | ">$"              { BLOCKEND }
    | "+"               { PLUS }
    | "-"               { SUB }
    | "*"               { MUL }
    | "/"               { DIV }
    | "%"               { MOD }
    | "="               { EQ }
    | "<>"              { NEQ }
    | "<="              { LEQ }
    | ">="              { GEQ }
    | "<"               { LT }
    | ">"               { GT }
    | ":"               { COLON }
    | "::"              { DOUBLECOLON }
    | "."               { DOT }
    | "("               { LPAREN }
    | ")"               { RPAREN }
    | "["               { LBRACKET }
    | "]"               { RBRACKET }
    | ","               { COMMA }
    | ";"               { SEMICOLON }

    (* Spaces *)
    | [' ' '\t' '\r' '\n']   {token lexbuf}

    (* Commentaries *)
    | "//" [^ '\n']* '\n' {Lexing.new_line lexbuf; token lexbuf}
    | "/*" {commentary lexbuf}

    (* Identifiers *)
    | ['a'-'z'] (alphanum)* as s  { ID(s) }

    (* Integers *)
    | (digit)+ as s     { INT(int_of_string s) }
    | "0x" (['0'-'9' 'A'-'F'])+ as s { INT(int_of_string s) }

    (* Floats *)
    | (digit)* "." (digit)* as s { REAL(float_of_string s) }
    
    | eof               { EOF }
    | _ as s            { let pos = Lexing.lexeme_start_p lexbuf in raise (Error(Format.sprintf "Line %d, char %d ,Read: '%c'. It is not an acceptable character" pos.pos_lnum (pos.pos_cnum - pos.pos_bol +1) s)) }

and commentary = parse
    | '\n'      {Lexing.new_line lexbuf; commentary lexbuf}
    | "*/"      { token lexbuf }
    | _ { commentary lexbuf }
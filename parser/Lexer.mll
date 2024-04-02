
{
    open Parser
    exception Error of string
}

let digit = ['0'-'9']
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9' '\'' '_']

(* Token names : And, Blue, Bool, Color, Coord, Cos, Draw, Else, False, Floor, For, Foreach, From,
Green, Head, If, In, Int, List, Not, Or, Pixel, Print, Real, Real_of_int, Red, Set,
Sin, Step, Tail, To, True, X, Y, Pi*)

(*To recognize: $< >$ + - * / % = <> <= >= < > : :: . ( ) [ ] , ;*)

rule token = parse


    (* Spaces *)
    | [' ' '\t' '\r' '\n']   {token lexbuf}

    (* Commentaries *)
    | "//" [^ '\n']* '\n' {Lexing.new_line lexbuf; token lexbuf}
    | "/*" [^ "*/"]* "*/" {commentary lexbuf}

    (* Identifiers *)
    | ['a'-'z'] (alphanum)* as s  { ID(s) }

    (* Integers *)
    | (digit)+ as s     { INT(int_of_string s) }
    | "0x" (['0'-'9' 'A'-'F'])+ as s { INT(int_of_string s) }

    (* Floats *)
    | (digit)* "." (digit)* as s { REAL(float_of_string s) }
    
    | eof               { EOF }
    | _ as s            { let pos = Lexing.lexeme_start_p lexbuf in raise (Error(Format.sprintf "Line %d, char %d ,Read: '%c'. It is not an acceptable character" pos.pos_lnum (pos.pos_cnum - pos.pos_bol +1) s)) }
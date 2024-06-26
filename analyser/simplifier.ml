(* Codez ici le simplificateur de termes.

    Tout comme pour le langage du cours, l’idée consiste à remplacer les termes constants par le résultat de leur calcul.

    Faites une sous-fonctions récursive pour les expressions et les statements.
    Ces fonction font un pattern matching sur leur argument et traitent chaque cas séparément. Elles renvoient un argument de même type que celui reçu.
    Par exemple : simplify_expression : Ast.expression -> Ast.expression

    Les cas minimaux attendus sont les cas sur les entiers, les flottants, les booléens, ainsi que les if dont le test est constant, et les for qui ne s’exécutent jamais.

    Deux points (entre autres) qui peuvent vous permettre d’aller plus loin :
      - les expressions ne peuvent pas faire d’effet de bord ici, ce qui permet de simplifier des expressions pas nécessairement constantes.
      - Les types composés (pixels, coordonnées et couleurs) peuvent également être simplifiés (e.g., (1,2) + (2,x) peut être simplifié en (3,2+x)).

    Vous détaillerez dans le rapport les différents cas que vous simplifiez dans votre simplificateur.
*)

open Ast

(* Gets type of an expression and return the corresponding annotation *)
let get_type_of_expression expr = 
    match expr with
    | Const_int (_, anno) 
    | Const_real (_, anno)
    | Const_bool (_, anno)
    | Variable (_, anno)
    | Coord (_, _, anno)
    | Color (_, _, _, anno)
    | Pixel (_, _, anno)
    | Binary_operator (_, _, _, anno)
    | Unary_operator (_, _, anno)
    | Field_accessor (_, _, anno)
    | List (_, anno)
    | Append (_, _, anno) -> match (Ast.Annotation.get_type anno) with
        | Some otype -> otype
        | None -> Type_generic
    
(* Simplify an expression *)

let rec simplify_expression expr =
    match expr with

    | Const_int _ -> expr
    | Const_real _ -> expr
    | Const_bool _ -> expr
    | Variable _ -> expr

    | Coord (x, y, anno) ->
        Coord (simplify_expression x, simplify_expression y, anno)

    | Color (r, g, b, anno) ->
        Color (simplify_expression r, simplify_expression g, simplify_expression b, anno)

    | Pixel (p, c, anno) ->
        Pixel (simplify_expression p, simplify_expression c, anno)

    | Binary_operator (op, e1, e2, anno) ->
        (match (simplify_expression e1, simplify_expression e2) with
        | (Const_int (i1, _), Const_int (i2, _)) ->
            (match op with
            | Plus -> Const_int ((i1 + i2), anno)
            | Minus -> Const_int ((i1 - i2), anno)
            | Times -> Const_int ((i1 * i2), anno)
            | Div -> Const_int ((i1 / i2), anno)
            | Rem -> Const_int ((i1 mod i2), anno)
            | Equal -> Const_bool (i1 = i2, anno)
            | Diff -> Const_bool (i1 != i2, anno)
            | Lt -> Const_bool (i1 < i2, anno)
            | Gt -> Const_bool (i1 > i2, anno)
            | Leq -> Const_bool (i1 <= i2, anno)
            | Geq -> Const_bool (i1 >= i2, anno)
            | _ -> Binary_operator (op, simplify_expression e1, simplify_expression e2, anno))

        | (Const_real (r1, _), Const_real (r2, _)) ->
            (match op with
            | Plus -> Const_real ((r1 +. r2), anno)
            | Minus -> Const_real ((r1 -. r2), anno)
            | Times -> Const_real ((r1 *. r2), anno)
            | Div -> Const_real ((r1 /. r2), anno)
            | Rem -> Const_real (mod_float r1 r2, anno)
            | Equal -> Const_bool (r1 = r2, anno)
            | Diff -> Const_bool (r1 != r2, anno)
            | Lt -> Const_bool (r1 < r2, anno)
            | Gt -> Const_bool (r1 > r2, anno)
            | Leq -> Const_bool (r1 <= r2, anno)
            | Geq -> Const_bool (r1 >= r2, anno)
            | _ -> Binary_operator (op, simplify_expression e1, simplify_expression e2, anno))

        | (Const_bool (b1, _), Const_bool (b2, _)) ->
            (match op with
            | And -> Const_bool (b1 && b2, anno)
            | Or -> Const_bool (b1 || b2, anno)
            | Equal -> Const_bool (b1 = b2, anno)
            | Diff -> Const_bool (b1 != b2, anno)
            | _ -> Binary_operator (op, simplify_expression e1, simplify_expression e2, anno))
        
        | (Coord (x1, y1, _), Coord (x2, y2, _)) ->
            (match op with
            | Plus -> Coord (Binary_operator (Plus, x1, x2, anno), Binary_operator (Plus, y1, y2, anno), anno)
            | Minus -> Coord (Binary_operator (Minus, x1, x2, anno), Binary_operator (Minus, y1, y2, anno), anno)
            | Times -> Coord (Binary_operator (Times, x1, x2, anno), Binary_operator (Times, y1, y2, anno), anno)
            | Div -> Coord (Binary_operator (Div, x1, x2, anno), Binary_operator (Div, y1, y2, anno), anno)
            | Rem -> Coord (Binary_operator (Rem, x1, x2, anno), Binary_operator (Rem, y1, y2, anno), anno)
            | Equal -> Const_bool (x1 = x2 && y1 = y2, anno)
            | Diff -> Const_bool (x1 != x2 || y1 != y2, anno)
            | _ -> Binary_operator (op, simplify_expression e1, simplify_expression e2, anno))
        
        | (Color (r1, g1, b1, _), Color (r2, g2, b2, _)) ->
            (match op with
            | Plus -> Color (Binary_operator (Plus, r1, r2, anno), Binary_operator (Plus, g1, g2, anno), Binary_operator (Plus, b1, b2, anno), anno)
            | Minus -> Color (Binary_operator (Minus, r1, r2, anno), Binary_operator (Minus, g1, g2, anno), Binary_operator (Minus, b1, b2, anno), anno)
            | Times -> Color (Binary_operator (Times, r1, r2, anno), Binary_operator (Times, g1, g2, anno), Binary_operator (Times, b1, b2, anno), anno)
            | Div -> Color (Binary_operator (Div, r1, r2, anno), Binary_operator (Div, g1, g2, anno), Binary_operator (Div, b1, b2, anno), anno)
            | Rem -> Color (Binary_operator (Rem, r1, r2, anno), Binary_operator (Rem, g1, g2, anno), Binary_operator (Rem, b1, b2, anno), anno)
            | Equal -> Const_bool (r1 = r2 && g1 = g2 && b1 = b2, anno)
            | Diff -> Const_bool (r1 != r2 || g1 != g2 || b1 != b2, anno)
            | _ -> Binary_operator (op, simplify_expression e1, simplify_expression e2, anno))
        
        | (Pixel (p1, c1, _), Pixel (p2, c2, _)) ->
            (match op with
            | Plus -> Pixel (Binary_operator (Plus, p1, p2, anno), Binary_operator (Plus, c1, c2, anno), anno)
            | Minus -> Pixel (Binary_operator (Minus, p1, p2, anno), Binary_operator (Minus, c1, c2, anno), anno)
            | Times -> Pixel (Binary_operator (Times, p1, p2, anno), Binary_operator (Times, c1, c2, anno), anno)
            | Div -> Pixel (Binary_operator (Div, p1, p2, anno), Binary_operator (Div, c1, c2, anno), anno)
            | Rem -> Pixel (Binary_operator (Rem, p1, p2, anno), Binary_operator (Rem, c1, c2, anno), anno)
            | Equal -> Const_bool (p1 = p2 && c1 = c2, anno)
            | Diff -> Const_bool (p1 != p2 || c1 != c2, anno)
            | _ -> Binary_operator (op, simplify_expression e1, simplify_expression e2, anno))

        | (List(l1, _), List(l2, _)) ->
            (match op with
            | Plus -> simplify_expression (List (l1 @ l2, anno))
            | Equal -> Const_bool (l1 = l2, anno)
            | Diff -> Const_bool (l1 != l2, anno)
            | _ -> Binary_operator (op, simplify_expression e1, simplify_expression e2, anno))
            
        | (Const_int (i, _), (Coord (x, y, _))) ->
            let b1 = Binary_operator (op, Const_int (i, anno), x, anno) in
            let b2 = Binary_operator (op, Const_int (i, anno), y, anno) in
            simplify_expression (Coord (b1, b2, anno))
        
        | ((Coord (x, y, _)), Const_int (i, _)) ->
            let b1 = Binary_operator (op, x, Const_int (i, anno), anno) in
            let b2 = Binary_operator (op, y, Const_int (i, anno), anno) in
            simplify_expression (Coord (b1, b2, anno))
        
        | (Const_int (i, _), Color (r, g, b, _)) ->
            let b1 = Binary_operator (op, Const_int (i, anno), r, anno) in
            let b2 = Binary_operator (op, Const_int (i, anno), g, anno) in
            let b3 = Binary_operator (op, Const_int (i, anno), b, anno) in
            simplify_expression (Color (b1, b2, b3, anno))

        | ((Color (r, g, b, _) ), Const_int (i, _)) ->
            let b1 = Binary_operator (op, r, Const_int (i, anno), anno) in
            let b2 = Binary_operator (op, g, Const_int (i, anno), anno) in
            let b3 = Binary_operator (op, b, Const_int (i, anno), anno) in
            simplify_expression (Color (b1, b2, b3, anno))
        
        | _ -> (match (get_type_of_expression e1, get_type_of_expression e2) with
            | Type_int, Type_real ->
                let new_annotation = Ast.Annotation.create (Lexing.dummy_pos, Lexing.dummy_pos) in
                Ast.Annotation.set_type new_annotation (get_type_of_expression expr);
                Binary_operator (op, Unary_operator (Real_of_int, simplify_expression e1, new_annotation), simplify_expression e2, anno)
            | Type_real, Type_int ->
                let new_annotation = Ast.Annotation.create (Lexing.dummy_pos, Lexing.dummy_pos) in
                Ast.Annotation.set_type new_annotation (get_type_of_expression expr);
                Binary_operator (op, simplify_expression e1, Unary_operator (Real_of_int, simplify_expression e2, new_annotation), anno)
            | _ -> Binary_operator (op, simplify_expression e1, simplify_expression e2, anno))
        )

    | Unary_operator (op, e, anno) ->
        (match simplify_expression e with
        | Const_int (n, _) -> 
            (match op with 
            | Opposite -> Const_int ((-n), anno) 
            | Real_of_int -> Const_real (float_of_int n, anno)
            | _ -> Unary_operator (op, simplify_expression e, anno))
        | Const_real (n, _) -> 
            (match op with 
            | Opposite -> Const_real ((-.n), anno) 
            | Floor -> Const_int (int_of_float (floor n), anno)
            | Cos -> Const_real (cos n, anno)
            | Sin -> Const_real (sin n, anno)
            | _ -> Unary_operator (op, simplify_expression e, anno))
        | Const_bool (b, _) -> 
            (match op with 
            |Not -> Const_bool (not b, anno) 
            | _ -> Unary_operator (op, simplify_expression e, anno))
        | Unary_operator (op2, e2, _) -> 
            (match (op, op2) with
            | (Floor, Real_of_int) -> simplify_expression e2
            | _ -> Unary_operator (op, simplify_expression e, anno))
        | _ -> Unary_operator (op, simplify_expression e, anno))

    | Field_accessor (field, e, anno) ->
        (match simplify_expression e with
        | Pixel (p, c, _) ->
            (match field with 
            | Coord_field -> p 
            | Color_field -> c
            | _ -> Field_accessor (field, simplify_expression e, anno))
        | Coord (x, y, _) -> 
            (match field with 
            | X_field -> x 
            | Y_field -> y 
            | _ -> Field_accessor (field, simplify_expression e, anno))
        | Color (r, g, b, _) -> 
            (match field with 
            | Red_field -> r 
            | Green_field -> g 
            | Blue_field -> b 
            | _ -> Field_accessor (field, simplify_expression e, anno))
        | _ -> Field_accessor (field, simplify_expression e, anno))
        
    | List (elems, anno) ->
        let simp_elems = List.map simplify_expression elems in
        List (simp_elems, anno)

    | Append (e, l, anno) -> (match simplify_expression l with
        | List (elems, _) -> List ( (simplify_expression e) :: elems, anno)
        | _ -> Append (simplify_expression e, simplify_expression l, anno))

let rec simplify_statement state =
    match state with
    
    | Affectation (e1, e2, anno) ->
        Affectation (simplify_expression e1, simplify_expression e2, anno)

    | Declaration _ -> state

    | Block (l, anno) -> Block (List.map simplify_statement l, anno)

    | IfThenElse (test, th, el, anno) ->
        (match simplify_expression test with
        | Const_bool (true, _) -> simplify_statement th
        | Const_bool (false, _) -> simplify_statement el
        | _ -> IfThenElse (simplify_expression test, simplify_statement th, simplify_statement el, anno))
        
    | For (id, e1, e2, e3, body, anno) ->
        (match (simplify_expression e1, simplify_expression e2, simplify_expression e3) with
        | (Const_int (i1, _), Const_int (i2, _), Const_int (_, _)) ->
            if i1 > i2 then Block ([], anno)
            else For (id, Const_int (i1, anno), Const_int (i2, anno), simplify_expression e3, simplify_statement body, anno)
        | Const_real (r1, _), Const_real (r2, _), Const_real (_, _) ->
            if r1 > r2 then Block ([], anno)
            else For (id, Const_real (r1, anno), Const_real (r2, anno), simplify_expression e3, simplify_statement body, anno)
        | _ -> For (id, simplify_expression e1, simplify_expression e2, simplify_expression e3, simplify_statement body, anno))

    | Foreach (id, test, body, anno) ->
        (match simplify_expression test with
        | List ([], _) -> Block ([], anno)
        | _ -> Foreach (id, simplify_expression test, simplify_statement body, anno))

    | While (test, body, anno) ->
        (match simplify_expression test with
        | Const_bool (false, _) -> Block ([], anno)
        | _ -> While (simplify_expression test, simplify_statement body, anno))
    
    | Draw_pixel (e, anno) ->
        Draw_pixel (simplify_expression e, anno)
    
    | Nop -> Nop

    | Print (e, anno) ->
        Print (simplify_expression e, anno)


let simplifier (program : Ast.program) = (match program with
    | Program (decls, body) -> Program (decls, simplify_statement body))

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

let simplifier (program : Ast.program) = program

open Ast

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

        | (Const_int (i, _), (Coord (x, y, _))) | (Coord (x, y, _), Const_int (i, _)) ->
            let b1 = Binary_operator (op, Const_int (i, anno), x, anno) in
            let b2 = Binary_operator (op, Const_int (i, anno), y, anno) in
            simplify_expression (Coord (b1, b2, anno))

        | (Const_int (i, _), Color (r, g, b, _)) | ((Color (r, g, b, _) ), Const_int (i, _)) ->
            let b1 = Binary_operator (op, Const_int (i, anno), r, anno) in
            let b2 = Binary_operator (op, Const_int (i, anno), g, anno) in
            let b3 = Binary_operator (op, Const_int (i, anno), b, anno) in
            simplify_expression (Color (b1, b2, b3, anno))
            
        | _ -> Binary_operator (op, simplify_expression e1, simplify_expression e2, anno))

    | Unary_operator (op, e, anno) ->
        (match simplify_expression e with
        | Const_int (n, _) -> 
            (match op with 
            | Opposite -> Const_int ((-n), anno) 
            | _ -> Unary_operator (op, simplify_expression e, anno))
        | Const_real (n, _) -> 
            (match op with 
            |Opposite -> Const_real ((-.n), anno) 
            | _ -> Unary_operator (op, simplify_expression e, anno))
        | Const_bool (b, _) -> 
            (match op with 
            |Not -> Const_bool (not b, anno) 
            | _ -> Unary_operator (op, simplify_expression e, anno))

        | _ -> Unary_operator (op, simplify_expression e, anno))

    | Field_accessor (field, e, anno) ->
        (match simplify_expression e with
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

    | Append (e, l, anno) ->
        (match (simplify_expression e, simplify_expression l) with
        | (List (elems, _), List (elems2, _)) -> List (elems @ elems2, anno)
        | (List _, _) -> simplify_expression l
        | _ -> Append (simplify_expression e, simplify_expression l, anno))
  


let rec simplify_statement state =
    match state with

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
        | _ -> For (id, simplify_expression e1, simplify_expression e2, simplify_expression e3, simplify_statement body, anno))

    | Foreach (id, test, body, anno) ->
        (match simplify_expression test with
        | List ([], _) -> Block ([], anno)
        | _ -> Foreach (id, simplify_expression test, simplify_statement body, anno))

    | Affectation (e1, e2, anno) ->
        Affectation (simplify_expression e1, simplify_expression e2, anno)

    | Draw_pixel (e, anno) ->
        Draw_pixel (simplify_expression e, anno)

    | Print (e, anno) ->
        Print (simplify_expression e, anno)

    | _ -> state
  
